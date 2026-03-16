#[allow(clippy::wildcard_imports)]
use super::*;
use crate::patch::{apply_hunks, apply_hunks_best_effort, parse_patch, sanitize_patch_path};

use std::sync::Mutex;

/// Temp dirs registered for cleanup on exit / signal.
static PATCH_TEMP_DIRS: Mutex<Vec<PathBuf>> = Mutex::new(Vec::new());

/// Remove all registered patch temp dirs. Called from shutdown and signal handlers.
pub fn cleanup_patch_temp_dirs() {
    if let Ok(mut dirs) = PATCH_TEMP_DIRS.lock() {
        for dir in dirs.iter() {
            let _ = fs::remove_dir_all(dir);
        }
        dirs.clear();
    }
}

// ─── Patch file viewing window ─────────────────────────────────────────────

pub(super) fn build_patch_window(
    app: &Application,
    base: PathBuf,
    patch_path: PathBuf,
    labels: &[String],
    settings: &Rc<RefCell<Settings>>,
) {
    // Read patch file
    let patch_content = match fs::read_to_string(&patch_path) {
        Ok(c) => c,
        Err(e) => {
            show_patch_error(app, &format!("Cannot read patch file: {e}"));
            return;
        }
    };

    // Parse patch
    let file_patches = match parse_patch(&patch_content) {
        Ok(fp) => fp,
        Err(e) => {
            show_patch_error(app, &format!("Cannot parse patch: {e}"));
            return;
        }
    };

    if file_patches.is_empty() {
        show_patch_error(app, "Patch file contains no file entries");
        return;
    }

    // Create temp directory with secure permissions (0o700, collision-resistant)
    let tmp_dir = match tempfile::tempdir() {
        Ok(d) => d.keep(),
        Err(e) => {
            show_patch_error(app, &format!("Cannot create temp directory: {e}"));
            return;
        }
    };

    if base.is_file() {
        build_single_file_patch(
            app,
            &base,
            &file_patches,
            &patch_path,
            &tmp_dir,
            labels,
            settings,
        );
    } else if base.is_dir() {
        build_multi_file_patch(app, &base, &file_patches, &tmp_dir, labels, settings);
    } else {
        show_patch_error(
            app,
            &format!("Base path does not exist: {}", base.display()),
        );
        let _ = fs::remove_dir_all(&tmp_dir);
    }
}

fn show_patch_error(app: &Application, message: &str) {
    if let Some(win) = app.active_window().and_downcast::<ApplicationWindow>() {
        show_error_dialog(&win, message);
    } else {
        // No window yet — create a transient one that closes after the dialog
        let window = ApplicationWindow::builder()
            .application(app)
            .title("mergers")
            .default_width(400)
            .default_height(100)
            .build();
        window.present();
        show_error_dialog(&window, message);
        window.close();
    }
}

fn build_single_file_patch(
    app: &Application,
    base: &Path,
    file_patches: &[crate::patch::FilePatch],
    _patch_path: &Path,
    tmp_dir: &Path,
    labels: &[String],
    settings: &Rc<RefCell<Settings>>,
) {
    let original = match fs::read_to_string(base) {
        Ok(c) => c,
        Err(e) => {
            show_patch_error(app, &format!("Cannot read base file: {e}"));
            let _ = fs::remove_dir_all(tmp_dir);
            return;
        }
    };

    // Find the patch entry that matches the base filename. Reject ambiguous
    // matches (multiple entries with the same basename) to avoid silently
    // applying the wrong file's hunks.
    let base_name = base
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_default();
    let fp = if file_patches.len() == 1 {
        &file_patches[0]
    } else {
        let matches: Vec<_> = file_patches
            .iter()
            .filter(|fp| {
                std::path::Path::new(&fp.original_path)
                    .file_name()
                    .is_some_and(|n| n.to_string_lossy() == base_name)
            })
            .collect();
        if matches.len() == 1 {
            matches[0]
        } else {
            let msg = if matches.is_empty() {
                format!(
                    "Patch does not contain an entry for '{base_name}'. \
                     Use directory mode for multi-file patches.",
                )
            } else {
                format!(
                    "Patch contains {} entries matching '{base_name}'. \
                     Use directory mode for multi-file patches.",
                    matches.len()
                )
            };
            show_patch_error(app, &msg);
            let _ = fs::remove_dir_all(tmp_dir);
            return;
        }
    };
    let patched = match apply_hunks(&original, &fp.hunks) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Warning: failed to apply patch cleanly: {e}");
            apply_hunks_best_effort(&original, &fp.hunks)
        }
    };

    // Write patched content to temp file
    let base_filename = base.file_name().map_or_else(
        || "patched".to_string(),
        |n| n.to_string_lossy().into_owned(),
    );
    let tmp_path = tmp_dir.join(&base_filename);
    if let Err(e) = fs::write(&tmp_path, &patched) {
        show_patch_error(app, &format!("Cannot write temp file: {e}"));
        let _ = fs::remove_dir_all(tmp_dir);
        return;
    }

    // Labels
    let left_label = labels
        .first()
        .cloned()
        .unwrap_or_else(|| base_filename.clone());
    let right_label = labels
        .get(1)
        .cloned()
        .unwrap_or_else(|| format!("{base_filename} (patched)"));
    let diff_labels = vec![left_label, right_label];

    let dv = build_diff_view(base, &tmp_path, &diff_labels, settings);

    // Right pane is read-only (patched output)
    dv.right_save.set_sensitive(false);
    dv.right_save.set_visible(false);

    // Window title
    let title = format!("mergers \u{2014} {base_filename} (patch)");

    let AppWindow {
        window,
        notebook,
        open_tabs,
    } = build_app_window(app, settings, 900, 600, false);
    window.set_title(Some(&title));

    dv.widget
        .insert_action_group("diff", Some(&dv.action_group));

    let tab_title = format!("{base_filename} (patch)");
    notebook.append_page(&dv.widget, Some(&Label::new(Some(&tab_title))));

    // Register tab
    {
        let tab_id = NEXT_TAB_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        open_tabs.borrow_mut().push(FileTab::Diff {
            id: tab_id,
            rel_path: tab_title,
            widget: dv.widget.clone(),
            left: PaneInfo {
                path: dv.left_tab_path.clone(),
                buf: dv.left_buf.clone(),
                save: dv.left_save.clone(),
            },
            right: PaneInfo {
                path: dv.right_tab_path.clone(),
                buf: dv.right_buf.clone(),
                save: dv.right_save.clone(),
            },
        });
    }

    // Register for cleanup on shutdown / signal (only after successful creation)
    if let Ok(mut dirs) = PATCH_TEMP_DIRS.lock() {
        dirs.push(tmp_dir.to_path_buf());
    }

    // Clean up temp dir on destroy and remove from global registry
    let tmp_dir_owned = tmp_dir.to_path_buf();
    window.connect_destroy(move |_| {
        let _ = fs::remove_dir_all(&tmp_dir_owned);
        if let Ok(mut dirs) = PATCH_TEMP_DIRS.lock() {
            dirs.retain(|d| *d != tmp_dir_owned);
        }
    });

    window.present();

    // Focus the left text view
    let ltv = dv.left_text_view.clone();
    gtk4::glib::idle_add_local_once(move || {
        ltv.grab_focus();
    });
}

fn build_multi_file_patch(
    app: &Application,
    base: &Path,
    file_patches: &[crate::patch::FilePatch],
    tmp_dir: &Path,
    labels: &[String],
    settings: &Rc<RefCell<Settings>>,
) {
    use crate::patch::PatchKind;

    // Canonicalize base so symlinks use absolute paths
    let base = base.canonicalize().unwrap_or_else(|_| base.to_path_buf());

    // Two temp dirs: "left" has only the originals mentioned in the patch,
    // "right" has the patched versions. This avoids scanning the entire base tree.
    let left_dir = tmp_dir.join("left");
    let right_dir = tmp_dir.join("right");
    if let Err(e) = fs::create_dir(&left_dir) {
        show_patch_error(app, &format!("Cannot create temp directory: {e}"));
        let _ = fs::remove_dir_all(tmp_dir);
        return;
    }
    if let Err(e) = fs::create_dir(&right_dir) {
        show_patch_error(app, &format!("Cannot create temp directory: {e}"));
        let _ = fs::remove_dir_all(tmp_dir);
        return;
    }

    let mut conflict_paths: Vec<String> = Vec::new();

    for fp in file_patches {
        let Some(rel_path) = sanitize_patch_path(&fp.original_path) else {
            eprintln!(
                "Warning: skipping unsafe patch path: {:?}",
                fp.original_path
            );
            continue;
        };
        let left_path = left_dir.join(&rel_path);
        let right_path = right_dir.join(&rel_path);

        // Ensure parent dirs exist in the right (patched) tree.
        // Left tree uses symlinks, so only the right tree needs real dirs.
        if let Some(parent) = right_path.parent()
            && let Err(e) = fs::create_dir_all(parent)
        {
            eprintln!("Warning: cannot create temp dir for {rel_path}: {e}");
            continue;
        }

        match fp.kind {
            PatchKind::Deleted => {
                // Symlink original on left, nothing on right → shows as LeftOnly
                let orig_path = base.join(&rel_path);
                if orig_path.exists() {
                    if let Some(parent) = left_path.parent() {
                        fs::create_dir_all(parent).ok();
                    }
                    #[cfg(unix)]
                    std::os::unix::fs::symlink(&orig_path, &left_path).ok();
                    #[cfg(windows)]
                    fs::copy(&orig_path, &left_path).ok();
                } else {
                    // Base file missing — write error to both sides so the entry
                    // appears as a Modified/conflict row, not invisibly absent.
                    eprintln!("Warning: base file not found for deleted entry: {rel_path}");
                    let msg = format!("[Base file not found: {rel_path}]\n");
                    if let Some(parent) = left_path.parent() {
                        fs::create_dir_all(parent).ok();
                    }
                    fs::write(&left_path, &msg).ok();
                    if let Some(parent) = right_path.parent() {
                        fs::create_dir_all(parent).ok();
                    }
                    fs::write(&right_path, &msg).ok();
                    conflict_paths.push(rel_path.clone());
                }
            }
            PatchKind::Added => {
                // Nothing on left, patched on right → shows as RightOnly
                let patched = if let Ok(p) = apply_hunks("", &fp.hunks) {
                    p
                } else {
                    let p = apply_hunks_best_effort("", &fp.hunks);
                    if p.contains("<<<<<<< original") {
                        conflict_paths.push(rel_path.clone());
                    }
                    p
                };
                if let Err(e) = fs::write(&right_path, patched) {
                    eprintln!(
                        "Warning: failed to write patched file {}: {e}",
                        right_path.display()
                    );
                    conflict_paths.push(rel_path.clone());
                }
            }
            PatchKind::Modified => {
                let orig_path = base.join(&rel_path);
                let original = match fs::read_to_string(&orig_path) {
                    Ok(c) => c,
                    Err(e) => {
                        eprintln!("Warning: cannot read {}: {e}", orig_path.display());
                        // Write error message to both sides so the entry shows as
                        // Modified (not RightOnly), making the failure obvious.
                        let msg = format!("[Cannot read original file: {e}]\n");
                        if let Some(parent) = left_path.parent() {
                            fs::create_dir_all(parent).ok();
                        }
                        fs::write(&left_path, &msg).ok();
                        fs::write(&right_path, &msg).ok();
                        conflict_paths.push(rel_path.clone());
                        continue;
                    }
                };

                // Symlink original to left tree — saves write back through symlink
                if let Some(parent) = left_path.parent() {
                    fs::create_dir_all(parent).ok();
                }
                #[cfg(unix)]
                std::os::unix::fs::symlink(&orig_path, &left_path).ok();
                #[cfg(windows)]
                fs::copy(&orig_path, &left_path).ok();

                let patched = match apply_hunks(&original, &fp.hunks) {
                    Ok(p) => p,
                    Err(e) => {
                        eprintln!("Warning: failed to apply patch cleanly to {rel_path}: {e}");
                        conflict_paths.push(rel_path.clone());
                        apply_hunks_best_effort(&original, &fp.hunks)
                    }
                };

                if let Err(e) = fs::write(&right_path, patched) {
                    eprintln!(
                        "Warning: failed to write patched file {}: {e}",
                        right_path.display()
                    );
                    conflict_paths.push(rel_path.clone());
                }
            }
        }
    }

    // Write marker file so the dir scan can color conflicted files red
    if !conflict_paths.is_empty() {
        let marker = right_dir.join(".mergers-conflicts");
        fs::write(&marker, conflict_paths.join("\n")).ok();
    }

    // Provide sensible labels so the headers show the base dir, not temp paths
    let patch_labels = if labels.len() >= 2 {
        labels.to_vec()
    } else {
        let base_name = base.file_name().map_or_else(
            || base.display().to_string(),
            |n| n.to_string_lossy().into_owned(),
        );
        vec![
            labels.first().cloned().unwrap_or(base_name.clone()),
            labels
                .get(1)
                .cloned()
                .unwrap_or(format!("{base_name} (patched)")),
        ]
    };

    // Tooltip dirs show the original base path (not temp dirs).
    // Only the left side gets overridden — the right side is generated temp content.
    let tooltip_dirs = vec![base.display().to_string()];

    // Register for cleanup on shutdown / signal (only after successful creation)
    if let Ok(mut dirs) = PATCH_TEMP_DIRS.lock() {
        dirs.push(tmp_dir.to_path_buf());
    }

    // Clean up temp dir and unregister on window destroy
    let tmp_owned = tmp_dir.to_path_buf();
    let on_destroy: Box<dyn Fn() + 'static> = Box::new(move || {
        let _ = fs::remove_dir_all(&tmp_owned);
        if let Ok(mut dirs) = PATCH_TEMP_DIRS.lock() {
            dirs.retain(|d| *d != tmp_owned);
        }
    });

    // Delegate to the existing directory comparison window
    build_dir_window_with_tooltips(
        app,
        left_dir,
        right_dir,
        &patch_labels,
        &tooltip_dirs,
        Some(on_destroy),
        Rc::clone(settings),
    );
}
