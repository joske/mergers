#[allow(clippy::wildcard_imports)]
use super::*;
use crate::patch::{apply_hunks, parse_patch};

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

    // Create temp directory with a unique name
    let tmp_dir = std::env::temp_dir().join(format!("mergers-patch-{}", std::process::id()));
    if let Err(e) = fs::create_dir_all(&tmp_dir) {
        show_patch_error(app, &format!("Cannot create temp directory: {e}"));
        return;
    }

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
        build_multi_file_patch(
            app,
            &base,
            &file_patches,
            &patch_path,
            &tmp_dir,
            labels,
            settings,
        );
    } else {
        show_patch_error(
            app,
            &format!("Base path does not exist: {}", base.display()),
        );
        let _ = fs::remove_dir_all(&tmp_dir);
    }
}

fn show_patch_error(app: &Application, message: &str) {
    // Create a temporary window to host the error dialog, then show it.
    let window = ApplicationWindow::builder()
        .application(app)
        .title("mergers")
        .default_width(400)
        .default_height(100)
        .build();
    window.present();
    show_error_dialog(&window, message);
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

    let fp = &file_patches[0];
    let patched = match apply_hunks(&original, &fp.hunks) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Warning: failed to apply patch: {e}");
            original.clone()
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

    // Clean up temp dir on destroy
    let tmp_dir_owned = tmp_dir.to_path_buf();
    window.connect_destroy(move |_| {
        let _ = fs::remove_dir_all(&tmp_dir_owned);
    });

    window.present();

    // Focus the left text view
    let ltv = dv.left_text_view.clone();
    gtk4::glib::idle_add_local_once(move || {
        ltv.grab_focus();
    });
}

#[allow(clippy::too_many_arguments)]
fn build_multi_file_patch(
    app: &Application,
    base: &Path,
    file_patches: &[crate::patch::FilePatch],
    patch_path: &Path,
    tmp_dir: &Path,
    labels: &[String],
    settings: &Rc<RefCell<Settings>>,
) {
    let patch_filename = patch_path
        .file_name()
        .map_or_else(|| "patch".to_string(), |n| n.to_string_lossy().into_owned());
    let title = format!("mergers \u{2014} {patch_filename}");

    let AppWindow {
        window,
        notebook,
        open_tabs,
    } = build_app_window(app, settings, 900, 600, false);
    window.set_title(Some(&title));

    let mut has_tabs = false;

    for fp in file_patches {
        let rel_path = &fp.original_path;
        let orig_path = base.join(rel_path);

        let original = match fs::read_to_string(&orig_path) {
            Ok(c) => c,
            Err(e) => {
                eprintln!("Warning: cannot read {}: {e}", orig_path.display());
                continue;
            }
        };

        let patched = match apply_hunks(&original, &fp.hunks) {
            Ok(p) => p,
            Err(e) => {
                eprintln!("Warning: failed to apply patch to {rel_path}: {e}");
                original.clone()
            }
        };

        // Write patched content — preserve subdirectory structure in temp dir
        let tmp_path = tmp_dir.join(rel_path);
        if let Some(parent) = tmp_path.parent()
            && let Err(e) = fs::create_dir_all(parent)
        {
            eprintln!("Warning: cannot create temp dir for {rel_path}: {e}");
            continue;
        }
        if let Err(e) = fs::write(&tmp_path, &patched) {
            eprintln!("Warning: cannot write temp file for {rel_path}: {e}");
            continue;
        }

        let left_label = labels.first().cloned().unwrap_or_else(|| rel_path.clone());
        let right_label = labels
            .get(1)
            .cloned()
            .unwrap_or_else(|| format!("{rel_path} (patched)"));
        let diff_labels = vec![left_label, right_label];

        let dv = build_diff_view(&orig_path, &tmp_path, &diff_labels, settings);

        // Right pane is read-only (patched output)
        dv.right_save.set_sensitive(false);
        dv.right_save.set_visible(false);

        dv.widget
            .insert_action_group("diff", Some(&dv.action_group));

        let tab_title = rel_path.clone();
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

        has_tabs = true;
    }

    if !has_tabs {
        show_error_dialog(
            &window,
            "No files from the patch could be applied to the base directory",
        );
        let _ = fs::remove_dir_all(tmp_dir);
        window.close();
        return;
    }

    // Clean up temp dir on destroy
    let tmp_dir_owned = tmp_dir.to_path_buf();
    window.connect_destroy(move |_| {
        let _ = fs::remove_dir_all(&tmp_dir_owned);
    });

    window.present();
}
