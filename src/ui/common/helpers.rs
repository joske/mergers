#[allow(clippy::wildcard_imports)]
use super::*;

/// Returns `true` when `path` is the empty sentinel used for blank comparisons.
pub fn is_blank_path(p: &Path) -> bool {
    p.as_os_str().is_empty()
}

/// Read a file as text, returning content and whether it was binary.
/// Binary files return an empty string and `true`.
/// Only reads the file once.
pub fn read_file_content(path: &Path) -> (String, bool) {
    let Ok(bytes) = fs::read(path) else {
        return (String::new(), false);
    };
    if bytes.iter().take(8192).any(|&b| b == 0) {
        (String::new(), true)
    } else {
        (String::from_utf8_lossy(&bytes).into_owned(), false)
    }
}

/// Read a file for reload: returns `None` if binary or on read error (to avoid corrupting buffers).
/// Only reads the file once.
pub fn read_file_for_reload(path: &Path) -> Option<String> {
    let bytes = fs::read(path).ok()?;
    if bytes.iter().take(8192).any(|&b| b == 0) {
        return None;
    }
    Some(String::from_utf8_lossy(&bytes).into_owned())
}

/// Write file content with error handling. On failure, shows an error dialog and
/// leaves the save button sensitive (preserving unsaved state).
pub fn save_file(path: &Path, content: &str, save_btn: &Button) {
    match fs::write(path, content) {
        Ok(()) => {
            mark_saving(path);
            save_btn.set_sensitive(false);
        }
        Err(e) => {
            if let Some(win) = save_btn
                .root()
                .and_then(|r| r.downcast::<ApplicationWindow>().ok())
            {
                show_error_dialog(&win, &format!("Failed to save {}: {e}", path.display()));
            }
        }
    }
}

/// Open a file with the system's default application.
pub fn open_externally(path: &Path) {
    let path = path.to_path_buf();
    std::thread::spawn(move || {
        #[cfg(target_os = "macos")]
        let result = std::process::Command::new("open").arg(&path).status();
        #[cfg(target_os = "windows")]
        let result = std::process::Command::new("cmd")
            .args(["/C", "start", ""])
            .arg(&path)
            .status();
        #[cfg(not(any(target_os = "macos", target_os = "windows")))]
        let result = std::process::Command::new("xdg-open").arg(&path).status();
        match result {
            Ok(status) if !status.success() => {
                eprintln!(
                    "Failed to open {}: external opener exited with status {:?}",
                    path.display(),
                    status.code()
                );
            }
            Err(e) => {
                eprintln!("Failed to open {}: {e}", path.display());
            }
            _ => {}
        }
    });
}

/// Show a modal error dialog with a single OK button.
pub fn show_error_dialog(parent: &ApplicationWindow, message: &str) {
    let dialog = gtk4::Window::builder()
        .modal(true)
        .transient_for(parent)
        .resizable(false)
        .decorated(true)
        .deletable(true)
        .title("Error")
        .build();

    let content = GtkBox::new(Orientation::Vertical, 8);
    content.set_margin_top(18);
    content.set_margin_bottom(18);
    content.set_margin_start(18);
    content.set_margin_end(18);

    let label = Label::new(Some(message));
    label.set_wrap(true);
    label.set_max_width_chars(60);
    content.append(&label);

    let ok_btn = Button::with_label("OK");
    ok_btn.add_css_class("suggested-action");
    ok_btn.set_halign(gtk4::Align::Center);
    let d = dialog.clone();
    ok_btn.connect_clicked(move |_| d.close());
    content.append(&ok_btn);

    dialog.set_child(Some(&content));
    dialog.present();
}

/// Show a modal confirmation dialog with Cancel and a destructive action button.
/// Calls `on_confirm` when the action button is clicked.
pub fn show_confirm_dialog(
    parent: &ApplicationWindow,
    title: &str,
    message: &str,
    action_label: &str,
    on_confirm: impl Fn() + 'static,
) {
    let dialog = gtk4::Window::builder()
        .modal(true)
        .transient_for(parent)
        .resizable(false)
        .decorated(true)
        .deletable(true)
        .build();

    let content = GtkBox::new(Orientation::Vertical, 8);
    content.set_margin_top(18);
    content.set_margin_bottom(18);
    content.set_margin_start(18);
    content.set_margin_end(18);

    let title_label = Label::new(Some(title));
    title_label.add_css_class("title-3");
    content.append(&title_label);

    let msg_label = Label::new(Some(message));
    msg_label.set_wrap(true);
    msg_label.set_max_width_chars(60);
    content.append(&msg_label);

    let btn_box = GtkBox::new(Orientation::Horizontal, 8);
    btn_box.set_margin_top(8);
    btn_box.set_halign(gtk4::Align::End);

    let cancel_btn = Button::with_label("Cancel");
    let action_btn = Button::with_label(action_label);
    action_btn.add_css_class("destructive-action");

    btn_box.append(&cancel_btn);
    btn_box.append(&action_btn);
    content.append(&btn_box);

    dialog.set_child(Some(&content));

    {
        let d = dialog.clone();
        cancel_btn.connect_clicked(move |_| d.close());
    }
    {
        let d = dialog.clone();
        action_btn.connect_clicked(move |_| {
            on_confirm();
            d.close();
        });
    }

    // Allow Escape to close the dialog
    {
        let d = dialog.clone();
        let key_ctl = EventControllerKey::new();
        key_ctl.connect_key_pressed(move |_, key, _, _| {
            if key == gtk4::gdk::Key::Escape {
                d.close();
                return gtk4::glib::Propagation::Stop;
            }
            gtk4::glib::Propagation::Proceed
        });
        dialog.add_controller(key_ctl);
    }

    dialog.present();
}

/// Pre-filter text for diff comparison.
/// Returns `(filtered_text, line_map)` where `line_map[filtered_idx] = original_idx`.
/// - `ignore_whitespace`: collapse each line's whitespace to single spaces.
/// - `ignore_blanks`: remove blank lines (the line map tracks where they were).
pub fn filter_for_diff(
    text: &str,
    ignore_whitespace: bool,
    ignore_blanks: bool,
) -> (String, Vec<usize>) {
    let lines: Vec<&str> = text.lines().collect();
    let mut filtered = Vec::with_capacity(lines.len());
    let mut line_map = Vec::with_capacity(lines.len());

    for (i, line) in lines.iter().enumerate() {
        if ignore_blanks && line.trim().is_empty() {
            continue;
        }
        if ignore_whitespace {
            filtered.push(line.split_whitespace().collect::<Vec<_>>().join(" "));
        } else {
            filtered.push((*line).to_string());
        }
        line_map.push(i);
    }

    (filtered.join("\n"), line_map)
}

/// Remap diff chunks from filtered line numbers back to original line numbers.
pub fn remap_chunks(
    chunks: Vec<DiffChunk>,
    left_map: &[usize],
    left_total: usize,
    right_map: &[usize],
    right_total: usize,
) -> Vec<DiffChunk> {
    chunks
        .into_iter()
        .map(|mut chunk| {
            chunk.start_a = left_map.get(chunk.start_a).copied().unwrap_or(left_total);
            chunk.end_a = left_map.get(chunk.end_a).copied().unwrap_or(left_total);
            chunk.start_b = right_map.get(chunk.start_b).copied().unwrap_or(right_total);
            chunk.end_b = right_map.get(chunk.end_b).copied().unwrap_or(right_total);
            chunk
        })
        .collect()
}

pub fn format_size(bytes: u64) -> String {
    if bytes < 1000 {
        format!("{bytes} B")
    } else if bytes < 1_000_000 {
        format!("{:.1} kB", bytes as f64 / 1000.0)
    } else if bytes < 1_000_000_000 {
        format!("{:.1} MB", bytes as f64 / 1_000_000.0)
    } else {
        format!("{:.1} GB", bytes as f64 / 1_000_000_000.0)
    }
}

pub fn format_mtime(t: SystemTime) -> String {
    let dt: DateTime<Local> = t.into();
    dt.format("%Y-%m-%d %H:%M:%S").to_string()
}

/// Generate a unified diff (patch) string from chunks and source texts.
pub fn generate_unified_diff(
    left_label: &str,
    right_label: &str,
    left_text: &str,
    right_text: &str,
    chunks: &[DiffChunk],
) -> String {
    let left_lines: Vec<&str> = left_text.lines().collect();
    let right_lines: Vec<&str> = right_text.lines().collect();
    let mut out = String::new();
    let _ = writeln!(out, "--- {left_label}");
    let _ = writeln!(out, "+++ {right_label}");

    // Group non-Equal chunks into hunks with 3 lines of context
    let context = 3_usize;
    let changes: Vec<&DiffChunk> = chunks.iter().filter(|c| c.tag != DiffTag::Equal).collect();
    if changes.is_empty() {
        return out;
    }

    // Build hunks: merge nearby changes
    let mut hunks: Vec<(usize, usize, usize, usize, Vec<&DiffChunk>)> = Vec::new();
    for &ch in &changes {
        let ctx_start_a = ch.start_a.saturating_sub(context);
        let ctx_start_b = ch.start_b.saturating_sub(context);
        let ctx_end_a = (ch.end_a + context).min(left_lines.len());
        let ctx_end_b = (ch.end_b + context).min(right_lines.len());

        if let Some(last) = hunks.last_mut() {
            // Merge if overlapping
            if ctx_start_a <= last.1 {
                last.1 = ctx_end_a;
                last.3 = ctx_end_b;
                last.4.push(ch);
                continue;
            }
        }
        hunks.push((ctx_start_a, ctx_end_a, ctx_start_b, ctx_end_b, vec![ch]));
    }

    for (hunk_start_a, hunk_end_a, hunk_start_b, hunk_end_b, hunk_chunks) in &hunks {
        let count_a = hunk_end_a - hunk_start_a;
        let count_b = hunk_end_b - hunk_start_b;
        let _ = writeln!(
            out,
            "@@ -{},{count_a} +{},{count_b} @@",
            hunk_start_a + 1,
            hunk_start_b + 1
        );

        let mut pos_a = *hunk_start_a;
        for ch in hunk_chunks {
            // Context lines before this change
            while pos_a < ch.start_a {
                if let Some(line) = left_lines.get(pos_a) {
                    let _ = writeln!(out, " {line}");
                }
                pos_a += 1;
            }
            // Removed lines
            for i in ch.start_a..ch.end_a {
                if let Some(line) = left_lines.get(i) {
                    let _ = writeln!(out, "-{line}");
                }
            }
            // Added lines
            for i in ch.start_b..ch.end_b {
                if let Some(line) = right_lines.get(i) {
                    let _ = writeln!(out, "+{line}");
                }
            }
            pos_a = ch.end_a;
        }
        // Trailing context
        while pos_a < *hunk_end_a {
            if let Some(line) = left_lines.get(pos_a) {
                let _ = writeln!(out, " {line}");
            }
            pos_a += 1;
        }
    }
    out
}

/// Compute the row index at a given y-coordinate in a `ColumnView`.
/// Uses `pick()` to find the widget under the cursor, then walks
/// up to the row widget and counts siblings to determine position.
pub fn column_view_row_at_y(view: &ColumnView, x: f64, y: f64, n_items: u32) -> Option<u32> {
    if n_items == 0 {
        return None;
    }
    // Pick the widget at the click point
    let picked = view.pick(x, y, gtk4::PickFlags::DEFAULT)?;
    // Walk up from the picked widget until we find one whose parent
    // is a direct child of the ColumnView (the list area container).
    // Row widgets are children of that container.
    let mut widget = picked;
    loop {
        let parent = widget.parent()?;
        let grandparent = parent.parent()?;
        if grandparent == *view.upcast_ref::<gtk4::Widget>() {
            // `parent` is a direct child of the ColumnView (the list container),
            // and `widget` is a row inside it. Count preceding siblings.
            let mut pos = 0u32;
            let mut sibling = parent.first_child();
            while let Some(s) = sibling {
                if s == widget {
                    return if pos < n_items { Some(pos) } else { None };
                }
                pos += 1;
                sibling = s.next_sibling();
            }
            return None;
        }
        widget = parent;
    }
}

pub fn make_info_bar(message: &str) -> GtkBox {
    let bar = GtkBox::new(Orientation::Horizontal, 8);
    bar.add_css_class("info-bar");
    let icon = Image::from_icon_name("dialog-information-symbolic");
    let label = Label::new(Some(message));
    label.set_hexpand(true);
    label.set_halign(gtk4::Align::Start);
    let hide_btn = Button::with_label("Hide");
    hide_btn.add_css_class("raised");
    bar.append(&icon);
    bar.append(&label);
    bar.append(&hide_btn);
    let bar_ref = bar.clone();
    hide_btn.connect_clicked(move |_| bar_ref.set_visible(false));
    bar
}

pub fn shortened_path(full: &Path) -> String {
    let components: Vec<_> = full.components().collect();
    if components.len() <= 2 {
        return full.display().to_string();
    }
    let tail: std::path::PathBuf = components[components.len() - 2..].iter().collect();
    format!("\u{2026}/{}", tail.display())
}
