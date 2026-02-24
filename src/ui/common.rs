#[allow(clippy::wildcard_imports)]
use super::*;

// ─── Data types ────────────────────────────────────────────────────────────

#[derive(Clone, Copy, PartialEq)]
pub(super) enum FileStatus {
    Same,
    Different,
    LeftOnly,
    RightOnly,
}

impl FileStatus {
    pub(super) fn code(self) -> &'static str {
        match self {
            Self::Same => "S",
            Self::Different => "D",
            Self::LeftOnly => "L",
            Self::RightOnly => "R",
        }
    }
}

pub(super) struct DirMeta {
    pub(super) size: Option<u64>,
    pub(super) mtime: Option<SystemTime>,
    pub(super) is_dir: bool,
}

pub(super) struct FileTab {
    pub(super) id: u64,
    pub(super) rel_path: String,
    pub(super) widget: GtkBox,
    pub(super) left_path: String,
    pub(super) right_path: String,
    pub(super) left_buf: TextBuffer,
    pub(super) right_buf: TextBuffer,
    pub(super) left_save: Button,
    pub(super) right_save: Button,
}

pub(super) static NEXT_TAB_ID: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

pub(super) fn mark_saving(path: &Path) {
    let p = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    SAVING_PATHS.with(|s| s.borrow_mut().insert(p.clone()));
    gtk4::glib::timeout_add_local_once(Duration::from_millis(600), move || {
        SAVING_PATHS.with(|s| s.borrow_mut().remove(&p));
    });
}

pub(super) fn is_saving(paths: &[&Path]) -> bool {
    SAVING_PATHS.with(|s| {
        let set = s.borrow();
        paths.iter().any(|p| {
            let canon = p.canonicalize().unwrap_or_else(|_| p.to_path_buf());
            set.contains(&canon)
        })
    })
}

pub(super) fn is_saving_under(roots: &[&Path]) -> bool {
    SAVING_PATHS.with(|s| {
        let set = s.borrow();
        let canon_roots: Vec<PathBuf> = roots
            .iter()
            .map(|r| r.canonicalize().unwrap_or_else(|_| r.to_path_buf()))
            .collect();
        set.iter()
            .any(|saving_path| canon_roots.iter().any(|root| saving_path.starts_with(root)))
    })
}

pub(super) fn apply_diff_tags(left_buf: &TextBuffer, right_buf: &TextBuffer, chunks: &[DiffChunk]) {
    for chunk in chunks {
        match chunk.tag {
            DiffTag::Equal => {}
            DiffTag::Replace => {
                apply_line_tag(left_buf, "changed", chunk.start_a, chunk.end_a);
                apply_line_tag(right_buf, "changed", chunk.start_b, chunk.end_b);
                apply_inline_tags(left_buf, right_buf, chunk);
            }
            DiffTag::Delete => {
                apply_line_tag(left_buf, "deleted", chunk.start_a, chunk.end_a);
            }
            DiffTag::Insert => {
                apply_line_tag(right_buf, "inserted", chunk.start_b, chunk.end_b);
            }
        }
    }
}

/// 3-way merge tagging: outer panes get only their own side's tags,
/// middle pane gets one-sided change highlights + conflict overlay.
pub(super) fn apply_merge_tags(
    left_buf: &TextBuffer,
    middle_buf: &TextBuffer,
    right_buf: &TextBuffer,
    left_chunks: &[DiffChunk],
    right_chunks: &[DiffChunk],
) {
    // Left pane: a-side of left diff only
    // left_chunks = diff(left, middle): a-side = left, b-side = middle
    for chunk in left_chunks {
        match chunk.tag {
            DiffTag::Equal | DiffTag::Insert => {}
            DiffTag::Replace => {
                apply_line_tag(left_buf, "changed", chunk.start_a, chunk.end_a);
                apply_inline_tags(left_buf, middle_buf, chunk);
            }
            DiffTag::Delete => apply_line_tag(left_buf, "deleted", chunk.start_a, chunk.end_a),
        }
    }

    // Right pane: b-side of right diff only
    // right_chunks = diff(middle, right): a-side = middle, b-side = right
    for chunk in right_chunks {
        match chunk.tag {
            DiffTag::Equal | DiffTag::Delete => {}
            DiffTag::Replace => {
                apply_line_tag(right_buf, "changed", chunk.start_b, chunk.end_b);
                apply_inline_tags(middle_buf, right_buf, chunk);
            }
            DiffTag::Insert => apply_line_tag(right_buf, "inserted", chunk.start_b, chunk.end_b),
        }
    }

    // Middle pane: b-side of left diff (lines in middle that differ from left)
    for chunk in left_chunks {
        match chunk.tag {
            DiffTag::Equal | DiffTag::Delete => {}
            DiffTag::Replace | DiffTag::Insert => {
                apply_line_tag(middle_buf, "changed", chunk.start_b, chunk.end_b);
            }
        }
    }
    // Middle pane: a-side of right diff (lines in middle that differ from right)
    for chunk in right_chunks {
        match chunk.tag {
            DiffTag::Equal | DiffTag::Insert => {}
            DiffTag::Replace | DiffTag::Delete => {
                apply_line_tag(middle_buf, "changed", chunk.start_a, chunk.end_a);
            }
        }
    }

    // Conflict overlay (higher priority tag, added last in setup_diff_tags)
    apply_conflict_tags(middle_buf, left_chunks, right_chunks);
}

/// Reload a file tab from disk. Returns `false` if a read failed (binary or
/// I/O error) so the caller can keep dirty and retry on the next tick.
pub(super) fn reload_file_tab(tab: &FileTab, left_dir: &str, right_dir: &str) -> bool {
    // Don't overwrite unsaved user edits
    if tab.left_save.is_sensitive() || tab.right_save.is_sensitive() {
        return true; // not a read failure — just skip
    }

    let left_content = read_file_for_reload(&Path::new(left_dir).join(&tab.rel_path));
    let right_content = read_file_for_reload(&Path::new(right_dir).join(&tab.rel_path));

    // Skip panes where read failed or file became binary
    let Some(left_content) = left_content else {
        return false;
    };
    let Some(right_content) = right_content else {
        return false;
    };

    // Only reset buffers if the on-disk content actually differs from what's in the buffer
    let cur_left = tab
        .left_buf
        .text(&tab.left_buf.start_iter(), &tab.left_buf.end_iter(), false);
    let cur_right = tab.right_buf.text(
        &tab.right_buf.start_iter(),
        &tab.right_buf.end_iter(),
        false,
    );

    if cur_left.as_str() == left_content && cur_right.as_str() == right_content {
        return true; // nothing changed on disk vs buffer
    }

    // set_text triggers connect_changed which schedules refresh_diff with
    // the current filter state (ignore_blanks / ignore_whitespace).
    // We must NOT run a separate diff here — it would race and potentially
    // overwrite the filtered result with an unfiltered one.
    tab.left_buf.set_text(&left_content);
    tab.right_buf.set_text(&right_content);
    // Buffer now matches disk — clear unsaved-changes indicator.
    // (set_text triggers the save-button connect_changed which sets
    // sensitive=true, so we reset it after.)
    tab.left_save.set_sensitive(false);
    tab.right_save.set_sensitive(false);
    true
}

// ─── Helpers ───────────────────────────────────────────────────────────────

/// Check if a file appears to be binary by looking for NUL bytes in the first 8KB.
pub(super) fn is_binary(path: &Path) -> bool {
    fs::read(path)
        .map(|bytes| bytes.iter().take(8192).any(|&b| b == 0))
        .unwrap_or(false)
}

/// Read a file with lossy UTF-8 conversion (replacement chars for invalid bytes).
pub(super) fn read_file_lossy(path: &Path) -> String {
    fs::read(path)
        .map(|b| String::from_utf8_lossy(&b).into_owned())
        .unwrap_or_default()
}

/// Read a file as text, returning content and whether it was binary.
/// Binary files return an empty string and `true`.
pub(super) fn read_file_content(path: &Path) -> (String, bool) {
    if is_binary(path) {
        (String::new(), true)
    } else {
        (read_file_lossy(path), false)
    }
}

/// Read a file for reload: returns `None` if binary or on read error (to avoid corrupting buffers).
pub(super) fn read_file_for_reload(path: &Path) -> Option<String> {
    if is_binary(path) {
        return None;
    }
    fs::read(path)
        .ok()
        .map(|b| String::from_utf8_lossy(&b).into_owned())
}

/// Write file content with error handling. On failure, shows an error dialog and
/// leaves the save button sensitive (preserving unsaved state).
pub(super) fn save_file(path: &Path, content: &str, save_btn: &Button) {
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

/// Show a modal error dialog with a single OK button.
pub(super) fn show_error_dialog(parent: &ApplicationWindow, message: &str) {
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
pub(super) fn show_confirm_dialog(
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

    dialog.present();
}

/// Pre-filter text for diff comparison.
/// Returns `(filtered_text, line_map)` where `line_map[filtered_idx] = original_idx`.
/// - `ignore_whitespace`: collapse each line's whitespace to single spaces.
/// - `ignore_blanks`: remove blank lines (the line map tracks where they were).
pub(super) fn filter_for_diff(
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
pub(super) fn remap_chunks(
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

pub(super) fn format_size(bytes: u64) -> String {
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

pub(super) fn format_mtime(t: SystemTime) -> String {
    let dt: DateTime<Local> = t.into();
    dt.format("%Y-%m-%d %H:%M:%S").to_string()
}

/// Generate a unified diff (patch) string from chunks and source texts.
pub(super) fn generate_unified_diff(
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

// ─── Diff view helpers ─────────────────────────────────────────────────────

pub(super) fn setup_diff_tags(buffer: &TextBuffer) {
    let table = buffer.tag_table();
    table.add(
        &TextTag::builder()
            .name("changed")
            .paragraph_background("#fff3cd")
            .build(),
    );
    table.add(
        &TextTag::builder()
            .name("deleted")
            .paragraph_background("#f8d7da")
            .build(),
    );
    table.add(
        &TextTag::builder()
            .name("inserted")
            .paragraph_background("#d4edda")
            .build(),
    );
    table.add(
        &TextTag::builder()
            .name("conflict")
            .paragraph_background("#f5c6cb")
            .build(),
    );
    // Search match highlighting
    table.add(
        &TextTag::builder()
            .name("search-match")
            .background("#ffe066")
            .build(),
    );
    table.add(
        &TextTag::builder()
            .name("search-current")
            .background("#ff9632")
            .build(),
    );
    // Inline word-level highlighting (layered on top of line-level paragraph_background)
    table.add(
        &TextTag::builder()
            .name("inline-changed")
            .background("#f0d080")
            .build(),
    );
    table.add(
        &TextTag::builder()
            .name("inline-deleted")
            .background("#f0b0b0")
            .build(),
    );
    table.add(
        &TextTag::builder()
            .name("inline-inserted")
            .background("#a0dfa0")
            .build(),
    );
}

pub(super) fn create_source_buffer(file_path: &Path, settings: &Settings) -> TextBuffer {
    let buf = sourceview5::Buffer::new(None::<&gtk4::TextTagTable>);
    let lang_mgr = sourceview5::LanguageManager::default();
    let filename = file_path.file_name().and_then(|n| n.to_str());
    if let Some(lang) = lang_mgr.guess_language(filename, None::<&str>) {
        buf.set_language(Some(&lang));
    }
    buf.set_highlight_syntax(true);
    let scheme_mgr = sourceview5::StyleSchemeManager::default();
    if let Some(scheme) = scheme_mgr.scheme(&settings.style_scheme) {
        buf.set_style_scheme(Some(&scheme));
    }
    setup_diff_tags(buf.upcast_ref());
    buf.upcast()
}

pub(super) fn remove_diff_tags(buf: &TextBuffer) {
    let start = buf.start_iter();
    let end = buf.end_iter();
    for name in &[
        "changed",
        "deleted",
        "inserted",
        "conflict",
        "inline-changed",
        "inline-deleted",
        "inline-inserted",
    ] {
        if let Some(tag) = buf.tag_table().lookup(name) {
            buf.remove_tag(&tag, &start, &end);
        }
    }
    remove_filler_tags(buf);
}

// ─── Filler (placeholder) lines ────────────────────────────────────────────

// Colors matching the diff tag backgrounds
const FILLER_DELETE: (f64, f64, f64) = (0.973, 0.843, 0.855); // #f8d7da
const FILLER_INSERT: (f64, f64, f64) = (0.831, 0.929, 0.855); // #d4edda
const FILLER_REPLACE: (f64, f64, f64) = (1.0, 0.953, 0.804); // #fff3cd

/// Draw thin colored lines at filler (pixel-padding) gaps to indicate missing content.
/// The line is drawn at the boundary between the filler gap and the anchor line,
/// which aligns with where the gutter arrow points.
#[allow(clippy::too_many_arguments)]
pub(super) fn draw_fillers(
    cr: &gtk4::cairo::Context,
    width: f64,
    tv: &TextView,
    scroll: &ScrolledWindow,
    chunks: &[DiffChunk],
    side_is_left: bool,
    _line_height: i32,
) {
    let scroll_y = scroll.vadjustment().value();
    let view_h = scroll.vadjustment().page_size();
    let line_thickness = 2.0_f64;

    for chunk in chunks {
        if chunk.tag == DiffTag::Equal {
            continue;
        }
        let left_lines = chunk.end_a - chunk.start_a;
        let right_lines = chunk.end_b - chunk.start_b;
        if left_lines == right_lines {
            continue;
        }

        // Determine if this pane needs a filler for this chunk
        let (need_filler, anchor) = if side_is_left && right_lines > left_lines {
            (true, chunk.end_a)
        } else if !side_is_left && left_lines > right_lines {
            (true, chunk.end_b)
        } else {
            (false, 0)
        };
        if !need_filler {
            continue;
        }

        let color = match chunk.tag {
            DiffTag::Delete => FILLER_DELETE,
            DiffTag::Insert => FILLER_INSERT,
            DiffTag::Replace => FILLER_REPLACE,
            DiffTag::Equal => unreachable!(),
        };

        // The anchor line is right after the filler gap. Draw the thin line
        // at the top of the anchor line (= bottom of the filler gap), which
        // matches where the gutter arrow/band points.
        let buf = tv.buffer();
        let anchor_y_buf = {
            let anchor_i32 = anchor.min(buf.line_count().max(0) as usize) as i32;
            if let Some(iter) = buf.iter_at_line(anchor_i32) {
                tv.line_yrange(&iter).0 as f64
            } else {
                let iter = buf.end_iter();
                let (y, h) = tv.line_yrange(&iter);
                (y + h) as f64
            }
        };

        let line_y = anchor_y_buf - scroll_y;

        // Skip if not visible
        if line_y + line_thickness < 0.0 || line_y - line_thickness > view_h {
            continue;
        }

        cr.set_source_rgb(color.0, color.1, color.2);
        cr.rectangle(0.0, line_y - line_thickness / 2.0, width, line_thickness);
        let _ = cr.fill();
    }
}

pub(super) fn get_line_height(tv: &TextView) -> i32 {
    let buf = tv.buffer();
    if buf.line_count() > 0
        && let Some(iter) = buf.iter_at_line(0)
    {
        let (_y, h) = tv.line_yrange(&iter);
        if h > 0 {
            return h;
        }
    }
    16 // fallback
}

pub(super) fn remove_filler_tags(buf: &TextBuffer) {
    let table = buf.tag_table();
    let to_remove: Rc<RefCell<Vec<TextTag>>> = Rc::new(RefCell::new(Vec::new()));
    let tr = to_remove.clone();
    table.foreach(move |tag| {
        if tag.name().is_some_and(|n| n.starts_with("filler-")) {
            tr.borrow_mut().push(tag.clone());
        }
    });
    let (start, end) = (buf.start_iter(), buf.end_iter());
    for tag in to_remove.borrow().iter() {
        buf.remove_tag(tag, &start, &end);
        table.remove(tag);
    }
}

pub(super) fn apply_single_filler(buf: &TextBuffer, anchor_line: usize, pixels: i32) {
    if pixels <= 0 {
        return;
    }
    let name = format!("filler-{anchor_line}");
    let tag = if anchor_line == 0 {
        TextTag::builder()
            .name(&name)
            .pixels_above_lines(pixels)
            .build()
    } else {
        TextTag::builder()
            .name(&name)
            .pixels_below_lines(pixels)
            .build()
    };
    buf.tag_table().add(&tag);

    let target_line = if anchor_line == 0 {
        0
    } else {
        anchor_line as i32 - 1
    };
    if let Some(start) = buf.iter_at_line(target_line) {
        let end = buf.iter_at_line(target_line + 1).unwrap_or(buf.end_iter());
        buf.apply_tag(&tag, &start, &end);
    }
}

pub(super) fn apply_filler_tags(
    left_buf: &TextBuffer,
    right_buf: &TextBuffer,
    chunks: &[DiffChunk],
    line_height: i32,
) {
    remove_filler_tags(left_buf);
    remove_filler_tags(right_buf);

    for chunk in chunks {
        if chunk.tag == DiffTag::Equal {
            continue;
        }
        let left_lines = chunk.end_a - chunk.start_a;
        let right_lines = chunk.end_b - chunk.start_b;
        if left_lines == right_lines {
            continue;
        }
        if left_lines > right_lines {
            let pixels = (left_lines - right_lines) as i32 * line_height;
            apply_single_filler(right_buf, chunk.end_b, pixels);
        } else {
            let pixels = (right_lines - left_lines) as i32 * line_height;
            apply_single_filler(left_buf, chunk.end_a, pixels);
        }
    }
}

pub(super) fn apply_merge_filler_tags(
    left_buf: &TextBuffer,
    middle_buf: &TextBuffer,
    right_buf: &TextBuffer,
    left_chunks: &[DiffChunk],
    right_chunks: &[DiffChunk],
    line_height: i32,
) {
    remove_filler_tags(left_buf);
    remove_filler_tags(middle_buf);
    remove_filler_tags(right_buf);

    // Middle buffer may get fillers from both diff pairs — accumulate
    let mut middle_fillers: HashMap<usize, i32> = HashMap::new();

    // left_chunks = diff(left, middle): a-side = left, b-side = middle
    for chunk in left_chunks {
        if chunk.tag == DiffTag::Equal {
            continue;
        }
        let left_lines = chunk.end_a - chunk.start_a;
        let mid_lines = chunk.end_b - chunk.start_b;
        if left_lines == mid_lines {
            continue;
        }
        if left_lines > mid_lines {
            let pixels = (left_lines - mid_lines) as i32 * line_height;
            *middle_fillers.entry(chunk.end_b).or_insert(0) += pixels;
            // Right side also needs this filler to stay aligned
            apply_single_filler(right_buf, chunk.end_b, pixels);
        } else {
            let pixels = (mid_lines - left_lines) as i32 * line_height;
            apply_single_filler(left_buf, chunk.end_a, pixels);
        }
    }

    // right_chunks = diff(middle, right): a-side = middle, b-side = right
    for chunk in right_chunks {
        if chunk.tag == DiffTag::Equal {
            continue;
        }
        let mid_lines = chunk.end_a - chunk.start_a;
        let right_lines = chunk.end_b - chunk.start_b;
        if mid_lines == right_lines {
            continue;
        }
        if mid_lines > right_lines {
            let pixels = (mid_lines - right_lines) as i32 * line_height;
            apply_single_filler(right_buf, chunk.end_b, pixels);
        } else {
            let pixels = (right_lines - mid_lines) as i32 * line_height;
            *middle_fillers.entry(chunk.end_a).or_insert(0) += pixels;
            // Left side also needs this filler to stay aligned
            apply_single_filler(left_buf, chunk.end_a, pixels);
        }
    }

    // Apply accumulated middle fillers
    for (anchor, pixels) in &middle_fillers {
        apply_single_filler(middle_buf, *anchor, *pixels);
    }
}

/// Detect conflicts: overlapping non-Equal regions from two pairwise diffs on the middle file.
/// `left_chunks`: diff(left, middle) — middle lines are `start_b..end_b`
/// `right_chunks`: diff(middle, right) — middle lines are `start_a..end_a`
pub(super) fn apply_conflict_tags(
    middle_buf: &TextBuffer,
    left_chunks: &[DiffChunk],
    right_chunks: &[DiffChunk],
) {
    for lc in left_chunks {
        if lc.tag == DiffTag::Equal {
            continue;
        }
        // In the left diff (left vs middle), the middle file range is start_b..end_b
        let l_start = lc.start_b;
        let l_end = lc.end_b;

        for rc in right_chunks {
            if rc.tag == DiffTag::Equal {
                continue;
            }
            // In the right diff (middle vs right), the middle file range is start_a..end_a
            let r_start = rc.start_a;
            let r_end = rc.end_a;

            // Check for overlap (handle zero-length ranges as point insertions)
            let overlap_start = l_start.max(r_start);
            let overlap_end = l_end.max(l_start).min(r_end.max(r_start));

            // Two ranges overlap if they share any lines, or if both are insertions at the same point
            let overlaps = if l_start == l_end && r_start == r_end {
                l_start == r_start
            } else if l_start == l_end {
                l_start >= r_start && l_start < r_end
            } else if r_start == r_end {
                r_start >= l_start && r_start < l_end
            } else {
                overlap_start < overlap_end
            };

            if overlaps {
                // Tag the union of both ranges on the middle buffer
                let tag_start = l_start.min(r_start);
                let tag_end = l_end.max(r_end);
                apply_line_tag(middle_buf, "conflict", tag_start, tag_end);
            }
        }
    }
}

pub(super) fn apply_line_tag(
    buffer: &TextBuffer,
    tag_name: &str,
    start_line: usize,
    end_line: usize,
) {
    let start = buffer.iter_at_line(start_line as i32);
    let end = if (end_line as i32) < buffer.line_count() {
        buffer.iter_at_line(end_line as i32)
    } else {
        Some(buffer.end_iter())
    };
    if let (Some(s), Some(e)) = (start, end) {
        buffer.apply_tag_by_name(tag_name, &s, &e);
    }
}

pub(super) fn get_line_text(buf: &TextBuffer, line: usize) -> String {
    let start = buf
        .iter_at_line(line as i32)
        .unwrap_or_else(|| buf.start_iter());
    let mut end = start;
    end.forward_to_line_end();
    buf.text(&start, &end, false).to_string()
}

pub(super) fn apply_char_tag(
    buf: &TextBuffer,
    tag_name: &str,
    line: usize,
    tokens: &[myers::Token<'_>],
    tok_start: usize,
    tok_end: usize,
) {
    if tok_start >= tok_end || tok_start >= tokens.len() {
        return;
    }
    let byte_start = tokens[tok_start].offset;
    let byte_end = if tok_end < tokens.len() {
        tokens[tok_end].offset
    } else {
        tokens.last().map_or(0, |t| t.offset + t.text.len())
    };
    // iter_at_line_offset uses character (not byte) offsets, so convert
    let line_text = get_line_text(buf, line);
    let char_start = line_text[..byte_start].chars().count() as i32;
    let char_end = line_text[..byte_end].chars().count() as i32;
    let s = buf.iter_at_line_offset(line as i32, char_start);
    let e = buf.iter_at_line_offset(line as i32, char_end);
    if let (Some(s), Some(e)) = (s, e) {
        buf.apply_tag_by_name(tag_name, &s, &e);
    }
}

pub(super) fn apply_inline_tags(left_buf: &TextBuffer, right_buf: &TextBuffer, chunk: &DiffChunk) {
    let n = std::cmp::min(chunk.end_a - chunk.start_a, chunk.end_b - chunk.start_b);
    for i in 0..n {
        let left_line = chunk.start_a + i;
        let right_line = chunk.start_b + i;
        let left_text = get_line_text(left_buf, left_line);
        let right_text = get_line_text(right_buf, right_line);

        let (toks_a, toks_b, word_chunks) = myers::diff_words(&left_text, &right_text);

        for wc in &word_chunks {
            match wc.tag {
                DiffTag::Equal => {}
                DiffTag::Replace => {
                    apply_char_tag(
                        left_buf,
                        "inline-changed",
                        left_line,
                        &toks_a,
                        wc.start_a,
                        wc.end_a,
                    );
                    apply_char_tag(
                        right_buf,
                        "inline-changed",
                        right_line,
                        &toks_b,
                        wc.start_b,
                        wc.end_b,
                    );
                }
                DiffTag::Delete => {
                    apply_char_tag(
                        left_buf,
                        "inline-deleted",
                        left_line,
                        &toks_a,
                        wc.start_a,
                        wc.end_a,
                    );
                }
                DiffTag::Insert => {
                    apply_char_tag(
                        right_buf,
                        "inline-inserted",
                        right_line,
                        &toks_b,
                        wc.start_b,
                        wc.end_b,
                    );
                }
            }
        }
    }
}

pub(super) fn make_info_bar(message: &str) -> GtkBox {
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

pub(super) fn shortened_path(full: &Path) -> String {
    let components: Vec<_> = full.components().collect();
    if components.len() <= 2 {
        return full.display().to_string();
    }
    let tail: std::path::PathBuf = components[components.len() - 2..].iter().collect();
    format!("\u{2026}/{}", tail.display())
}

pub(super) struct DiffPane {
    pub(super) container: GtkBox,
    pub(super) text_view: TextView,
    pub(super) scroll: ScrolledWindow,
    pub(super) filler_overlay: DrawingArea,
    pub(super) save_btn: Button,
    pub(super) path_label: Label,
}

pub(super) fn make_diff_pane(
    buf: &TextBuffer,
    file_path: &Path,
    info: Option<&str>,
    label_override: Option<&str>,
    settings: &Settings,
) -> DiffPane {
    let sv = sourceview5::View::with_buffer(
        buf.downcast_ref::<sourceview5::Buffer>()
            .expect("buffer must be a sourceview5::Buffer"),
    );
    sv.set_editable(true);
    sv.set_wrap_mode(settings.wrap_mode_gtk());
    sv.set_left_margin(4);
    sv.set_show_line_numbers(settings.show_line_numbers);
    sv.set_highlight_current_line(settings.highlight_current_line);
    sv.set_tab_width(settings.tab_width);
    sv.set_hexpand(true);
    update_font_css(settings);
    sv.add_css_class("meld-editor");

    let scroll = ScrolledWindow::builder()
        .min_content_width(360)
        .vexpand(true)
        .child(&sv)
        .build();

    let tv: gtk4::TextView = sv.upcast();

    let header = GtkBox::new(Orientation::Horizontal, 4);
    header.set_margin_start(4);
    header.set_margin_end(4);
    header.set_margin_top(2);
    header.set_margin_bottom(2);
    let save_btn = Button::from_icon_name("document-save-symbolic");
    save_btn.set_tooltip_text(Some("Save"));
    save_btn.set_sensitive(false);
    let display_name = label_override.map_or_else(|| shortened_path(file_path), String::from);
    let path_label = Label::new(Some(&display_name));
    path_label.set_hexpand(true);
    path_label.set_halign(gtk4::Align::Center);
    header.append(&save_btn);
    header.append(&path_label);

    // Enable save button when buffer content changes
    {
        let btn = save_btn.clone();
        buf.connect_changed(move |_| {
            btn.set_sensitive(true);
        });
    }

    let buf_clone = buf.clone();
    let path_owned = file_path.to_path_buf();
    let save_btn_ref = save_btn.clone();
    save_btn.connect_clicked(move |_| {
        let text = buf_clone.text(&buf_clone.start_iter(), &buf_clone.end_iter(), false);
        save_file(&path_owned, text.as_str(), &save_btn_ref);
    });

    let filler_overlay = DrawingArea::new();
    filler_overlay.set_can_target(false);

    let overlay = gtk4::Overlay::new();
    overlay.set_child(Some(&scroll));
    overlay.add_overlay(&filler_overlay);

    let vbox = GtkBox::new(Orientation::Vertical, 0);
    vbox.append(&header);
    if let Some(msg) = info {
        vbox.append(&make_info_bar(msg));
    }
    vbox.append(&overlay);

    DiffPane {
        container: vbox,
        text_view: tv,
        scroll,
        filler_overlay,
        save_btn,
        path_label,
    }
}

// ─── Gutter (link map) ────────────────────────────────────────────────────

pub(super) fn line_to_gutter_y(
    tv: &TextView,
    buf: &TextBuffer,
    line: usize,
    scroll: &ScrolledWindow,
    gutter: &impl IsA<gtk4::Widget>,
) -> f64 {
    // Use line_yrange per line so word-wrapped lines get correct positions.
    if let Some(iter) = buf.iter_at_line(line as i32) {
        let (y, _h) = tv.line_yrange(&iter);
        let visible_y = y as f64 - scroll.vadjustment().value();
        let point = gtk4::graphene::Point::new(0.0, visible_y as f32);
        if let Some(out) = scroll.compute_point(gutter, &point) {
            return out.y() as f64;
        }
    }
    0.0
}

#[allow(clippy::too_many_arguments)]
pub(super) fn draw_gutter(
    cr: &gtk4::cairo::Context,
    width: f64,
    left_tv: &TextView,
    right_tv: &TextView,
    left_buf: &TextBuffer,
    right_buf: &TextBuffer,
    left_scroll: &ScrolledWindow,
    right_scroll: &ScrolledWindow,
    gutter: &DrawingArea,
    chunks: &[DiffChunk],
) {
    for chunk in chunks {
        if chunk.tag == DiffTag::Equal {
            continue;
        }

        let lt = line_to_gutter_y(left_tv, left_buf, chunk.start_a, left_scroll, gutter);
        let lb = line_to_gutter_y(left_tv, left_buf, chunk.end_a, left_scroll, gutter);
        let rt = line_to_gutter_y(right_tv, right_buf, chunk.start_b, right_scroll, gutter);
        let rb = line_to_gutter_y(right_tv, right_buf, chunk.end_b, right_scroll, gutter);

        let (r, g, b) = match chunk.tag {
            DiffTag::Replace => (0.45, 0.62, 0.81),
            DiffTag::Delete => (0.96, 0.47, 0.0),
            DiffTag::Insert => (0.45, 0.82, 0.09),
            DiffTag::Equal => continue,
        };

        // Filled band
        cr.set_source_rgba(r, g, b, 0.3);
        cr.move_to(0.0, lt);
        cr.curve_to(width * 0.5, lt, width * 0.5, rt, width, rt);
        cr.line_to(width, rb);
        cr.curve_to(width * 0.5, rb, width * 0.5, lb, 0.0, lb);
        cr.close_path();
        let _ = cr.fill();

        // Edge-aligned arrows (meld style)
        let left_mid = f64::midpoint(lt, lb);
        let right_mid = f64::midpoint(rt, rb);
        draw_edge_arrow(cr, 2.0, left_mid, true, r, g, b);
        draw_edge_arrow(cr, width - 2.0, right_mid, false, r, g, b);
    }
}

#[allow(clippy::many_single_char_names)]
pub(super) fn draw_edge_arrow(
    cr: &gtk4::cairo::Context,
    x: f64,
    y: f64,
    points_right: bool,
    r: f64,
    g: f64,
    b: f64,
) {
    let size = 5.0;
    cr.set_source_rgba(r, g, b, 0.9);
    if points_right {
        cr.move_to(x, y - size);
        cr.line_to(x + size * 1.5, y);
        cr.line_to(x, y + size);
    } else {
        cr.move_to(x, y - size);
        cr.line_to(x - size * 1.5, y);
        cr.line_to(x, y + size);
    }
    cr.close_path();
    let _ = cr.fill();
}

pub(super) fn get_lines_text(buf: &TextBuffer, start_line: usize, end_line: usize) -> String {
    if start_line >= end_line {
        return String::new();
    }
    let start = buf
        .iter_at_line(start_line as i32)
        .unwrap_or(buf.start_iter());
    let end = if (end_line as i32) < buf.line_count() {
        buf.iter_at_line(end_line as i32).unwrap_or(buf.end_iter())
    } else {
        buf.end_iter()
    };
    buf.text(&start, &end, false).to_string()
}

pub(super) fn apply_paste_highlight(buf: &TextBuffer, start_line: usize, end_line: usize) {
    let table = buf.tag_table();
    let tag = table.lookup("paste-highlight").unwrap_or_else(|| {
        let t = TextTag::builder()
            .name("paste-highlight")
            .background("#a0d0ff")
            .build();
        table.add(&t);
        t
    });
    let start = buf
        .iter_at_line(start_line as i32)
        .unwrap_or(buf.start_iter());
    let end = if (end_line as i32) < buf.line_count() {
        buf.iter_at_line(end_line as i32).unwrap_or(buf.end_iter())
    } else {
        buf.end_iter()
    };
    buf.apply_tag(&tag, &start, &end);

    let b = buf.clone();
    gtk4::glib::timeout_add_local_once(Duration::from_millis(500), move || {
        if let Some(t) = b.tag_table().lookup("paste-highlight") {
            b.remove_tag(&t, &b.start_iter(), &b.end_iter());
        }
    });
}

pub(super) fn copy_chunk(
    src_buf: &TextBuffer,
    src_start: usize,
    src_end: usize,
    dst_buf: &TextBuffer,
    dst_start: usize,
    dst_end: usize,
) {
    let mut src_text = get_lines_text(src_buf, src_start, src_end);

    // EOF: ensure trailing newline when pasting at/past the last line
    if dst_end as i32 >= dst_buf.line_count() && !src_text.ends_with('\n') && !src_text.is_empty() {
        src_text.push('\n');
    }

    let ds = dst_buf
        .iter_at_line(dst_start as i32)
        .unwrap_or(dst_buf.start_iter());

    // Mark at insertion start (left gravity stays before inserted text)
    let mark = dst_buf.create_mark(None, &ds, true);

    dst_buf.begin_user_action();

    let mut de = if (dst_end as i32) < dst_buf.line_count() {
        dst_buf
            .iter_at_line(dst_end as i32)
            .unwrap_or(dst_buf.end_iter())
    } else {
        dst_buf.end_iter()
    };
    let mut ds = dst_buf.iter_at_mark(&mark);
    dst_buf.delete(&mut ds, &mut de);
    dst_buf.insert(&mut ds, &src_text);

    dst_buf.end_user_action();

    // Place cursor at start of inserted text
    let insert_start = dst_buf.iter_at_mark(&mark);
    dst_buf.place_cursor(&insert_start);
    dst_buf.delete_mark(&mark);

    // Flash highlight on inserted range
    if src_start < src_end {
        apply_paste_highlight(dst_buf, dst_start, dst_start + (src_end - src_start));
    }
}

#[allow(clippy::too_many_arguments)]
pub(super) fn refresh_diff(
    left_buf: &TextBuffer,
    right_buf: &TextBuffer,
    left_tv: &TextView,
    chunks: &Rc<RefCell<Vec<DiffChunk>>>,
    line_height_cell: &Rc<Cell<i32>>,
    on_complete: impl Fn() + 'static,
    ignore_blanks: bool,
    ignore_whitespace: bool,
    pending: &Rc<Cell<bool>>,
) {
    let lt = left_buf
        .text(&left_buf.start_iter(), &left_buf.end_iter(), false)
        .to_string();
    let rt = right_buf
        .text(&right_buf.start_iter(), &right_buf.end_iter(), false)
        .to_string();

    remove_diff_tags(left_buf);
    remove_diff_tags(right_buf);

    let lb = left_buf.clone();
    let rb = right_buf.clone();
    let ch = chunks.clone();
    let p = pending.clone();
    // Capture line height AFTER filler tags are removed but BEFORE new ones are applied
    let lh = get_line_height(left_tv);
    line_height_cell.set(lh);

    gtk4::glib::spawn_future_local(async move {
        let (lt_cmp, lt_map) = filter_for_diff(&lt, ignore_whitespace, ignore_blanks);
        let (rt_cmp, rt_map) = filter_for_diff(&rt, ignore_whitespace, ignore_blanks);
        let lt_total = lt.lines().count();
        let rt_total = rt.lines().count();
        let new_chunks = if lt_cmp == rt_cmp {
            Vec::new()
        } else {
            let raw = gio::spawn_blocking(move || myers::diff_lines(&lt_cmp, &rt_cmp))
                .await
                .unwrap_or_default();
            remap_chunks(raw, &lt_map, lt_total, &rt_map, rt_total)
        };
        apply_diff_tags(&lb, &rb, &new_chunks);
        apply_filler_tags(&lb, &rb, &new_chunks, lh);
        *ch.borrow_mut() = new_chunks;
        on_complete();
        p.set(false);
    });
}

#[allow(clippy::too_many_arguments)]
pub(super) fn handle_gutter_click(
    x: f64,
    y: f64,
    width: f64,
    left_tv: &TextView,
    right_tv: &TextView,
    left_buf: &TextBuffer,
    right_buf: &TextBuffer,
    left_scroll: &ScrolledWindow,
    right_scroll: &ScrolledWindow,
    gutter: &DrawingArea,
    chunks: &Rc<RefCell<Vec<DiffChunk>>>,
) {
    let snapshot: Vec<DiffChunk> = chunks.borrow().clone();
    for chunk in &snapshot {
        if chunk.tag == DiffTag::Equal {
            continue;
        }
        let lt = line_to_gutter_y(left_tv, left_buf, chunk.start_a, left_scroll, gutter);
        let lb = line_to_gutter_y(left_tv, left_buf, chunk.end_a, left_scroll, gutter);
        let rt = line_to_gutter_y(right_tv, right_buf, chunk.start_b, right_scroll, gutter);
        let rb = line_to_gutter_y(right_tv, right_buf, chunk.end_b, right_scroll, gutter);
        let left_mid = f64::midpoint(lt, lb);
        let right_mid = f64::midpoint(rt, rb);

        let hit = 12.0_f64;
        // → arrow at left edge: copy left to right
        if (x - 2.0).powi(2) + (y - left_mid).powi(2) < hit * hit {
            copy_chunk(
                left_buf,
                chunk.start_a,
                chunk.end_a,
                right_buf,
                chunk.start_b,
                chunk.end_b,
            );
            return;
        }
        // ← arrow at right edge: copy right to left
        if (x - (width - 2.0)).powi(2) + (y - right_mid).powi(2) < hit * hit {
            copy_chunk(
                right_buf,
                chunk.start_b,
                chunk.end_b,
                left_buf,
                chunk.start_a,
                chunk.end_a,
            );
            return;
        }
    }
}

// ─── Search helpers ─────────────────────────────────────────────────────────

pub(super) fn clear_search_tags(buf: &TextBuffer) {
    let start = buf.start_iter();
    let end = buf.end_iter();
    for name in &["search-match", "search-current"] {
        if let Some(tag) = buf.tag_table().lookup(name) {
            buf.remove_tag(&tag, &start, &end);
        }
    }
}

/// Highlight all occurrences of `needle` in `buf`. Returns match count.
pub(super) fn highlight_search_matches(buf: &TextBuffer, needle: &str) -> usize {
    clear_search_tags(buf);
    if needle.is_empty() {
        return 0;
    }
    let mut count = 0;
    let mut iter = buf.start_iter();
    while let Some((match_start, match_end)) =
        iter.forward_search(needle, TextSearchFlags::CASE_INSENSITIVE, None)
    {
        buf.apply_tag_by_name("search-match", &match_start, &match_end);
        count += 1;
        iter = match_end;
    }
    count
}

/// Find the next (or previous) match starting from `from`. Returns (start, end) iters.
pub(super) fn find_next_match(
    buf: &TextBuffer,
    needle: &str,
    from: &gtk4::TextIter,
    forward: bool,
) -> Option<(gtk4::TextIter, gtk4::TextIter)> {
    if needle.is_empty() {
        return None;
    }
    if forward {
        let result = from.forward_search(needle, TextSearchFlags::CASE_INSENSITIVE, None);
        if result.is_some() {
            return result;
        }
        // Wrap around
        buf.start_iter()
            .forward_search(needle, TextSearchFlags::CASE_INSENSITIVE, None)
    } else {
        let result = from.backward_search(needle, TextSearchFlags::CASE_INSENSITIVE, None);
        if result.is_some() {
            return result;
        }
        // Wrap around
        buf.end_iter()
            .backward_search(needle, TextSearchFlags::CASE_INSENSITIVE, None)
    }
}

// ─── Chunk map helpers ──────────────────────────────────────────────────────

pub(super) fn draw_chunk_map(
    cr: &gtk4::cairo::Context,
    _width: f64,
    height: f64,
    total_lines: i32,
    scroll: &ScrolledWindow,
    chunks: &[DiffChunk],
    is_left: bool,
) {
    if total_lines <= 0 || height <= 0.0 {
        return;
    }
    let lines = total_lines as f64;

    for chunk in chunks {
        if chunk.tag == DiffTag::Equal {
            continue;
        }

        let (start, end) = if is_left {
            (chunk.start_a, chunk.end_a)
        } else {
            (chunk.start_b, chunk.end_b)
        };

        let y_start = (start as f64 / lines) * height;
        let y_end = (end as f64 / lines) * height;
        let rect_h = (y_end - y_start).max(2.0);

        let (r, g, b) = match chunk.tag {
            DiffTag::Replace => (0.45, 0.62, 0.81),
            DiffTag::Delete => (0.96, 0.47, 0.0),
            DiffTag::Insert => (0.45, 0.82, 0.09),
            DiffTag::Equal => continue,
        };

        cr.set_source_rgba(r, g, b, 0.7);
        cr.rectangle(1.0, y_start, 10.0, rect_h);
        let _ = cr.fill();
    }

    // Viewport indicator
    let adj = scroll.vadjustment();
    if adj.upper() > 0.0 {
        let view_start = (adj.value() / adj.upper()) * height;
        let view_h = (adj.page_size() / adj.upper()) * height;
        cr.set_source_rgba(0.0, 0.0, 0.0, 0.12);
        cr.rectangle(0.0, view_start, 12.0, view_h);
        let _ = cr.fill();
        cr.set_source_rgba(0.0, 0.0, 0.0, 0.25);
        cr.rectangle(0.0, view_start, 12.0, 1.0);
        let _ = cr.fill();
        cr.rectangle(0.0, view_start + view_h - 1.0, 12.0, 1.0);
        let _ = cr.fill();
    }
}

// ─── Chunk navigation helpers ──────────────────────────────────────────────

pub(super) fn count_changes(chunks: &[DiffChunk]) -> usize {
    chunks.iter().filter(|c| c.tag != DiffTag::Equal).count()
}

pub(super) fn navigate_chunk(
    chunks: &[DiffChunk],
    current_chunk: &Rc<Cell<Option<usize>>>,
    direction: i32, // -1 = prev, +1 = next
    left_tv: &TextView,
    left_buf: &TextBuffer,
    left_scroll: &ScrolledWindow,
) {
    let non_equal: Vec<usize> = chunks
        .iter()
        .enumerate()
        .filter(|(_, c)| c.tag != DiffTag::Equal)
        .map(|(i, _)| i)
        .collect();

    if non_equal.is_empty() {
        return;
    }

    let next_idx = match current_chunk.get() {
        Some(cur) => {
            let pos = non_equal.iter().position(|&i| i == cur);
            match pos {
                Some(p) => {
                    if direction > 0 {
                        non_equal.get(p + 1).or(non_equal.first())
                    } else if p > 0 {
                        non_equal.get(p - 1)
                    } else {
                        non_equal.last()
                    }
                }
                None => {
                    if direction > 0 {
                        non_equal.first()
                    } else {
                        non_equal.last()
                    }
                }
            }
        }
        None => {
            if direction > 0 {
                non_equal.first()
            } else {
                non_equal.last()
            }
        }
    };

    if let Some(&idx) = next_idx {
        current_chunk.set(Some(idx));
        let chunk = &chunks[idx];
        // Scroll left pane to the chunk
        scroll_to_line(left_tv, left_buf, chunk.start_a, left_scroll);
    }
}

pub(super) fn scroll_to_line(
    tv: &TextView,
    buf: &TextBuffer,
    line: usize,
    scroll: &ScrolledWindow,
) {
    if let Some(iter) = buf.iter_at_line(line as i32) {
        let (y, h) = tv.line_yrange(&iter);
        let visible_h = scroll.vadjustment().page_size();
        // Center the line in the visible area
        let target = (y as f64 + h as f64 / 2.0) - visible_h / 2.0;
        scroll.vadjustment().set_value(target.max(0.0));
    }
}

pub(super) fn update_chunk_label(label: &Label, chunks: &[DiffChunk], current: Option<usize>) {
    let total = count_changes(chunks);
    if total == 0 {
        label.set_label("No changes");
        return;
    }
    let non_equal: Vec<usize> = chunks
        .iter()
        .enumerate()
        .filter(|(_, c)| c.tag != DiffTag::Equal)
        .map(|(i, _)| i)
        .collect();
    match current {
        Some(cur) => {
            if let Some(pos) = non_equal.iter().position(|&i| i == cur) {
                label.set_label(&format!("Change {} of {}", pos + 1, total));
            } else {
                label.set_label(&format!("{total} changes"));
            }
        }
        None => label.set_label(&format!("{total} changes")),
    }
}

// ─── Scroll synchronization ───────────────────────────────────────────────

pub(super) fn setup_scroll_sync(
    left_scroll: &ScrolledWindow,
    right_scroll: &ScrolledWindow,
    gutter: &DrawingArea,
) {
    let syncing = Rc::new(Cell::new(false));

    // Left → Right
    {
        let rs = right_scroll.clone();
        let s = syncing.clone();
        let g = gutter.clone();
        left_scroll.vadjustment().connect_value_changed(move |adj| {
            if !s.get() {
                s.set(true);
                sync_adjustment(&rs.vadjustment(), adj);
                g.queue_draw();
                s.set(false);
            }
        });
    }

    // Right → Left
    {
        let ls = left_scroll.clone();
        let s = syncing.clone();
        let g = gutter.clone();
        right_scroll
            .vadjustment()
            .connect_value_changed(move |adj| {
                if !s.get() {
                    s.set(true);
                    sync_adjustment(&ls.vadjustment(), adj);
                    g.queue_draw();
                    s.set(false);
                }
            });
    }

    // Horizontal: Left → Right
    {
        let rs = right_scroll.clone();
        let s = syncing.clone();
        left_scroll.hadjustment().connect_value_changed(move |adj| {
            if !s.get() {
                s.set(true);
                rs.hadjustment().set_value(adj.value());
                s.set(false);
            }
        });
    }

    // Horizontal: Right → Left
    {
        let ls = left_scroll.clone();
        let s = syncing.clone();
        right_scroll
            .hadjustment()
            .connect_value_changed(move |adj| {
                if !s.get() {
                    s.set(true);
                    ls.hadjustment().set_value(adj.value());
                    s.set(false);
                }
            });
    }
}

pub(super) fn sync_adjustment(target: &Adjustment, source: &Adjustment) {
    let src_max = source.upper() - source.page_size();
    if src_max <= 0.0 {
        return;
    }
    let ratio = source.value() / src_max;
    // Syncpoint-based: use the scroll ratio as a virtual anchor position
    let value = ratio * target.upper() - ratio * target.page_size();
    let tgt_max = target.upper() - target.page_size();
    target.set_value(value.clamp(0.0, tgt_max));
}

// ─── Unsaved-changes helpers ────────────────────────────────────────────────

/// Show a Meld-style "Save changes to documents before closing?" dialog.
///
/// `unsaved` lists (`display_path`, `save_button`) for each file with unsaved
/// changes.  The user can check / uncheck individual files.
///
/// * **Save** — clicks the save button for every checked file, then calls
///   `on_close`.
/// * **Close without Saving** — marks every save button insensitive (so the
///   subsequent close-request won't re-trigger), then calls `on_close`.
/// * **Cancel** — dismisses the dialog; nothing happens.
pub(super) fn confirm_unsaved_dialog(
    parent: &ApplicationWindow,
    unsaved: Vec<(String, Button)>,
    on_close: impl Fn() + 'static,
) {
    let dialog = gtk4::Window::builder()
        .modal(true)
        .transient_for(parent)
        .resizable(false)
        .decorated(true)
        .deletable(false)
        .build();

    let content = GtkBox::new(Orientation::Vertical, 6);
    content.set_margin_top(18);
    content.set_margin_bottom(18);
    content.set_margin_start(18);
    content.set_margin_end(18);

    let title_label = gtk4::Label::new(Some("Save changes to documents before closing?"));
    title_label.add_css_class("title-3");
    title_label.set_margin_bottom(4);
    content.append(&title_label);

    let subtitle = gtk4::Label::new(Some(
        "If you don\u{2019}t save, changes will be permanently lost.",
    ));
    subtitle.set_margin_bottom(8);
    content.append(&subtitle);

    // Build one checkbox per unsaved file.
    let checks: Rc<Vec<(gtk4::CheckButton, Button)>> = Rc::new(
        unsaved
            .into_iter()
            .map(|(path, btn)| {
                let check = gtk4::CheckButton::with_label(&path);
                check.set_active(true);
                content.append(&check);
                (check, btn)
            })
            .collect(),
    );

    // Button row — "Close without Saving" left-aligned, Cancel + Save right.
    let btn_box = GtkBox::new(Orientation::Horizontal, 8);
    btn_box.set_margin_top(14);

    let close_btn = gtk4::Button::with_label("Close without Saving");
    close_btn.add_css_class("destructive-action");

    let spacer = GtkBox::new(Orientation::Horizontal, 0);
    spacer.set_hexpand(true);

    let cancel_btn = gtk4::Button::with_label("Cancel");

    let save_btn = gtk4::Button::with_label("Save");
    save_btn.add_css_class("suggested-action");

    btn_box.append(&close_btn);
    btn_box.append(&spacer);
    btn_box.append(&cancel_btn);
    btn_box.append(&save_btn);
    content.append(&btn_box);

    dialog.set_child(Some(&content));

    let on_close: Rc<dyn Fn()> = Rc::new(on_close);

    // Cancel — just dismiss.
    {
        let d = dialog.clone();
        cancel_btn.connect_clicked(move |_| d.close());
    }
    // Save checked files, then close (unless a save failed).
    {
        let d = dialog.clone();
        let checks = checks.clone();
        let on_close = on_close.clone();
        save_btn.connect_clicked(move |_| {
            for (check, btn) in checks.iter() {
                if check.is_active() && btn.is_sensitive() {
                    btn.emit_clicked();
                }
            }
            // If any save button is still sensitive, a save failed — don't close.
            let any_failed = checks
                .iter()
                .any(|(check, btn)| check.is_active() && btn.is_sensitive());
            if any_failed {
                return;
            }
            d.close();
            on_close();
        });
    }
    // Close without saving — mark all insensitive so the subsequent
    // close-request handler won't re-prompt.
    {
        let d = dialog.clone();
        let checks = checks.clone();
        let on_close = on_close.clone();
        close_btn.connect_clicked(move |_| {
            for (_, btn) in checks.iter() {
                btn.set_sensitive(false);
            }
            d.close();
            on_close();
        });
    }

    dialog.present();
}

/// Collect unsaved (path, button) pairs from a list of (path, button) where
/// the button is sensitive.
pub(super) fn collect_unsaved(files: Vec<(String, Button)>) -> Vec<(String, Button)> {
    files
        .into_iter()
        .filter(|(_, b)| b.is_sensitive())
        .collect()
}

/// Shared close-request handler for windows with save buttons.
pub(super) fn handle_close_request(
    window: &ApplicationWindow,
    files: Vec<(String, Button)>,
) -> gtk4::glib::Propagation {
    let unsaved = collect_unsaved(files);
    if unsaved.is_empty() {
        return gtk4::glib::Propagation::Proceed;
    }
    let w = window.clone();
    confirm_unsaved_dialog(window, unsaved, move || w.close());
    gtk4::glib::Propagation::Stop
}

/// Close a notebook file tab, prompting to save if needed.
pub(super) fn close_notebook_tab(
    window: &ApplicationWindow,
    notebook: &Notebook,
    tabs: &Rc<RefCell<Vec<FileTab>>>,
    page: u32,
) {
    let info = tabs.borrow().iter().find_map(|t| {
        if notebook.page_num(&t.widget) == Some(page) {
            Some((
                t.id,
                t.left_path.clone(),
                t.right_path.clone(),
                t.left_save.clone(),
                t.right_save.clone(),
            ))
        } else {
            None
        }
    });
    let Some((tab_id, lp, rp, ls, rs)) = info else {
        notebook.remove_page(Some(page));
        return;
    };
    let unsaved = collect_unsaved(vec![(lp, ls), (rp, rs)]);
    if unsaved.is_empty() {
        notebook.remove_page(Some(page));
        tabs.borrow_mut().retain(|t| t.id != tab_id);
        return;
    }
    let nb = notebook.clone();
    let tabs = tabs.clone();
    confirm_unsaved_dialog(window, unsaved, move || {
        nb.remove_page(Some(page));
        tabs.borrow_mut().retain(|t| t.id != tab_id);
    });
}

/// Shared close-request handler for notebook windows (VCS/dir).
pub(super) fn handle_notebook_close_request(
    window: &ApplicationWindow,
    tabs: &Rc<RefCell<Vec<FileTab>>>,
) -> gtk4::glib::Propagation {
    let unsaved: Vec<(String, Button)> = tabs
        .borrow()
        .iter()
        .flat_map(|t| {
            let mut v = Vec::new();
            if t.left_save.is_sensitive() {
                v.push((t.left_path.clone(), t.left_save.clone()));
            }
            if t.right_save.is_sensitive() {
                v.push((t.right_path.clone(), t.right_save.clone()));
            }
            v
        })
        .collect();
    if unsaved.is_empty() {
        return gtk4::glib::Propagation::Proceed;
    }
    let w = window.clone();
    confirm_unsaved_dialog(window, unsaved, move || w.close());
    gtk4::glib::Propagation::Stop
}

#[cfg(test)]
mod tests {
    use super::*;

    // ── filter_for_diff ──────────────────────────────────────────

    #[test]
    fn filter_no_options() {
        let (text, map) = filter_for_diff("hello\nworld\n", false, false);
        assert_eq!(text, "hello\nworld");
        assert_eq!(map, vec![0, 1]);
    }

    #[test]
    fn filter_ignore_blanks() {
        let (text, map) = filter_for_diff("a\n\nb\n  \nc\n", false, true);
        assert_eq!(text, "a\nb\nc");
        assert_eq!(map, vec![0, 2, 4]);
    }

    #[test]
    fn filter_ignore_whitespace() {
        let (text, map) = filter_for_diff("  hello  world  \nfoo\n", true, false);
        assert_eq!(text, "hello world\nfoo");
        assert_eq!(map, vec![0, 1]);
    }

    #[test]
    fn filter_both_options() {
        let (text, map) = filter_for_diff("  a  b  \n\n  c  \n", true, true);
        assert_eq!(text, "a b\nc");
        assert_eq!(map, vec![0, 2]);
    }

    #[test]
    fn filter_empty_input() {
        let (text, map) = filter_for_diff("", false, false);
        assert_eq!(text, "");
        assert!(map.is_empty());
    }

    #[test]
    fn filter_all_blank_lines() {
        let (text, map) = filter_for_diff("\n\n\n", false, true);
        assert_eq!(text, "");
        assert!(map.is_empty());
    }

    // ── remap_chunks ─────────────────────────────────────────────

    #[test]
    fn remap_basic() {
        let chunks = vec![DiffChunk {
            tag: DiffTag::Replace,
            start_a: 0,
            end_a: 1,
            start_b: 0,
            end_b: 1,
        }];
        let left_map = vec![2, 5];
        let right_map = vec![1, 3];
        let remapped = remap_chunks(chunks, &left_map, 10, &right_map, 8);
        assert_eq!(remapped[0].start_a, 2);
        assert_eq!(remapped[0].end_a, 5);
        assert_eq!(remapped[0].start_b, 1);
        assert_eq!(remapped[0].end_b, 3);
    }

    #[test]
    fn remap_out_of_bounds_uses_total() {
        let chunks = vec![DiffChunk {
            tag: DiffTag::Delete,
            start_a: 0,
            end_a: 3, // beyond map length
            start_b: 0,
            end_b: 2, // beyond map length
        }];
        let left_map = vec![0, 1]; // len=2, so index 3 is OOB
        let right_map = vec![0]; // len=1, so index 2 is OOB
        let remapped = remap_chunks(chunks, &left_map, 10, &right_map, 5);
        assert_eq!(remapped[0].end_a, 10); // falls back to left_total
        assert_eq!(remapped[0].end_b, 5); // falls back to right_total
    }

    #[test]
    fn remap_empty_chunks() {
        let remapped = remap_chunks(vec![], &[0, 1], 2, &[0, 1], 2);
        assert!(remapped.is_empty());
    }

    // ── format_size ──────────────────────────────────────────────

    #[test]
    fn format_size_bytes() {
        assert_eq!(format_size(0), "0 B");
        assert_eq!(format_size(999), "999 B");
    }

    #[test]
    fn format_size_kilobytes() {
        assert_eq!(format_size(1000), "1.0 kB");
        assert_eq!(format_size(1500), "1.5 kB");
        assert_eq!(format_size(999_999), "1000.0 kB");
    }

    #[test]
    fn format_size_megabytes() {
        assert_eq!(format_size(1_000_000), "1.0 MB");
        assert_eq!(format_size(5_500_000), "5.5 MB");
    }

    #[test]
    fn format_size_gigabytes() {
        assert_eq!(format_size(1_000_000_000), "1.0 GB");
        assert_eq!(format_size(2_500_000_000), "2.5 GB");
    }

    // ── count_changes ────────────────────────────────────────────

    #[test]
    fn count_changes_empty() {
        assert_eq!(count_changes(&[]), 0);
    }

    #[test]
    fn count_changes_all_equal() {
        let chunks = vec![DiffChunk {
            tag: DiffTag::Equal,
            start_a: 0,
            end_a: 5,
            start_b: 0,
            end_b: 5,
        }];
        assert_eq!(count_changes(&chunks), 0);
    }

    #[test]
    fn count_changes_mixed() {
        let chunks = vec![
            DiffChunk {
                tag: DiffTag::Equal,
                start_a: 0,
                end_a: 2,
                start_b: 0,
                end_b: 2,
            },
            DiffChunk {
                tag: DiffTag::Replace,
                start_a: 2,
                end_a: 4,
                start_b: 2,
                end_b: 4,
            },
            DiffChunk {
                tag: DiffTag::Equal,
                start_a: 4,
                end_a: 6,
                start_b: 4,
                end_b: 6,
            },
            DiffChunk {
                tag: DiffTag::Delete,
                start_a: 6,
                end_a: 8,
                start_b: 6,
                end_b: 6,
            },
            DiffChunk {
                tag: DiffTag::Insert,
                start_a: 8,
                end_a: 8,
                start_b: 6,
                end_b: 8,
            },
        ];
        assert_eq!(count_changes(&chunks), 3);
    }
}
