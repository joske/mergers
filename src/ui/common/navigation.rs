#[allow(clippy::wildcard_imports)]
use super::*;

pub fn count_changes(chunks: &[DiffChunk]) -> usize {
    chunks.iter().filter(|c| c.tag != DiffTag::Equal).count()
}

#[allow(clippy::too_many_arguments)]
pub fn navigate_chunk(
    chunks: &[DiffChunk],
    current_chunk: &Rc<Cell<Option<usize>>>,
    direction: i32, // -1 = prev, +1 = next
    left_tv: &TextView,
    left_buf: &TextBuffer,
    left_scroll: &ScrolledWindow,
    right_tv: &TextView,
    right_buf: &TextBuffer,
    right_scroll: &ScrolledWindow,
    active_tv: &TextView,
    wrap: bool,
) {
    let cursor_line = cursor_line_from_view(active_tv);
    let side = if active_tv == right_tv {
        Side::B
    } else {
        Side::A
    };

    if let Some(idx) = diff_state::find_next_chunk(chunks, cursor_line, direction, side, wrap) {
        let chunk = &chunks[idx];
        scroll_to_line(left_tv, left_buf, chunk.start_a, left_scroll);
        scroll_to_line(right_tv, right_buf, chunk.start_b, right_scroll);
        place_cursor_at_line(left_buf, chunk.start_a);
        place_cursor_at_line(right_buf, chunk.start_b);
        // Set current_chunk AFTER placing cursors, because cursor_position_notify
        // handlers may overwrite it (e.g. for Insert chunks with zero-length ranges).
        current_chunk.set(Some(idx));
    }
}

/// Derive the `ScrolledWindow` ancestor of a `TextView`, falling back to the given default.
pub fn scroll_for_view(tv: &TextView, fallback: &ScrolledWindow) -> ScrolledWindow {
    tv.ancestor(ScrolledWindow::static_type())
        .and_then(|w| w.downcast::<ScrolledWindow>().ok())
        .unwrap_or_else(|| fallback.clone())
}

/// Get the cursor line number from a `TextView`.
pub fn cursor_line_from_view(tv: &TextView) -> usize {
    let buf = tv.buffer();
    let mark = buf.get_insert();
    let iter = buf.iter_at_mark(&mark);
    iter.line() as usize
}

/// Place the cursor at the beginning of `line` in the given buffer.
pub fn place_cursor_at_line(buf: &TextBuffer, line: usize) {
    if let Some(iter) = buf.iter_at_line(line as i32) {
        buf.place_cursor(&iter);
    }
}

pub fn scroll_to_line(tv: &TextView, buf: &TextBuffer, line: usize, scroll: &ScrolledWindow) {
    if let Some(iter) = buf.iter_at_line(line as i32) {
        let (y, h) = tv.line_yrange(&iter);
        let visible_h = scroll.vadjustment().page_size();
        // Center the line in the visible area
        let target = (y as f64 + h as f64 / 2.0) - visible_h / 2.0;
        scroll.vadjustment().set_value(target.max(0.0));
    }
}

pub fn update_chunk_label(label: &Label, chunks: &[DiffChunk], current: Option<usize>) {
    label.set_label(&diff_state::format_chunk_label(chunks, current));
}

/// Update sensitivity of prev/next chunk nav buttons based on cursor position.
pub fn update_chunk_nav_sensitivity(
    prev_btn: &Button,
    next_btn: &Button,
    chunks: &[DiffChunk],
    active_tv: &TextView,
    right_tv: &TextView,
    wrap: bool,
) {
    let cursor_line = cursor_line_from_view(active_tv);
    let side = if active_tv == right_tv {
        Side::B
    } else {
        Side::A
    };
    let (prev, next) = diff_state::chunk_nav_sensitivity(chunks, cursor_line, side, wrap);
    prev_btn.set_sensitive(prev);
    next_btn.set_sensitive(next);
}
