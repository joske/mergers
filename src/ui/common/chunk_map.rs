#[allow(clippy::wildcard_imports)]
use super::*;

pub fn draw_chunk_map(
    cr: &gtk4::cairo::Context,
    height: f64,
    total_lines: i32,
    scroll: &ScrolledWindow,
    chunks: &[DiffChunk],
    is_left: bool,
) {
    if total_lines <= 0 || height <= 0.0 {
        return;
    }

    for rect in &diff_state::compute_chunk_map_rects(chunks, total_lines as usize, height, is_left)
    {
        let (r, g, b) = match rect.tag {
            DiffTag::Replace => band_replace(),
            DiffTag::Delete | DiffTag::Insert => band_insert(),
            DiffTag::Equal => continue,
        };
        cr.set_source_rgba(r, g, b, 0.7);
        cr.rectangle(1.0, rect.y_start, 10.0, rect.height);
        let _ = cr.fill();
    }

    // Viewport indicator — darkish overlay in light mode, lightish in dark mode
    let adj = scroll.vadjustment();
    if adj.upper() > 0.0 {
        let view_start = (adj.value() / adj.upper()) * height;
        let view_h = (adj.page_size() / adj.upper()) * height;
        let dark = is_dark_scheme();
        let v = if dark { 1.0 } else { 0.0 };
        let fill_alpha = if dark { 0.20 } else { 0.25 };
        let border_alpha = if dark { 0.45 } else { 0.55 };
        cr.set_source_rgba(v, v, v, fill_alpha);
        cr.rectangle(0.0, view_start, 12.0, view_h);
        let _ = cr.fill();
        cr.set_source_rgba(v, v, v, border_alpha);
        cr.rectangle(0.0, view_start, 12.0, 1.0);
        let _ = cr.fill();
        cr.rectangle(0.0, view_start + view_h - 1.0, 12.0, 1.0);
        let _ = cr.fill();
    }
}
