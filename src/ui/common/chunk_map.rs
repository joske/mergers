#[allow(clippy::wildcard_imports)]
use super::*;

pub fn draw_chunk_map(
    area: &DrawingArea,
    cr: &gtk4::cairo::Context,
    height: f64,
    total_lines: i32,
    scroll: &ScrolledWindow,
    chunks: &[DiffChunk],
    side: Side,
) {
    if total_lines <= 0 || height <= 0.0 {
        return;
    }

    for rect in &diff_state::compute_chunk_map_rects(chunks, total_lines as usize, height, side)
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
        let fg = area.color();
        let fg_lum = 0.299 * f64::from(fg.red())
            + 0.587 * f64::from(fg.green())
            + 0.114 * f64::from(fg.blue());
        let dark = fg_lum > 0.5;
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

pub fn create_chunk_map(
    buf: &TextBuffer,
    scroll: &ScrolledWindow,
    chunks: &Rc<RefCell<Vec<DiffChunk>>>,
    side: Side,
) -> DrawingArea {
    let map = DrawingArea::new();
    map.set_content_width(12);
    map.set_vexpand(true);
    {
        let buf = buf.clone();
        let scroll = scroll.clone();
        let chunks = chunks.clone();
        map.set_draw_func(move |area, cr, _w, h| {
            draw_chunk_map(area, cr, h as f64, buf.line_count(), &scroll, &chunks.borrow(), side);
        });
    }
    {
        let gesture = GestureClick::new();
        let scroll = scroll.clone();
        let m = map.clone();
        gesture.connect_pressed(move |_, _, _x, y| {
            let h = m.height() as f64;
            if h > 0.0 {
                let adj = scroll.vadjustment();
                let target = (y / h) * adj.upper() - adj.page_size() / 2.0;
                adj.set_value(target.max(0.0));
            }
        });
        map.add_controller(gesture);
    }
    map
}
