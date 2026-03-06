#[allow(clippy::wildcard_imports)]
use super::*;

/// Calculate a sync anchor point within the visible area (0.0 = top, 0.5 = middle, 1.0 = bottom).
///
/// Normally anchors at the screen center, but scales linearly to the top/bottom
/// when near document edges so that both ends scroll into view correctly.
/// This matches meld's `calc_syncpoint` algorithm.
fn calc_syncpoint(adj: &gtk4::Adjustment) -> f64 {
    let current = adj.value();
    let half_page = adj.page_size() / 2.0;
    if half_page <= 0.0 {
        return 0.5;
    }

    let top_val = adj.lower();
    let first_scale = ((current - top_val) / half_page).min(1.0);
    let mut syncpoint = 0.5 * first_scale;

    let bottom_val = adj.upper() - 1.5 * adj.page_size();
    let last_scale = ((current - bottom_val) / half_page).max(0.0);
    syncpoint += 0.5 * last_scale;

    syncpoint
}

/// Map a line from one side of a diff to the corresponding line on the other side
/// using chunk boundaries.  Within equal regions the offset is preserved 1:1;
/// inside a change the position is interpolated proportionally.
fn map_line_through_chunks(
    chunks: &[DiffChunk],
    source_line: f64,
    master_line_count: usize,
    other_line_count: usize,
    master_is_a: bool,
) -> f64 {
    let (mut m_begin, mut o_begin): (f64, f64) = (0.0, 0.0);

    for chunk in chunks {
        let (m_start, m_end, o_start, o_end) = if master_is_a {
            (chunk.start_a, chunk.end_a, chunk.start_b, chunk.end_b)
        } else {
            (chunk.start_b, chunk.end_b, chunk.start_a, chunk.end_a)
        };
        let (ms, me, os, oe) = (m_start as f64, m_end as f64, o_start as f64, o_end as f64);

        if chunk.tag == DiffTag::Equal {
            if me >= source_line {
                // Source line is inside this equal region — 1:1 mapping.
                return os + (source_line - ms);
            }
            m_begin = me;
            o_begin = oe;
            continue;
        }

        if ms >= source_line {
            // Source line is in the equal region before this chunk.
            let m_end_eq = ms;
            let o_end_eq = os;
            let frac = if (m_end_eq - m_begin).abs() > f64::EPSILON {
                (source_line - m_begin) / (m_end_eq - m_begin)
            } else {
                0.0
            };
            return o_begin + frac * (o_end_eq - o_begin);
        }
        if me >= source_line {
            // Source line is inside this change chunk.
            let frac = if (me - ms).abs() > f64::EPSILON {
                (source_line - ms) / (me - ms)
            } else {
                0.0
            };
            return os + frac * (oe - os);
        }

        m_begin = me;
        o_begin = oe;
    }

    // Past all chunks — in the trailing equal region.
    let m_end_doc = master_line_count as f64;
    let o_end_doc = other_line_count as f64;
    let frac = if (m_end_doc - m_begin).abs() > f64::EPSILON {
        (source_line - m_begin) / (m_end_doc - m_begin)
    } else {
        0.0
    };
    o_begin + frac * (o_end_doc - o_begin)
}

/// Synchronize a target pane's vertical scroll to a master pane using
/// chunk-based line mapping (matching meld's `_sync_vscroll` algorithm).
pub fn sync_vscroll(
    master_adj: &gtk4::Adjustment,
    master_tv: &TextView,
    target_adj: &gtk4::Adjustment,
    target_tv: &TextView,
    target_buf: &TextBuffer,
    chunks: &[DiffChunk],
    master_is_a: bool,
) {
    let syncpoint = calc_syncpoint(master_adj);

    // The Y coordinate in buffer space at the sync anchor.
    let sync_y = master_adj.value() + master_adj.page_size() * syncpoint;

    // Convert to a fractional line number in the master pane.
    // Use line_at_y (not iter_at_location) — it always returns a result,
    // clamping to the last line if y is past the end.
    let (sync_iter, _line_top) = master_tv.line_at_y(sync_y as i32);
    let (line_y, height) = master_tv.line_yrange(&sync_iter);
    let h = if height == 0 { 1 } else { height };
    let target_line_master = sync_iter.line() as f64 + (sync_y - line_y as f64) / h as f64;

    let master_line_count = master_tv.buffer().line_count() as usize;
    let other_line_count = target_buf.line_count() as usize;

    let other_line = map_line_through_chunks(
        chunks,
        target_line_master,
        master_line_count,
        other_line_count,
        master_is_a,
    );

    // Convert the target line back to pixel coordinates.
    let target_line_i = (other_line.floor() as i32)
        .min(target_buf.line_count() - 1)
        .max(0);
    if let Some(it) = target_buf.iter_at_line(target_line_i) {
        let (val_y, th) = target_tv.line_yrange(&it);
        let line_frac = if it.is_end() {
            1.0
        } else {
            other_line - other_line.floor()
        };
        let mut val = val_y as f64 + line_frac * th as f64;

        // Factor in overscroll margin near the bottom.
        if syncpoint > 0.5 {
            let overscroll_scale = (syncpoint - 0.5) / 0.5;
            let overscroll_height = target_tv.bottom_margin() as f64;
            val += overscroll_height * overscroll_scale;
        }

        val -= target_adj.page_size() * syncpoint;
        val = val
            .max(target_adj.lower())
            .min(target_adj.upper() - target_adj.page_size());
        target_adj.set_value(val.floor());
    }
}

/// Wire horizontal scroll sync between N panes, sharing a syncing guard
/// to prevent re-entrant events.
fn sync_hscrolls(scrolls: &[&ScrolledWindow], syncing: &Rc<Cell<bool>>) {
    let adjs: Vec<gtk4::Adjustment> = scrolls.iter().map(|s| s.hadjustment()).collect();
    for i in 0..adjs.len() {
        let others: Vec<gtk4::Adjustment> = adjs
            .iter()
            .enumerate()
            .filter(|&(j, _)| j != i)
            .map(|(_, a)| a.clone())
            .collect();
        let s = syncing.clone();
        adjs[i].connect_value_changed(move |adj| {
            if !s.get() {
                s.set(true);
                for other in &others {
                    other.set_value(adj.value());
                }
                s.set(false);
            }
        });
    }
}

#[allow(clippy::too_many_arguments)]
pub fn setup_scroll_sync(
    left_scroll: &ScrolledWindow,
    right_scroll: &ScrolledWindow,
    left_tv: &TextView,
    right_tv: &TextView,
    left_buf: &TextBuffer,
    right_buf: &TextBuffer,
    chunks: &Rc<RefCell<Vec<DiffChunk>>>,
    gutter: &DrawingArea,
) {
    let syncing = Rc::new(Cell::new(false));

    // Vertical: Left → Right (chunk-based)
    {
        let rs = right_scroll.clone();
        let ltv = left_tv.clone();
        let rtv = right_tv.clone();
        let rb = right_buf.clone();
        let ch = chunks.clone();
        let g = gutter.clone();
        let s = syncing.clone();
        left_scroll.vadjustment().connect_value_changed(move |adj| {
            g.queue_draw();
            if !s.get() {
                s.set(true);
                sync_vscroll(adj, &ltv, &rs.vadjustment(), &rtv, &rb, &ch.borrow(), true);
                s.set(false);
            }
        });
    }

    // Vertical: Right → Left (chunk-based)
    {
        let ls = left_scroll.clone();
        let ltv = left_tv.clone();
        let rtv = right_tv.clone();
        let lb = left_buf.clone();
        let ch = chunks.clone();
        let g = gutter.clone();
        let s = syncing.clone();
        right_scroll
            .vadjustment()
            .connect_value_changed(move |adj| {
                g.queue_draw();
                if !s.get() {
                    s.set(true);
                    sync_vscroll(adj, &rtv, &ls.vadjustment(), &ltv, &lb, &ch.borrow(), false);
                    s.set(false);
                }
            });
    }

    sync_hscrolls(&[left_scroll, right_scroll], &syncing);
}

/// 3-way scroll sync using chunk-based mapping (matching meld's influence-through-middle).
///
/// `left_chunks`: left(A) vs middle(B).  `right_chunks`: middle(A) vs right(B).
#[allow(clippy::too_many_arguments)]
pub fn setup_scroll_sync_3way(
    left_scroll: &ScrolledWindow,
    middle_scroll: &ScrolledWindow,
    right_scroll: &ScrolledWindow,
    left_tv: &TextView,
    middle_tv: &TextView,
    right_tv: &TextView,
    left_buf: &TextBuffer,
    middle_buf: &TextBuffer,
    right_buf: &TextBuffer,
    left_chunks: &Rc<RefCell<Vec<DiffChunk>>>,
    right_chunks: &Rc<RefCell<Vec<DiffChunk>>>,
    left_gutter: &DrawingArea,
    right_gutter: &DrawingArea,
) {
    let syncing = Rc::new(Cell::new(false));

    // Vertical: Left → Middle (left_chunks), then Middle → Right (right_chunks)
    {
        let ms = middle_scroll.clone();
        let rs = right_scroll.clone();
        let ltv = left_tv.clone();
        let mtv = middle_tv.clone();
        let rtv = right_tv.clone();
        let mb = middle_buf.clone();
        let rb = right_buf.clone();
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let lg = left_gutter.clone();
        let rg = right_gutter.clone();
        let s = syncing.clone();
        left_scroll.vadjustment().connect_value_changed(move |adj| {
            lg.queue_draw();
            rg.queue_draw();
            if !s.get() {
                s.set(true);
                sync_vscroll(adj, &ltv, &ms.vadjustment(), &mtv, &mb, &lch.borrow(), true);
                sync_vscroll(
                    &ms.vadjustment(),
                    &mtv,
                    &rs.vadjustment(),
                    &rtv,
                    &rb,
                    &rch.borrow(),
                    true,
                );
                s.set(false);
            }
        });
    }

    // Vertical: Middle → Left (left_chunks) + Right (right_chunks)
    {
        let ls = left_scroll.clone();
        let rs = right_scroll.clone();
        let ltv = left_tv.clone();
        let mtv = middle_tv.clone();
        let rtv = right_tv.clone();
        let lb = left_buf.clone();
        let rb = right_buf.clone();
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let lg = left_gutter.clone();
        let rg = right_gutter.clone();
        let s = syncing.clone();
        middle_scroll
            .vadjustment()
            .connect_value_changed(move |adj| {
                lg.queue_draw();
                rg.queue_draw();
                if !s.get() {
                    s.set(true);
                    sync_vscroll(
                        adj,
                        &mtv,
                        &ls.vadjustment(),
                        &ltv,
                        &lb,
                        &lch.borrow(),
                        false,
                    );
                    sync_vscroll(adj, &mtv, &rs.vadjustment(), &rtv, &rb, &rch.borrow(), true);
                    s.set(false);
                }
            });
    }

    // Vertical: Right → Middle (right_chunks), then Middle → Left (left_chunks)
    {
        let ls = left_scroll.clone();
        let ms = middle_scroll.clone();
        let ltv = left_tv.clone();
        let mtv = middle_tv.clone();
        let rtv = right_tv.clone();
        let lb = left_buf.clone();
        let mb = middle_buf.clone();
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let lg = left_gutter.clone();
        let rg = right_gutter.clone();
        let s = syncing.clone();
        right_scroll
            .vadjustment()
            .connect_value_changed(move |adj| {
                lg.queue_draw();
                rg.queue_draw();
                if !s.get() {
                    s.set(true);
                    sync_vscroll(
                        adj,
                        &rtv,
                        &ms.vadjustment(),
                        &mtv,
                        &mb,
                        &rch.borrow(),
                        false,
                    );
                    sync_vscroll(
                        &ms.vadjustment(),
                        &mtv,
                        &ls.vadjustment(),
                        &ltv,
                        &lb,
                        &lch.borrow(),
                        false,
                    );
                    s.set(false);
                }
            });
    }

    sync_hscrolls(&[left_scroll, middle_scroll, right_scroll], &syncing);
}
