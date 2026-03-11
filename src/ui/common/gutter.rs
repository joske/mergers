#[allow(clippy::wildcard_imports)]
use super::*;

/// Y coordinate of `line` in the gutter widget's coordinate space.
pub fn line_to_gutter_y(
    tv: &TextView,
    buf: &TextBuffer,
    line: usize,
    scroll: &ScrolledWindow,
    gutter: &impl IsA<gtk4::Widget>,
) -> f64 {
    let y_buf = if let Some(iter) = buf.iter_at_line(line as i32) {
        tv.line_yrange(&iter).0 as f64
    } else {
        let iter = buf.end_iter();
        let (y, h) = tv.line_yrange(&iter);
        (y + h) as f64
    };
    let visible_y = y_buf - scroll.vadjustment().value();
    let point = gtk4::graphene::Point::new(0.0, visible_y as f32);
    if let Some(out) = scroll.compute_point(gutter, &point) {
        return out.y() as f64;
    }
    0.0
}

#[allow(clippy::too_many_arguments)]
/// Which copy arrows to draw on a gutter.
pub enum GutterArrows {
    /// Both left→right (→) and right→left (←).
    Both,
    /// Only the left→right (→) arrow.
    LeftToRight,
    /// Only the right→left (←) arrow.
    RightToLeft,
}

#[allow(clippy::too_many_arguments)]
fn draw_gutter_band(
    cr: &gtk4::cairo::Context,
    width: f64,
    lt: f64,
    lb: f64,
    rt: f64,
    rb: f64,
    r: f64,
    g: f64,
    b: f64,
    arrows: &GutterArrows,
) {
    cr.set_source_rgba(r, g, b, 0.3);
    cr.move_to(0.0, lt);
    cr.curve_to(width * 0.5, lt, width * 0.5, rt, width, rt);
    cr.line_to(width, rb);
    cr.curve_to(width * 0.5, rb, width * 0.5, lb, 0.0, lb);
    cr.close_path();
    let _ = cr.fill();

    let left_mid = f64::midpoint(lt, lb);
    let right_mid = f64::midpoint(rt, rb);
    if matches!(arrows, GutterArrows::Both | GutterArrows::LeftToRight) {
        draw_edge_arrow(cr, 2.0, left_mid, true, r, g, b);
    }
    if matches!(arrows, GutterArrows::Both | GutterArrows::RightToLeft) {
        draw_edge_arrow(cr, width - 2.0, right_mid, false, r, g, b);
    }
}

#[allow(clippy::too_many_arguments)]
pub fn draw_gutter(
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
    arrows: &GutterArrows,
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
            DiffTag::Replace => band_replace(),
            DiffTag::Delete | DiffTag::Insert => band_insert(),
            DiffTag::Equal => continue,
        };

        draw_gutter_band(cr, width, lt, lb, rt, rb, r, g, b, arrows);
    }
}

/// Compute merged conflict regions on the middle pane.
///
/// Returns a sorted, non-overlapping list of `(start, end)` half-open line
/// ranges on the middle pane where left and right changes overlap.
#[must_use]
pub fn middle_conflict_regions(
    left_chunks: &[DiffChunk],
    right_chunks: &[DiffChunk],
) -> Vec<(usize, usize)> {
    // Pre-collect non-Equal chunks projected onto the middle pane.
    // Left chunks use (start_b, end_b), right chunks use (start_a, end_a).
    // Both are already sorted by position since they come from diff output.
    let lefts: Vec<(usize, usize)> = left_chunks
        .iter()
        .filter(|c| c.tag != DiffTag::Equal)
        .map(|c| (c.start_b, c.end_b))
        .collect();
    let rights: Vec<(usize, usize)> = right_chunks
        .iter()
        .filter(|c| c.tag != DiffTag::Equal)
        .map(|c| (c.start_a, c.end_a))
        .collect();

    let mut merged: Vec<(usize, usize)> = Vec::new();

    // Two-pointer sweep: for each left chunk, find all overlapping right chunks.
    // `rj` tracks where to start scanning in `rights` for the current left chunk.
    let mut rj: usize = 0;
    for &(ls, le) in &lefts {
        // Advance rj past right chunks that cannot overlap this left chunk.
        // A right chunk (rs, re) is entirely before (ls, le) when:
        //   - for non-zero-width right: re <= ls (and not a zero-width touch)
        //   - for zero-width right at rs: rs < ls
        // We use chunks_overlap to be precise, but we can safely skip right
        // chunks whose end is strictly before the left start (for non-zero-width)
        // or whose position is strictly before the left start (for zero-width).
        while rj < rights.len() {
            let (rs, re) = rights[rj];
            if re < ls || (re == ls && !chunks_overlap(ls, le, rs, re)) {
                rj += 1;
            } else {
                break;
            }
        }

        // Scan forward from rj collecting all overlapping right chunks.
        let mut rk = rj;
        while rk < rights.len() {
            let (rs, re) = rights[rk];
            if !chunks_overlap(ls, le, rs, re) {
                // Since rights are sorted, once we pass the left chunk's end
                // no further right chunks can overlap.
                // For non-zero-width left: rs >= le means done.
                // For zero-width left: rs > ls means done.
                if (ls < le && rs >= le) || (ls == le && rs > ls) {
                    break;
                }
                rk += 1;
                continue;
            }
            let region_start = ls.min(rs);
            let region_end = le.max(re);
            // Skip zero-width regions (e.g. delete-at-same-line as insert).
            if region_start == region_end {
                rk += 1;
                continue;
            }
            // Merge into the output list.
            if let Some(last) = merged.last_mut()
                && region_start <= last.1
            {
                last.1 = last.1.max(region_end);
            } else {
                merged.push((region_start, region_end));
            }
            rk += 1;
        }
    }
    merged
}

/// Compute merged chunk list for the merge gutter, combining conflict chunks.
///
/// Returns `(chunk, is_conflict)` pairs.  Consecutive conflict chunks (including
/// equal chunks sandwiched between them) are merged into a single entry.
/// `side` is `Side::A` for the left gutter, `Side::B` for the right gutter.
///
/// Conflict detection uses the merged conflict regions on the middle pane so
/// that side-pane chunks sandwiched between two conflicting chunks (but not
/// directly overlapping the other side) are also included in the conflict band.
#[must_use]
pub fn merged_gutter_chunks(
    my_chunks: &[DiffChunk],
    other_chunks: &[DiffChunk],
    side: Side,
) -> Vec<(DiffChunk, bool)> {
    // Reconstruct left/right from my/other + side so we can compute middle
    // conflict regions the same way as the middle pane.
    let (left_chunks, right_chunks) = match side {
        Side::A => (my_chunks, other_chunks),
        Side::B => (other_chunks, my_chunks),
    };
    let mid_regions = middle_conflict_regions(left_chunks, right_chunks);

    // A non-Equal chunk is "in conflict" if its middle-pane projection
    // overlaps any merged conflict region.  Use binary search since
    // mid_regions is sorted and non-overlapping.
    let is_conflict: Vec<bool> = my_chunks
        .iter()
        .map(|mc| {
            if mc.tag == DiffTag::Equal {
                return false;
            }
            let (mid_s, mid_e) = match side {
                Side::A => (mc.start_b, mc.end_b),
                Side::B => (mc.start_a, mc.end_a),
            };
            // Find first region whose start > mid_s, then check it and predecessor.
            let idx = mid_regions.partition_point(|&(rs, _)| rs <= mid_s);
            (idx > 0
                && chunks_overlap(mid_s, mid_e, mid_regions[idx - 1].0, mid_regions[idx - 1].1))
                || (idx < mid_regions.len()
                    && chunks_overlap(mid_s, mid_e, mid_regions[idx].0, mid_regions[idx].1))
        })
        .collect();

    let mut result = Vec::new();
    let mut i = 0;
    while i < my_chunks.len() {
        if my_chunks[i].tag == DiffTag::Equal || !is_conflict[i] {
            result.push((my_chunks[i], false));
            i += 1;
            continue;
        }
        let mut merged = my_chunks[i];
        i += 1;
        while i < my_chunks.len() {
            let absorb = is_conflict[i]
                || (my_chunks[i].tag != DiffTag::Equal
                    && conflict_reachable(i + 1, my_chunks, &is_conflict, side, &mid_regions))
                || (my_chunks[i].tag == DiffTag::Equal
                    && is_conflict.get(i + 1).copied().unwrap_or(false)
                    && (chunk_mid_in_region(my_chunks[i], side, &mid_regions)
                        || ((my_chunks[i].end_a - my_chunks[i].start_a) <= 5
                            && (my_chunks[i].end_b - my_chunks[i].start_b) <= 5)));
            if absorb {
                merged.end_a = my_chunks[i].end_a;
                merged.end_b = my_chunks[i].end_b;
                i += 1;
            } else {
                break;
            }
        }
        result.push((merged, true));
    }
    result
}

/// Check whether the Equal chunk's middle-pane projection falls within a
/// conflict region.
fn chunk_mid_in_region(chunk: DiffChunk, side: Side, mid_regions: &[(usize, usize)]) -> bool {
    let (mid_s, mid_e) = match side {
        Side::A => (chunk.start_b, chunk.end_b),
        Side::B => (chunk.start_a, chunk.end_a),
    };
    mid_regions
        .iter()
        .any(|&(rs, re)| chunks_overlap(mid_s, mid_e, rs, re))
}

/// Check whether a conflict chunk is reachable from `from` without crossing a
/// large Equal gap that is outside any conflict region.
fn conflict_reachable(
    from: usize,
    chunks: &[DiffChunk],
    is_conflict: &[bool],
    side: Side,
    mid_regions: &[(usize, usize)],
) -> bool {
    for j in from..chunks.len() {
        if is_conflict[j] {
            return true;
        }
        if chunks[j].tag == DiffTag::Equal
            && !chunk_mid_in_region(chunks[j], side, mid_regions)
            && ((chunks[j].end_a - chunks[j].start_a) > 5
                || (chunks[j].end_b - chunks[j].start_b) > 5)
        {
            return false;
        }
    }
    false
}

/// Draw a merge gutter with conflict awareness.
///
/// Chunks that participate in a conflict are merged into a single band drawn
/// with the conflict colour.  Non-conflict chunks are drawn normally.
/// `side` is `Side::A` for the left gutter, `Side::B` for the right gutter.
#[allow(clippy::too_many_arguments)]
pub fn draw_merge_gutter(
    cr: &gtk4::cairo::Context,
    width: f64,
    left_tv: &TextView,
    right_tv: &TextView,
    left_buf: &TextBuffer,
    right_buf: &TextBuffer,
    left_scroll: &ScrolledWindow,
    right_scroll: &ScrolledWindow,
    gutter: &DrawingArea,
    my_chunks: &[DiffChunk],
    other_chunks: &[DiffChunk],
    arrows: &GutterArrows,
    side: Side,
) {
    let draw_list = merged_gutter_chunks(my_chunks, other_chunks, side);

    // Draw each band
    for (chunk, conflict) in &draw_list {
        if chunk.tag == DiffTag::Equal {
            continue;
        }

        let lt = line_to_gutter_y(left_tv, left_buf, chunk.start_a, left_scroll, gutter);
        let lb = line_to_gutter_y(left_tv, left_buf, chunk.end_a, left_scroll, gutter);
        let rt = line_to_gutter_y(right_tv, right_buf, chunk.start_b, right_scroll, gutter);
        let rb = line_to_gutter_y(right_tv, right_buf, chunk.end_b, right_scroll, gutter);

        let (r, g, b) = if *conflict {
            band_conflict()
        } else {
            match chunk.tag {
                DiffTag::Replace => band_replace(),
                DiffTag::Delete | DiffTag::Insert => band_insert(),
                DiffTag::Equal => continue,
            }
        };

        draw_gutter_band(cr, width, lt, lb, rt, rb, r, g, b, arrows);
    }
}

#[allow(clippy::many_single_char_names)]
pub fn draw_edge_arrow(
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

#[must_use]
pub fn get_lines_text(buf: &TextBuffer, start_line: usize, end_line: usize) -> String {
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

pub fn apply_paste_highlight(buf: &TextBuffer, start_line: usize, end_line: usize) {
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

pub fn copy_chunk(
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

/// Delete a chunk's text from the buffer (Meld's "delete change").
pub fn delete_chunk(buf: &TextBuffer, start: usize, end: usize) {
    if start >= end {
        return;
    }
    let mut start_iter = buf.iter_at_line(start as i32).unwrap_or(buf.start_iter());

    let mut end_iter = if (end as i32) < buf.line_count() {
        buf.iter_at_line(end as i32).unwrap_or(buf.end_iter())
    } else {
        // Chunk at end of buffer — also remove preceding newline
        if start > 0 {
            start_iter = buf
                .iter_at_line(start as i32 - 1)
                .unwrap_or(buf.start_iter());
            if !start_iter.ends_line() {
                start_iter.forward_to_line_end();
            }
        }
        buf.end_iter()
    };

    buf.begin_user_action();
    buf.delete(&mut start_iter, &mut end_iter);
    buf.end_user_action();
    buf.place_cursor(&start_iter);
}

pub fn refresh_diff(
    left_buf: &TextBuffer,
    right_buf: &TextBuffer,
    chunks: &Rc<RefCell<Vec<DiffChunk>>>,
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
        *ch.borrow_mut() = new_chunks;
        on_complete();
        p.set(false);
    });
}

#[allow(clippy::too_many_arguments)]
pub fn handle_gutter_click(
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

        match diff_state::hit_test_gutter_arrow(x, y, width, left_mid, right_mid) {
            Some(diff_state::GutterHit::CopyLeftToRight) => {
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
            Some(diff_state::GutterHit::CopyRightToLeft) => {
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
            None => {}
        }
    }
}
