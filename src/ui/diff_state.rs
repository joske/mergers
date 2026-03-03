use crate::myers::{DiffChunk, DiffTag};

/// Result of a gutter arrow hit-test.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum GutterHit {
    CopyLeftToRight,
    CopyRightToLeft,
}

/// A rectangle in the chunk-map sidebar for a non-Equal chunk.
#[derive(Debug, Clone, PartialEq)]
pub(super) struct ChunkMapRect {
    pub y_start: f64,
    pub height: f64,
    pub tag: DiffTag,
}

/// Which side of a diff chunk to use for line comparisons.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum Side {
    A,
    B,
}

/// Find the next or previous non-Equal chunk relative to `cursor_line`.
///
/// `direction > 0` searches forward; `direction < 0` searches backward.
/// `side` selects whether to compare `start_a` or `start_b`.
/// Wraps around if no match is found in the given direction.
/// Returns `None` if there are no non-Equal chunks.
pub(super) fn find_next_chunk(
    chunks: &[DiffChunk],
    cursor_line: usize,
    direction: i32,
    side: Side,
) -> Option<usize> {
    let non_equal: Vec<usize> = chunks
        .iter()
        .enumerate()
        .filter(|(_, c)| c.tag != DiffTag::Equal)
        .map(|(i, _)| i)
        .collect();

    if non_equal.is_empty() {
        return None;
    }

    let start_line = |i: usize| -> usize {
        if side == Side::A {
            chunks[i].start_a
        } else {
            chunks[i].start_b
        }
    };

    if direction > 0 {
        non_equal
            .iter()
            .find(|&&i| start_line(i) > cursor_line)
            .or(non_equal.first())
            .copied()
    } else {
        non_equal
            .iter()
            .rev()
            .find(|&&i| start_line(i) < cursor_line)
            .or(non_equal.last())
            .copied()
    }
}

/// Format a human-readable label describing the current chunk position.
///
/// - `"No changes"` if all chunks are Equal.
/// - `"N changes"` if `current` is `None` or points to an Equal chunk.
/// - `"Change X of Y"` if `current` is a non-Equal chunk (1-indexed).
pub(super) fn format_chunk_label(chunks: &[DiffChunk], current: Option<usize>) -> String {
    let non_equal: Vec<usize> = chunks
        .iter()
        .enumerate()
        .filter(|(_, c)| c.tag != DiffTag::Equal)
        .map(|(i, _)| i)
        .collect();

    let total = non_equal.len();
    if total == 0 {
        return "No changes".to_string();
    }

    match current {
        Some(cur) => {
            if let Some(pos) = non_equal.iter().position(|&i| i == cur) {
                format!("Change {} of {}", pos + 1, total)
            } else {
                format!("{total} changes")
            }
        }
        None => format!("{total} changes"),
    }
}

/// Case-insensitive substring search returning `(byte_start, byte_end)` pairs.
///
/// Allows overlapping matches (advances by 1 byte after each match).
/// Returns an empty vec if `needle` is empty.
#[must_use]
pub(super) fn find_all_matches(text: &str, needle: &str) -> Vec<(usize, usize)> {
    if needle.is_empty() {
        return Vec::new();
    }
    let lower_text = text.to_lowercase();
    let lower_needle = needle.to_lowercase();
    let mut results = Vec::new();
    let mut start = 0;
    while let Some(pos) = lower_text[start..].find(&lower_needle) {
        let byte_start = start + pos;
        let byte_end = byte_start + lower_needle.len();
        results.push((byte_start, byte_end));
        start = byte_start + 1;
    }
    results
}

/// Hit-test gutter arrows between two diff panes.
///
/// Left arrow is at `(2.0, left_mid)`, right arrow at `(width - 2.0, right_mid)`.
/// Hit radius is 12 pixels (checks distance squared < 144).
/// Returns `None` if no arrow is hit.
#[must_use]
pub(super) fn hit_test_gutter_arrow(
    x: f64,
    y: f64,
    width: f64,
    left_mid: f64,
    right_mid: f64,
) -> Option<GutterHit> {
    const HIT_RADIUS_SQ: f64 = 144.0; // 12^2

    let left_dx = x - 2.0;
    let left_dy = y - left_mid;
    if left_dx * left_dx + left_dy * left_dy < HIT_RADIUS_SQ {
        return Some(GutterHit::CopyLeftToRight);
    }

    let right_dx = x - (width - 2.0);
    let right_dy = y - right_mid;
    if right_dx * right_dx + right_dy * right_dy < HIT_RADIUS_SQ {
        return Some(GutterHit::CopyRightToLeft);
    }

    None
}

/// Compute chunk-map rectangles for the minimap sidebar.
///
/// Skips `Equal` chunks. Uses `start_a`/`end_a` when `is_left` is true,
/// otherwise `start_b`/`end_b`. Each rectangle has a minimum height of 2 pixels.
/// Returns empty if `total_lines == 0` or `map_height <= 0.0`.
#[must_use]
pub(super) fn compute_chunk_map_rects(
    chunks: &[DiffChunk],
    total_lines: usize,
    map_height: f64,
    is_left: bool,
) -> Vec<ChunkMapRect> {
    if total_lines == 0 || map_height <= 0.0 {
        return Vec::new();
    }
    let total = total_lines as f64;
    chunks
        .iter()
        .filter(|c| c.tag != DiffTag::Equal)
        .map(|c| {
            let (start, end) = if is_left {
                (c.start_a, c.end_a)
            } else {
                (c.start_b, c.end_b)
            };
            let y_start = (start as f64 / total) * map_height;
            let raw_height = ((end - start) as f64 / total) * map_height;
            ChunkMapRect {
                y_start,
                height: raw_height.max(2.0),
                tag: c.tag,
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::myers::{DiffChunk, DiffTag};

    fn eq(start_a: usize, end_a: usize, start_b: usize, end_b: usize) -> DiffChunk {
        DiffChunk {
            tag: DiffTag::Equal,
            start_a,
            end_a,
            start_b,
            end_b,
        }
    }

    fn rep(start_a: usize, end_a: usize, start_b: usize, end_b: usize) -> DiffChunk {
        DiffChunk {
            tag: DiffTag::Replace,
            start_a,
            end_a,
            start_b,
            end_b,
        }
    }

    fn del(start_a: usize, end_a: usize, start_b: usize, end_b: usize) -> DiffChunk {
        DiffChunk {
            tag: DiffTag::Delete,
            start_a,
            end_a,
            start_b,
            end_b,
        }
    }

    fn ins(start_a: usize, end_a: usize, start_b: usize, end_b: usize) -> DiffChunk {
        DiffChunk {
            tag: DiffTag::Insert,
            start_a,
            end_a,
            start_b,
            end_b,
        }
    }

    // ── find_next_chunk ─────────────────────────────────────────────────

    #[test]
    fn find_next_empty_chunks() {
        assert_eq!(find_next_chunk(&[], 0, 1, Side::A), None);
        assert_eq!(find_next_chunk(&[], 0, -1, Side::A), None);
    }

    #[test]
    fn find_next_all_equal() {
        let chunks = [eq(0, 5, 0, 5), eq(5, 10, 5, 10)];
        assert_eq!(find_next_chunk(&chunks, 0, 1, Side::A), None);
        assert_eq!(find_next_chunk(&chunks, 0, -1, Side::A), None);
    }

    #[test]
    fn find_next_forward() {
        let chunks = [eq(0, 3, 0, 3), rep(3, 5, 3, 6), eq(5, 10, 6, 11)];
        // Cursor at line 0, forward -> chunk index 1 (start_a=3 > 0)
        assert_eq!(find_next_chunk(&chunks, 0, 1, Side::A), Some(1));
    }

    #[test]
    fn find_next_backward() {
        let chunks = [eq(0, 3, 0, 3), rep(3, 5, 3, 6), eq(5, 10, 6, 11)];
        // Cursor at line 7, backward -> chunk index 1 (start_a=3 < 7)
        assert_eq!(find_next_chunk(&chunks, 7, -1, Side::A), Some(1));
    }

    #[test]
    fn find_next_forward_wraps() {
        let chunks = [eq(0, 3, 0, 3), rep(3, 5, 3, 6), eq(5, 10, 6, 11)];
        // Cursor at line 5 (past the change), forward -> wraps to first non-equal (index 1)
        assert_eq!(find_next_chunk(&chunks, 5, 1, Side::A), Some(1));
    }

    #[test]
    fn find_next_backward_wraps() {
        let chunks = [eq(0, 3, 0, 3), rep(3, 5, 3, 6), eq(5, 10, 6, 11)];
        // Cursor at line 2 (before the change), backward -> wraps to last non-equal (index 1)
        assert_eq!(find_next_chunk(&chunks, 2, -1, Side::A), Some(1));
    }

    #[test]
    fn find_next_side_b() {
        // Side B has different start lines
        let chunks = [eq(0, 3, 0, 5), rep(3, 5, 5, 8), eq(5, 10, 8, 13)];
        // Cursor at line 4, forward on side B -> chunk 1 (start_b=5 > 4)
        assert_eq!(find_next_chunk(&chunks, 4, 1, Side::B), Some(1));
        // Cursor at line 6, backward on side B -> chunk 1 (start_b=5 < 6)
        assert_eq!(find_next_chunk(&chunks, 6, -1, Side::B), Some(1));
    }

    #[test]
    fn find_next_multiple_changes_forward() {
        let chunks = [
            eq(0, 2, 0, 2),
            del(2, 4, 2, 2),
            eq(4, 6, 2, 4),
            ins(6, 6, 4, 7),
            eq(6, 10, 7, 11),
        ];
        // Cursor at line 3 (inside first change), forward -> second change at index 3 (start_a=6 > 3)
        assert_eq!(find_next_chunk(&chunks, 3, 1, Side::A), Some(3));
        // Cursor at line 0, forward -> first change at index 1 (start_a=2 > 0)
        assert_eq!(find_next_chunk(&chunks, 0, 1, Side::A), Some(1));
    }

    #[test]
    fn find_next_multiple_changes_backward() {
        let chunks = [
            eq(0, 2, 0, 2),
            del(2, 4, 2, 2),
            eq(4, 6, 2, 4),
            ins(6, 6, 4, 7),
            eq(6, 10, 7, 11),
        ];
        // Cursor at line 8, backward -> second change at index 3 (start_a=6 < 8)
        assert_eq!(find_next_chunk(&chunks, 8, -1, Side::A), Some(3));
        // Cursor at line 5, backward -> first change at index 1 (start_a=2 < 5)
        assert_eq!(find_next_chunk(&chunks, 5, -1, Side::A), Some(1));
    }

    #[test]
    fn find_next_cursor_on_change_start() {
        let chunks = [eq(0, 3, 0, 3), rep(3, 5, 3, 6), eq(5, 10, 6, 11)];
        // Cursor exactly at start_a of the change (line 3), forward -> wraps (3 is NOT > 3)
        assert_eq!(find_next_chunk(&chunks, 3, 1, Side::A), Some(1));
        // Cursor exactly at start_a of the change (line 3), backward -> wraps (3 is NOT < 3)
        assert_eq!(find_next_chunk(&chunks, 3, -1, Side::A), Some(1));
    }

    // ── format_chunk_label ──────────────────────────────────────────────

    #[test]
    fn label_no_changes() {
        let chunks = [eq(0, 5, 0, 5)];
        assert_eq!(format_chunk_label(&chunks, None), "No changes");
    }

    #[test]
    fn label_no_changes_empty() {
        assert_eq!(format_chunk_label(&[], None), "No changes");
    }

    #[test]
    fn label_n_changes_no_current() {
        let chunks = [eq(0, 3, 0, 3), rep(3, 5, 3, 6), eq(5, 10, 6, 11)];
        assert_eq!(format_chunk_label(&chunks, None), "1 changes");
    }

    #[test]
    fn label_n_changes_current_is_equal() {
        let chunks = [eq(0, 3, 0, 3), rep(3, 5, 3, 6), eq(5, 10, 6, 11)];
        // Current points to an Equal chunk (index 0)
        assert_eq!(format_chunk_label(&chunks, Some(0)), "1 changes");
    }

    #[test]
    fn label_change_x_of_y() {
        let chunks = [
            eq(0, 2, 0, 2),
            rep(2, 4, 2, 5),
            eq(4, 6, 5, 7),
            del(6, 8, 7, 7),
            eq(8, 10, 7, 9),
        ];
        assert_eq!(format_chunk_label(&chunks, Some(1)), "Change 1 of 2");
        assert_eq!(format_chunk_label(&chunks, Some(3)), "Change 2 of 2");
    }

    #[test]
    fn label_invalid_current_index() {
        let chunks = [eq(0, 3, 0, 3), rep(3, 5, 3, 6), eq(5, 10, 6, 11)];
        // Index 99 doesn't exist in the non-equal list
        assert_eq!(format_chunk_label(&chunks, Some(99)), "1 changes");
    }

    // ── find_all_matches ────────────────────────────────────────────

    #[test]
    fn find_matches_empty_needle() {
        assert_eq!(
            find_all_matches("hello world", ""),
            Vec::<(usize, usize)>::new()
        );
    }

    #[test]
    fn find_matches_no_match() {
        assert_eq!(
            find_all_matches("hello world", "xyz"),
            Vec::<(usize, usize)>::new()
        );
    }

    #[test]
    fn find_matches_single() {
        assert_eq!(find_all_matches("hello world", "world"), vec![(6, 11)]);
    }

    #[test]
    fn find_matches_multiple() {
        assert_eq!(find_all_matches("abcabc", "abc"), vec![(0, 3), (3, 6)]);
    }

    #[test]
    fn find_matches_case_insensitive() {
        assert_eq!(
            find_all_matches("Hello HELLO hElLo", "hello"),
            vec![(0, 5), (6, 11), (12, 17)]
        );
    }

    #[test]
    fn find_matches_overlapping() {
        assert_eq!(find_all_matches("aaa", "aa"), vec![(0, 2), (1, 3)]);
    }

    #[test]
    fn find_matches_empty_text() {
        assert_eq!(find_all_matches("", "abc"), Vec::<(usize, usize)>::new());
    }

    #[test]
    fn find_matches_needle_longer_than_text() {
        assert_eq!(find_all_matches("ab", "abcd"), Vec::<(usize, usize)>::new());
    }

    // ── hit_test_gutter_arrow ───────────────────────────────────────

    #[test]
    fn gutter_hit_left_arrow() {
        // Click very close to left arrow at (2.0, 50.0)
        assert_eq!(
            hit_test_gutter_arrow(3.0, 50.0, 100.0, 50.0, 80.0),
            Some(GutterHit::CopyLeftToRight)
        );
    }

    #[test]
    fn gutter_hit_right_arrow() {
        // Click very close to right arrow at (98.0, 80.0) for width=100
        assert_eq!(
            hit_test_gutter_arrow(97.0, 80.0, 100.0, 50.0, 80.0),
            Some(GutterHit::CopyRightToLeft)
        );
    }

    #[test]
    fn gutter_miss_center() {
        // Click in the middle, far from both arrows
        assert_eq!(hit_test_gutter_arrow(50.0, 50.0, 100.0, 50.0, 50.0), None);
    }

    #[test]
    fn gutter_miss_outside_radius() {
        // Click 13 pixels away from left arrow (> 12 radius)
        assert_eq!(hit_test_gutter_arrow(15.0, 50.0, 100.0, 50.0, 80.0), None);
    }

    #[test]
    fn gutter_hit_exactly_at_boundary() {
        // Distance exactly 12 should miss (strict less-than)
        // Left arrow at (2.0, 50.0), click at (14.0, 50.0) -> distance = 12.0
        assert_eq!(hit_test_gutter_arrow(14.0, 50.0, 100.0, 50.0, 80.0), None);
    }

    #[test]
    fn gutter_hit_just_inside_radius() {
        // Left arrow at (2.0, 50.0), click at (13.9, 50.0) -> distance = 11.9 < 12
        assert_eq!(
            hit_test_gutter_arrow(13.9, 50.0, 100.0, 50.0, 80.0),
            Some(GutterHit::CopyLeftToRight)
        );
    }

    // ── compute_chunk_map_rects ─────────────────────────────────────

    #[test]
    fn chunk_map_empty_chunks() {
        assert_eq!(
            compute_chunk_map_rects(&[], 100, 500.0, true),
            Vec::<ChunkMapRect>::new()
        );
    }

    #[test]
    fn chunk_map_zero_total_lines() {
        let chunks = [rep(0, 5, 0, 3)];
        assert_eq!(
            compute_chunk_map_rects(&chunks, 0, 500.0, true),
            Vec::<ChunkMapRect>::new()
        );
    }

    #[test]
    fn chunk_map_zero_map_height() {
        let chunks = [rep(0, 5, 0, 3)];
        assert_eq!(
            compute_chunk_map_rects(&chunks, 100, 0.0, true),
            Vec::<ChunkMapRect>::new()
        );
    }

    #[test]
    fn chunk_map_negative_map_height() {
        let chunks = [rep(0, 5, 0, 3)];
        assert_eq!(
            compute_chunk_map_rects(&chunks, 100, -10.0, true),
            Vec::<ChunkMapRect>::new()
        );
    }

    #[test]
    fn chunk_map_skips_equal() {
        let chunks = [eq(0, 10, 0, 10)];
        assert_eq!(
            compute_chunk_map_rects(&chunks, 10, 100.0, true),
            Vec::<ChunkMapRect>::new()
        );
    }

    #[test]
    fn chunk_map_basic_left() {
        // 100 total lines, 1000px height, replace chunk lines 10..20 on side A
        let chunks = [eq(0, 10, 0, 10), rep(10, 20, 10, 15), eq(20, 100, 15, 85)];
        let rects = compute_chunk_map_rects(&chunks, 100, 1000.0, true);
        assert_eq!(rects.len(), 1);
        assert!((rects[0].y_start - 100.0).abs() < f64::EPSILON);
        assert!((rects[0].height - 100.0).abs() < f64::EPSILON);
        assert_eq!(rects[0].tag, DiffTag::Replace);
    }

    #[test]
    fn chunk_map_basic_right() {
        // Same chunks, but using side B (start_b=10, end_b=15)
        let chunks = [eq(0, 10, 0, 10), rep(10, 20, 10, 15), eq(20, 100, 15, 85)];
        let rects = compute_chunk_map_rects(&chunks, 85, 850.0, false);
        assert_eq!(rects.len(), 1);
        // y_start = (10/85)*850 = 100.0
        assert!((rects[0].y_start - 100.0).abs() < f64::EPSILON);
        // height = (5/85)*850 = 50.0
        assert!((rects[0].height - 50.0).abs() < f64::EPSILON);
        assert_eq!(rects[0].tag, DiffTag::Replace);
    }

    #[test]
    fn chunk_map_minimum_height_clamp() {
        // 1 line out of 10000 at 100px -> raw height = 0.01, clamped to 2.0
        let chunks = [rep(500, 501, 500, 501)];
        let rects = compute_chunk_map_rects(&chunks, 10000, 100.0, true);
        assert_eq!(rects.len(), 1);
        assert!((rects[0].height - 2.0).abs() < f64::EPSILON);
    }

    #[test]
    fn chunk_map_multiple_non_equal() {
        let chunks = [
            eq(0, 5, 0, 5),
            del(5, 8, 5, 5),
            eq(8, 15, 5, 12),
            ins(15, 15, 12, 14),
            eq(15, 20, 14, 19),
        ];
        let rects = compute_chunk_map_rects(&chunks, 20, 200.0, true);
        assert_eq!(rects.len(), 2);
        assert_eq!(rects[0].tag, DiffTag::Delete);
        assert_eq!(rects[1].tag, DiffTag::Insert);
        // Delete: y_start = (5/20)*200 = 50, height = (3/20)*200 = 30
        assert!((rects[0].y_start - 50.0).abs() < f64::EPSILON);
        assert!((rects[0].height - 30.0).abs() < f64::EPSILON);
        // Insert: start_a=15, end_a=15, so 0 lines on left -> height clamped to 2.0
        assert!((rects[1].y_start - 150.0).abs() < f64::EPSILON);
        assert!((rects[1].height - 2.0).abs() < f64::EPSILON);
    }

    mod proptests {
        use super::*;
        use proptest::prelude::*;

        fn arb_tag() -> impl Strategy<Value = DiffTag> {
            prop_oneof![
                Just(DiffTag::Equal),
                Just(DiffTag::Replace),
                Just(DiffTag::Insert),
                Just(DiffTag::Delete),
            ]
        }

        fn arb_chunks() -> impl Strategy<Value = Vec<DiffChunk>> {
            prop::collection::vec(
                (
                    arb_tag(),
                    0..1000_usize,
                    0..1000_usize,
                    0..1000_usize,
                    0..1000_usize,
                )
                    .prop_map(|(tag, a, b, c, d)| DiffChunk {
                        tag,
                        start_a: a.min(b),
                        end_a: a.max(b),
                        start_b: c.min(d),
                        end_b: c.max(d),
                    }),
                1..20,
            )
        }

        proptest! {
            #[test]
            fn nav_forward_returns_valid_non_equal(
                chunks in arb_chunks(),
                cursor in 0..2000_usize,
            ) {
                if let Some(idx) = find_next_chunk(&chunks, cursor, 1, Side::A) {
                    prop_assert!(idx < chunks.len());
                    prop_assert_ne!(chunks[idx].tag, DiffTag::Equal);
                }
            }

            #[test]
            fn nav_backward_returns_valid_non_equal(
                chunks in arb_chunks(),
                cursor in 0..2000_usize,
            ) {
                if let Some(idx) = find_next_chunk(&chunks, cursor, -1, Side::A) {
                    prop_assert!(idx < chunks.len());
                    prop_assert_ne!(chunks[idx].tag, DiffTag::Equal);
                }
            }

            #[test]
            fn nav_returns_none_iff_all_equal(
                chunks in arb_chunks(),
                cursor in 0..2000_usize,
            ) {
                let has_non_equal = chunks.iter().any(|c| c.tag != DiffTag::Equal);
                let result = find_next_chunk(&chunks, cursor, 1, Side::A);
                prop_assert_eq!(result.is_some(), has_non_equal);
            }
        }
    }
}
