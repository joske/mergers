use crate::myers::{DiffChunk, DiffTag};

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
}
