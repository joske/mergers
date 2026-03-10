use crate::myers::{DiffChunk, DiffTag};

/// Collect non-Equal chunks from both diffs, sorted by middle-file line position.
/// Returns (`chunk_index`, `is_right_diff`) pairs.
///
/// In the left diff, A = left file and B = middle file, so `start_b` gives the
/// middle-file line.  In the right diff, A = middle file and B = right file, so
/// `start_a` gives the middle-file line.
pub fn merge_change_indices(
    left_chunks: &[DiffChunk],
    right_chunks: &[DiffChunk],
) -> Vec<(usize, bool)> {
    let mut indices: Vec<(usize, bool, usize)> = Vec::new();
    for (i, c) in left_chunks.iter().enumerate() {
        if c.tag != DiffTag::Equal {
            indices.push((i, false, c.start_b));
        }
    }
    for (i, c) in right_chunks.iter().enumerate() {
        if c.tag != DiffTag::Equal {
            indices.push((i, true, c.start_a));
        }
    }
    indices.sort_by_key(|&(_, _, line)| line);
    indices
        .into_iter()
        .map(|(i, is_right, _)| (i, is_right))
        .collect()
}

/// Find 0-indexed line numbers of `<<<<<<<` conflict markers in the given text.
///
/// A line is considered a conflict marker only if it *starts with* `<<<<<<<`.
pub fn find_conflict_markers_in_text(text: &str) -> Vec<usize> {
    text.lines()
        .enumerate()
        .filter_map(|(i, line)| {
            if line.starts_with("<<<<<<<") {
                Some(i)
            } else {
                None
            }
        })
        .collect()
}

/// Precompute conflict block ranges: `(open_line, close_line)` pairs (both inclusive).
///
/// Scans the text once and returns a sorted `Vec` of `(<<<<<<<` line, `>>>>>>>` line)` pairs.
pub fn find_conflict_blocks(text: &str) -> Vec<(usize, usize)> {
    let mut blocks = Vec::new();
    let mut open: Option<usize> = None;
    for (i, line) in text.lines().enumerate() {
        if line.starts_with("<<<<<<<") {
            open = Some(i);
        } else if line.starts_with(">>>>>>>") {
            if let Some(start) = open.take() {
                blocks.push((start, i));
            }
        }
    }
    blocks
}

/// Binary search for which precomputed conflict block contains `cursor_line`.
///
/// Returns `Some(open_line)` if `cursor_line` falls within a block (inclusive of
/// both the `<<<<<<<` and `>>>>>>>` lines), or `None` otherwise.
pub fn conflict_at_cursor_fast(blocks: &[(usize, usize)], cursor_line: usize) -> Option<usize> {
    // Find the first block whose open_line > cursor_line; the candidate is one before that.
    let idx = blocks.partition_point(|&(open, _)| open <= cursor_line);
    if idx == 0 {
        return None;
    }
    let (open, close) = blocks[idx - 1];
    if cursor_line <= close {
        Some(open)
    } else {
        None
    }
}

/// Find the `<<<<<<<` marker line for the conflict block enclosing `cursor_line`, if any.
///
/// A conflict block spans from a `<<<<<<<` line to a `>>>>>>>` line (inclusive).
/// Returns the line number of the opening `<<<<<<<` marker, which is the value
/// stored in `current_conflict`.
///
/// For repeated calls on the same text, prefer precomputing blocks with
/// [`find_conflict_blocks`] and using [`conflict_at_cursor_fast`] directly.
pub fn conflict_at_cursor(text: &str, cursor_line: usize) -> Option<usize> {
    let blocks = find_conflict_blocks(text);
    conflict_at_cursor_fast(&blocks, cursor_line)
}

#[cfg(test)]
mod tests {
    use super::*;

    // ── merge_change_indices ─────────────────────────────────────

    #[test]
    fn change_indices_empty() {
        let result = merge_change_indices(&[], &[]);
        assert!(result.is_empty());
    }

    #[test]
    fn change_indices_only_equal() {
        let left = vec![DiffChunk {
            tag: DiffTag::Equal,
            start_a: 0,
            end_a: 5,
            start_b: 0,
            end_b: 5,
        }];
        let right = left.clone();
        let result = merge_change_indices(&left, &right);
        assert!(result.is_empty());
    }

    #[test]
    fn change_indices_left_only() {
        let left = vec![
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
        ];
        let result = merge_change_indices(&left, &[]);
        assert_eq!(result, vec![(1, false)]);
    }

    #[test]
    fn change_indices_right_only() {
        let right = vec![
            DiffChunk {
                tag: DiffTag::Equal,
                start_a: 0,
                end_a: 3,
                start_b: 0,
                end_b: 3,
            },
            DiffChunk {
                tag: DiffTag::Delete,
                start_a: 3,
                end_a: 5,
                start_b: 3,
                end_b: 3,
            },
        ];
        let result = merge_change_indices(&[], &right);
        assert_eq!(result, vec![(1, true)]);
    }

    #[test]
    fn change_indices_sorted_by_middle_line() {
        let left = vec![DiffChunk {
            tag: DiffTag::Replace,
            start_a: 0,
            end_a: 2,
            start_b: 10, // middle line 10
            end_b: 12,
        }];
        let right = vec![DiffChunk {
            tag: DiffTag::Insert,
            start_a: 5, // middle line 5
            end_a: 5,
            start_b: 0,
            end_b: 2,
        }];
        let result = merge_change_indices(&left, &right);
        assert_eq!(result, vec![(0, true), (0, false)]);
    }

    #[test]
    fn change_indices_mixed_tags() {
        let left = vec![
            DiffChunk {
                tag: DiffTag::Insert,
                start_a: 0,
                end_a: 0,
                start_b: 0, // middle line 0
                end_b: 2,
            },
            DiffChunk {
                tag: DiffTag::Equal,
                start_a: 0,
                end_a: 5,
                start_b: 2,
                end_b: 7,
            },
            DiffChunk {
                tag: DiffTag::Delete,
                start_a: 5,
                end_a: 8,
                start_b: 7, // middle line 7
                end_b: 7,
            },
        ];
        let right = vec![DiffChunk {
            tag: DiffTag::Replace,
            start_a: 3, // middle line 3
            end_a: 5,
            start_b: 3,
            end_b: 6,
        }];
        let result = merge_change_indices(&left, &right);
        // Sorted: left[0] middle=0, right[0] middle=3, left[2] middle=7
        assert_eq!(result, vec![(0, false), (0, true), (2, false)]);
    }

    // ── find_conflict_markers_in_text ────────────────────────────

    #[test]
    fn conflict_markers_empty_text() {
        let result = find_conflict_markers_in_text("");
        assert!(result.is_empty());
    }

    #[test]
    fn conflict_markers_none_present() {
        let text = "line one\nline two\nline three\n";
        let result = find_conflict_markers_in_text(text);
        assert!(result.is_empty());
    }

    #[test]
    fn conflict_markers_single() {
        let text =
            "before\n<<<<<<< HEAD\nconflict content\n=======\nother side\n>>>>>>> branch\nafter\n";
        let result = find_conflict_markers_in_text(text);
        assert_eq!(result, vec![1]);
    }

    #[test]
    fn conflict_markers_multiple() {
        let text = "<<<<<<< HEAD\nfoo\n=======\nbar\n>>>>>>> b\n<<<<<<< HEAD\nbaz\n=======\nqux\n>>>>>>> b\n";
        let result = find_conflict_markers_in_text(text);
        assert_eq!(result, vec![0, 5]);
    }

    #[test]
    fn conflict_markers_not_at_line_start() {
        let text = "some text <<<<<<< HEAD\nanother line\n";
        let result = find_conflict_markers_in_text(text);
        assert!(result.is_empty());
    }

    #[test]
    fn conflict_markers_bare_marker() {
        // Exactly seven angle brackets with nothing after
        let text = "hello\n<<<<<<<\nworld\n";
        let result = find_conflict_markers_in_text(text);
        assert_eq!(result, vec![1]);
    }

    // ── conflict_at_cursor ───────────────────────────────────────

    #[test]
    fn conflict_at_cursor_outside() {
        let text = "before\n<<<<<<< HEAD\nours\n=======\ntheirs\n>>>>>>> b\nafter\n";
        assert_eq!(conflict_at_cursor(text, 0), None); // "before"
        assert_eq!(conflict_at_cursor(text, 6), None); // "after"
    }

    #[test]
    fn conflict_at_cursor_on_open_marker() {
        let text = "before\n<<<<<<< HEAD\nours\n=======\ntheirs\n>>>>>>> b\nafter\n";
        assert_eq!(conflict_at_cursor(text, 1), Some(1));
    }

    #[test]
    fn conflict_at_cursor_inside() {
        let text = "before\n<<<<<<< HEAD\nours\n=======\ntheirs\n>>>>>>> b\nafter\n";
        assert_eq!(conflict_at_cursor(text, 2), Some(1)); // "ours"
        assert_eq!(conflict_at_cursor(text, 3), Some(1)); // "======="
        assert_eq!(conflict_at_cursor(text, 4), Some(1)); // "theirs"
    }

    #[test]
    fn conflict_at_cursor_on_close_marker() {
        let text = "before\n<<<<<<< HEAD\nours\n=======\ntheirs\n>>>>>>> b\nafter\n";
        assert_eq!(conflict_at_cursor(text, 5), Some(1)); // ">>>>>>> b"
    }

    #[test]
    fn conflict_at_cursor_multiple_blocks() {
        let text = "<<<<<<< HEAD\na\n>>>>>>> b\nmiddle\n<<<<<<< HEAD\nb\n>>>>>>> b\n";
        assert_eq!(conflict_at_cursor(text, 0), Some(0)); // first block open
        assert_eq!(conflict_at_cursor(text, 1), Some(0)); // first block body
        assert_eq!(conflict_at_cursor(text, 3), None); // between blocks
        assert_eq!(conflict_at_cursor(text, 4), Some(4)); // second block open
        assert_eq!(conflict_at_cursor(text, 5), Some(4)); // second block body
    }

    #[test]
    fn conflict_at_cursor_past_end() {
        let text = "hello\nworld\n";
        assert_eq!(conflict_at_cursor(text, 99), None);
    }

    #[test]
    fn conflict_at_cursor_empty() {
        assert_eq!(conflict_at_cursor("", 0), None);
    }
}
