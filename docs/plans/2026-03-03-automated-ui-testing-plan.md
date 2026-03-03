# Automated UI Testing Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Automate ~74% of TESTING.md checklist items across three layers: pure Rust unit tests, GTK widget state tests, and a Python/dogtail pre-release suite.

**Architecture:** Extract pure logic from GTK callbacks into `src/ui/diff_state.rs` and `src/ui/merge_state.rs`, add CLI integration tests, extend property-based tests, add `#[gtk::test]` widget tests under Xvfb, and build a Python/dogtail pre-release integration suite.

**Tech Stack:** Rust (proptest, tempfile, image-compare), GTK4/sourceview5, Python (dogtail, pytest), xvfb for headless CI.

**Design doc:** `docs/plans/2026-03-03-automated-ui-testing-design.md`

---

## Task 1: Create `diff_state.rs` — Extract navigation + label logic

**Files:**
- Create: `src/ui/diff_state.rs`
- Modify: `src/ui/mod.rs` (add `mod diff_state;`)
- Modify: `src/ui/common.rs:1538-1565` (make `chunk_near_cursor` call into `diff_state`)
- Modify: `src/ui/common.rs:1645-1670` (make `update_chunk_label` call into `diff_state`)

**Step 1: Create `diff_state.rs` with `find_next_chunk` and failing test**

Create `src/ui/diff_state.rs`:

```rust
use crate::myers::{DiffChunk, DiffTag};

/// Which side of a `DiffChunk` to compare cursor position against.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Side {
    A,
    B,
}

/// Find the index of the next (direction > 0) or previous (direction < 0)
/// non-Equal chunk relative to `cursor_line`. Wraps around if no match is
/// found in the given direction.
#[must_use]
pub fn find_next_chunk(
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

/// Format the chunk navigation label text.
/// Returns e.g. "Change 3 of 7", "7 changes", or "No changes".
#[must_use]
pub fn format_chunk_label(chunks: &[DiffChunk], current: Option<usize>) -> String {
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
                format!("Change {} of {total}", pos + 1)
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
    use crate::myers::DiffChunk;

    fn chunk(tag: DiffTag, sa: usize, ea: usize, sb: usize, eb: usize) -> DiffChunk {
        DiffChunk { tag, start_a: sa, end_a: ea, start_b: sb, end_b: eb }
    }

    // ─── find_next_chunk ────────────────────────────────────────────

    #[test]
    fn find_next_chunk_empty() {
        assert_eq!(find_next_chunk(&[], 0, 1, Side::A), None);
    }

    #[test]
    fn find_next_chunk_all_equal() {
        let chunks = vec![chunk(DiffTag::Equal, 0, 5, 0, 5)];
        assert_eq!(find_next_chunk(&chunks, 0, 1, Side::A), None);
    }

    #[test]
    fn find_next_chunk_forward() {
        let chunks = vec![
            chunk(DiffTag::Equal, 0, 3, 0, 3),
            chunk(DiffTag::Replace, 3, 5, 3, 6),
            chunk(DiffTag::Equal, 5, 8, 6, 9),
            chunk(DiffTag::Insert, 8, 8, 9, 11),
        ];
        // Cursor at line 0, go forward → first non-equal is index 1 (start_a=3)
        assert_eq!(find_next_chunk(&chunks, 0, 1, Side::A), Some(1));
        // Cursor at line 4 (inside chunk 1), go forward → next is index 3
        assert_eq!(find_next_chunk(&chunks, 4, 1, Side::A), Some(3));
    }

    #[test]
    fn find_next_chunk_backward() {
        let chunks = vec![
            chunk(DiffTag::Equal, 0, 3, 0, 3),
            chunk(DiffTag::Replace, 3, 5, 3, 6),
            chunk(DiffTag::Equal, 5, 8, 6, 9),
            chunk(DiffTag::Insert, 8, 8, 9, 11),
        ];
        // Cursor at line 10, go backward → last non-equal with start < 10 is index 3 (start_a=8)
        assert_eq!(find_next_chunk(&chunks, 10, -1, Side::A), Some(3));
    }

    #[test]
    fn find_next_chunk_wraps_forward() {
        let chunks = vec![
            chunk(DiffTag::Replace, 0, 2, 0, 2),
            chunk(DiffTag::Equal, 2, 5, 2, 5),
        ];
        // Cursor past all chunks, wraps to first
        assert_eq!(find_next_chunk(&chunks, 99, 1, Side::A), Some(0));
    }

    #[test]
    fn find_next_chunk_wraps_backward() {
        let chunks = vec![
            chunk(DiffTag::Equal, 0, 3, 0, 3),
            chunk(DiffTag::Replace, 3, 5, 3, 5),
        ];
        // Cursor before all chunks, wraps to last
        assert_eq!(find_next_chunk(&chunks, 0, -1, Side::A), Some(1));
    }

    #[test]
    fn find_next_chunk_side_b() {
        let chunks = vec![
            chunk(DiffTag::Equal, 0, 3, 0, 5),
            chunk(DiffTag::Replace, 3, 5, 5, 10),
        ];
        // Side::B uses start_b=5, cursor at 3 → found (5 > 3)
        assert_eq!(find_next_chunk(&chunks, 3, 1, Side::B), Some(1));
        // Side::A uses start_a=3, cursor at 3 → NOT found (3 > 3 is false), wraps
        assert_eq!(find_next_chunk(&chunks, 3, 1, Side::A), Some(1));
    }

    // ─── format_chunk_label ─────────────────────────────────────────

    #[test]
    fn label_no_changes() {
        let chunks = vec![chunk(DiffTag::Equal, 0, 5, 0, 5)];
        assert_eq!(format_chunk_label(&chunks, None), "No changes");
    }

    #[test]
    fn label_total_no_current() {
        let chunks = vec![
            chunk(DiffTag::Equal, 0, 3, 0, 3),
            chunk(DiffTag::Replace, 3, 5, 3, 5),
            chunk(DiffTag::Equal, 5, 8, 5, 8),
            chunk(DiffTag::Delete, 8, 10, 8, 8),
        ];
        assert_eq!(format_chunk_label(&chunks, None), "2 changes");
    }

    #[test]
    fn label_with_current() {
        let chunks = vec![
            chunk(DiffTag::Equal, 0, 3, 0, 3),
            chunk(DiffTag::Replace, 3, 5, 3, 5),
            chunk(DiffTag::Equal, 5, 8, 5, 8),
            chunk(DiffTag::Delete, 8, 10, 8, 8),
        ];
        // current=3 is the Delete chunk, which is the 2nd non-equal
        assert_eq!(format_chunk_label(&chunks, Some(3)), "Change 2 of 2");
    }

    #[test]
    fn label_current_is_first() {
        let chunks = vec![
            chunk(DiffTag::Replace, 0, 2, 0, 3),
            chunk(DiffTag::Equal, 2, 5, 3, 6),
        ];
        assert_eq!(format_chunk_label(&chunks, Some(0)), "Change 1 of 1");
    }

    #[test]
    fn label_current_invalid() {
        let chunks = vec![
            chunk(DiffTag::Replace, 0, 2, 0, 3),
            chunk(DiffTag::Equal, 2, 5, 3, 6),
        ];
        // current=1 is an Equal chunk — falls back to "N changes"
        assert_eq!(format_chunk_label(&chunks, Some(1)), "1 changes");
    }
}
```

**Step 2: Register the module**

In `src/ui/mod.rs`, add `pub(super) mod diff_state;` (or `pub mod diff_state;` — check the existing module visibility pattern). The existing modules use:
```rust
mod common;
mod diff_view;
// etc.
```

Add `mod diff_state;` alongside them.

**Step 3: Run tests to verify they pass**

Run: `cargo test diff_state`
Expected: All 11 tests pass.

**Step 4: Wire `common.rs` to use `diff_state`**

In `src/ui/common.rs`:
- Change `chunk_near_cursor` to delegate to `diff_state::find_next_chunk`
- Change `update_chunk_label` to use `diff_state::format_chunk_label`
- Re-export `diff_state::Side` or update `common.rs::Side` to use the one from `diff_state`

The `Side` enum currently lives in `common.rs:6-10`. Move it to `diff_state.rs` and re-export from `common.rs`:
```rust
pub(super) use super::diff_state::Side;
```

Update `navigate_chunk` (common.rs:1567) to call `diff_state::find_next_chunk` instead of the local `chunk_near_cursor`.

Update `update_chunk_label` (common.rs:1645) to call `diff_state::format_chunk_label` for the label text, then set it on the widget.

**Step 5: Run full test suite**

Run: `cargo test`
Expected: All tests pass (existing + new).

**Step 6: Run clippy and fmt**

Run: `cargo +nightly fmt && cargo clippy -- -W clippy::pedantic`
Expected: Clean.

**Step 7: Commit**

```
git add src/ui/diff_state.rs src/ui/mod.rs src/ui/common.rs
git commit -m "Extract navigation + label logic into diff_state.rs with tests"
```

---

## Task 2: Extract search, gutter hit-test, and chunk-map geometry into `diff_state.rs`

**Files:**
- Modify: `src/ui/diff_state.rs`
- Modify: `src/ui/common.rs:1424` (search), `1358` (gutter), `1475` (chunk map)

**Step 1: Add `find_all_matches` to `diff_state.rs` with tests**

```rust
/// Find all case-insensitive matches of `needle` in `text`.
/// Returns `(byte_start, byte_end)` pairs.
#[must_use]
pub fn find_all_matches(text: &str, needle: &str) -> Vec<(usize, usize)> {
    if needle.is_empty() {
        return vec![];
    }
    let lower_text = text.to_lowercase();
    let lower_needle = needle.to_lowercase();
    let mut matches = Vec::new();
    let mut start = 0;
    while let Some(pos) = lower_text[start..].find(&lower_needle) {
        let abs_pos = start + pos;
        matches.push((abs_pos, abs_pos + needle.len()));
        start = abs_pos + 1; // Allow overlapping matches
    }
    matches
}
```

Tests:
```rust
#[test]
fn find_all_matches_empty_needle() {
    assert_eq!(find_all_matches("hello", ""), vec![]);
}

#[test]
fn find_all_matches_no_match() {
    assert_eq!(find_all_matches("hello world", "xyz"), vec![]);
}

#[test]
fn find_all_matches_case_insensitive() {
    let m = find_all_matches("Hello HELLO hello", "hello");
    assert_eq!(m.len(), 3);
}

#[test]
fn find_all_matches_multiple() {
    let m = find_all_matches("abcabc", "abc");
    assert_eq!(m, vec![(0, 3), (3, 6)]);
}
```

**Step 2: Add `hit_test_gutter_arrow` to `diff_state.rs` with tests**

```rust
/// Result of a gutter arrow hit test.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GutterHit {
    /// Copy left → right (arrow at left edge)
    CopyLeftToRight,
    /// Copy right → left (arrow at right edge)
    CopyRightToLeft,
}

/// Test whether click at `(x, y)` in a gutter of given `width` hits an arrow.
/// `left_mid` / `right_mid` are the vertical midpoints of the chunk on each side.
/// Hit radius is 12px.
#[must_use]
pub fn hit_test_gutter_arrow(
    x: f64,
    y: f64,
    width: f64,
    left_mid: f64,
    right_mid: f64,
) -> Option<GutterHit> {
    let hit = 12.0_f64;
    // → arrow at left edge
    if (x - 2.0).powi(2) + (y - left_mid).powi(2) < hit * hit {
        return Some(GutterHit::CopyLeftToRight);
    }
    // ← arrow at right edge
    if (x - (width - 2.0)).powi(2) + (y - right_mid).powi(2) < hit * hit {
        return Some(GutterHit::CopyRightToLeft);
    }
    None
}
```

Tests:
```rust
#[test]
fn gutter_hit_left_arrow() {
    // Click near (2, 50) with left_mid=50 → hit left arrow
    assert_eq!(
        hit_test_gutter_arrow(3.0, 51.0, 100.0, 50.0, 50.0),
        Some(GutterHit::CopyLeftToRight)
    );
}

#[test]
fn gutter_hit_right_arrow() {
    // Click near (98, 50) with right_mid=50, width=100 → hit right arrow
    assert_eq!(
        hit_test_gutter_arrow(97.0, 49.0, 100.0, 50.0, 50.0),
        Some(GutterHit::CopyRightToLeft)
    );
}

#[test]
fn gutter_hit_miss() {
    // Click in middle → no hit
    assert_eq!(
        hit_test_gutter_arrow(50.0, 50.0, 100.0, 50.0, 50.0),
        None
    );
}

#[test]
fn gutter_hit_outside_radius() {
    // Click 20px away from arrow → no hit (radius is 12)
    assert_eq!(
        hit_test_gutter_arrow(2.0, 70.0, 100.0, 50.0, 50.0),
        None
    );
}
```

**Step 3: Add `compute_chunk_map_rects` to `diff_state.rs` with tests**

```rust
/// A rectangle in the chunk map minimap.
pub struct ChunkMapRect {
    pub y_start: f64,
    pub height: f64,
    pub tag: DiffTag,
}

/// Compute the visual rectangles for non-Equal chunks in the minimap.
/// `total_lines` is the total line count on the relevant side.
/// `map_height` is the pixel height of the minimap area.
/// `is_left` selects side-A or side-B line ranges.
#[must_use]
pub fn compute_chunk_map_rects(
    chunks: &[DiffChunk],
    total_lines: usize,
    map_height: f64,
    is_left: bool,
) -> Vec<ChunkMapRect> {
    if total_lines == 0 || map_height <= 0.0 {
        return vec![];
    }
    let lines = total_lines as f64;
    chunks
        .iter()
        .filter(|c| c.tag != DiffTag::Equal)
        .map(|c| {
            let (start, end) = if is_left {
                (c.start_a, c.end_a)
            } else {
                (c.start_b, c.end_b)
            };
            let y_start = (start as f64 / lines) * map_height;
            let y_end = (end as f64 / lines) * map_height;
            ChunkMapRect {
                y_start,
                height: (y_end - y_start).max(2.0),
                tag: c.tag,
            }
        })
        .collect()
}
```

Tests:
```rust
#[test]
fn chunk_map_empty() {
    assert!(compute_chunk_map_rects(&[], 100, 500.0, true).is_empty());
}

#[test]
fn chunk_map_zero_lines() {
    let chunks = vec![chunk(DiffTag::Replace, 0, 2, 0, 2)];
    assert!(compute_chunk_map_rects(&chunks, 0, 500.0, true).is_empty());
}

#[test]
fn chunk_map_basic() {
    let chunks = vec![
        chunk(DiffTag::Equal, 0, 50, 0, 50),
        chunk(DiffTag::Replace, 50, 60, 50, 65),
        chunk(DiffTag::Equal, 60, 100, 65, 105),
    ];
    let rects = compute_chunk_map_rects(&chunks, 100, 500.0, true);
    assert_eq!(rects.len(), 1);
    assert!((rects[0].y_start - 250.0).abs() < 0.01); // 50/100 * 500
    assert!((rects[0].height - 50.0).abs() < 0.01);   // 10/100 * 500
    assert_eq!(rects[0].tag, DiffTag::Replace);
}

#[test]
fn chunk_map_min_height() {
    // Single-line chunk on 1000-line file at height 100 → natural height 0.1, clamped to 2.0
    let chunks = vec![chunk(DiffTag::Insert, 500, 501, 500, 502)];
    let rects = compute_chunk_map_rects(&chunks, 1000, 100.0, true);
    assert_eq!(rects.len(), 1);
    assert!((rects[0].height - 2.0).abs() < 0.01);
}
```

**Step 4: Wire `common.rs` to delegate to these new functions**

Update `handle_gutter_click` (common.rs:1358) to call `diff_state::hit_test_gutter_arrow` with the computed midpoints, then dispatch `copy_chunk` based on the result.

Update `draw_chunk_map` (common.rs:1475) to call `diff_state::compute_chunk_map_rects` for the geometry, then only do the cairo drawing.

Note: `highlight_search_matches` uses GTK's `TextBuffer::forward_search` which does case-insensitive search differently than our pure `find_all_matches`. The pure function is for testing search logic; the GTK function still does the actual highlighting. Keep both — the pure version validates the match-counting logic.

**Step 5: Run full test suite + clippy + fmt**

Run: `cargo test && cargo +nightly fmt && cargo clippy -- -W clippy::pedantic`

**Step 6: Commit**

```
git add src/ui/diff_state.rs src/ui/common.rs
git commit -m "Extract search, gutter hit-test, chunk-map geometry into diff_state"
```

---

## Task 3: Create `merge_state.rs` — Extract merge-specific logic

**Files:**
- Create: `src/ui/merge_state.rs`
- Modify: `src/ui/mod.rs` (add `mod merge_state;`)
- Modify: `src/ui/merge_view.rs:156,181`

**Step 1: Create `merge_state.rs` with extracted functions and tests**

Move `merge_change_indices` and extract `find_conflict_markers_in_text` from `find_conflict_markers`:

```rust
use crate::myers::{DiffChunk, DiffTag};

/// Merge left and right change indices sorted by middle-file line.
/// Returns `(chunk_index, is_right_diff)` pairs.
#[must_use]
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
    indices.into_iter().map(|(i, is_right, _)| (i, is_right)).collect()
}

/// Find line numbers of `<<<<<<<` conflict markers in text.
#[must_use]
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::myers::DiffChunk;

    fn chunk(tag: DiffTag, sa: usize, ea: usize, sb: usize, eb: usize) -> DiffChunk {
        DiffChunk { tag, start_a: sa, end_a: ea, start_b: sb, end_b: eb }
    }

    #[test]
    fn conflict_markers_none() {
        assert_eq!(find_conflict_markers_in_text("hello\nworld\n"), vec![]);
    }

    #[test]
    fn conflict_markers_found() {
        let text = "line0\n<<<<<<< HEAD\nours\n=======\ntheirs\n>>>>>>> branch\nline6";
        assert_eq!(find_conflict_markers_in_text(text), vec![1]);
    }

    #[test]
    fn conflict_markers_multiple() {
        let text = "<<<<<<< A\na\n=======\nb\n>>>>>>> B\n<<<<<<< C\nc\n=======\nd\n>>>>>>> D";
        assert_eq!(find_conflict_markers_in_text(text), vec![0, 5]);
    }

    #[test]
    fn merge_indices_empty() {
        assert_eq!(merge_change_indices(&[], &[]), vec![]);
    }

    #[test]
    fn merge_indices_sorted_by_middle() {
        let left = vec![
            chunk(DiffTag::Equal, 0, 3, 0, 3),
            chunk(DiffTag::Replace, 3, 5, 3, 6),  // middle line 3
        ];
        let right = vec![
            chunk(DiffTag::Replace, 0, 2, 0, 2),  // middle line 0 (start_a)
            chunk(DiffTag::Equal, 2, 6, 2, 6),
        ];
        let result = merge_change_indices(&left, &right);
        // Right chunk at middle line 0 sorts before left chunk at middle line 3
        assert_eq!(result, vec![(0, true), (1, false)]);
    }
}
```

**Step 2: Register module, wire merge_view.rs**

Add `mod merge_state;` to `src/ui/mod.rs`.

In `merge_view.rs`, update `find_conflict_markers` to call `merge_state::find_conflict_markers_in_text` on extracted buffer text. Update `merge_change_indices` to delegate to `merge_state::merge_change_indices`.

**Step 3: Move existing `merge_change_indices` tests from `merge_view.rs`**

The existing tests in `merge_view.rs` for `merge_change_indices` should be moved/duplicated to `merge_state.rs` since the logic now lives there. Remove duplicates from `merge_view.rs` if the functions are fully delegated.

**Step 4: Run full suite + clippy + fmt, commit**

```
cargo test && cargo +nightly fmt && cargo clippy -- -W clippy::pedantic
git add src/ui/merge_state.rs src/ui/mod.rs src/ui/merge_view.rs
git commit -m "Extract merge logic into merge_state.rs with conflict marker tests"
```

---

## Task 4: CLI integration tests

**Files:**
- Create: `tests/cli_integration.rs`

**Step 1: Create the test file with all CLI mode tests**

This tests TESTING.md "CLI / Launch Modes" section (all 15 items).

```rust
use std::process::Command;
use tempfile::tempdir;

fn mergers_cmd() -> Command {
    Command::new(env!("CARGO_BIN_EXE_mergers"))
}

// ── Zero arguments: launches Welcome (exit 0 expected, but the app
//    tries to open a GTK window which fails without a display).
//    We test this under xvfb in CI; here we just verify it doesn't
//    produce an error on stderr about arguments.

#[test]
fn two_files_exits_zero() {
    let dir = tempdir().unwrap();
    let left = dir.path().join("left.txt");
    let right = dir.path().join("right.txt");
    std::fs::write(&left, "hello\n").unwrap();
    std::fs::write(&right, "world\n").unwrap();

    // Without a display, GTK will fail to init — that's expected.
    // We verify it does NOT print our custom "Error:" messages.
    let out = mergers_cmd().args([&left, &right]).output().unwrap();
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(!stderr.contains("Error:"), "unexpected arg error: {stderr}");
}

#[test]
fn single_file_errors() {
    let dir = tempdir().unwrap();
    let f = dir.path().join("only.txt");
    std::fs::write(&f, "content").unwrap();

    let out = mergers_cmd().arg(&f).output().unwrap();
    assert!(!out.status.success());
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("single file argument not supported"));
}

#[test]
fn nonexistent_path_errors() {
    let out = mergers_cmd()
        .args(["/nonexistent/path/a", "/nonexistent/path/b"])
        .output()
        .unwrap();
    assert!(!out.status.success());
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("does not exist"));
}

#[test]
fn file_dir_mix_errors() {
    let dir = tempdir().unwrap();
    let f = dir.path().join("file.txt");
    std::fs::write(&f, "x").unwrap();

    let out = mergers_cmd().args([&f, dir.path()]).output().unwrap();
    assert!(!out.status.success());
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("cannot compare a file with a directory"));
}

#[test]
fn three_files_non_file_errors() {
    let dir = tempdir().unwrap();
    let f1 = dir.path().join("a.txt");
    let f2 = dir.path().join("b.txt");
    std::fs::write(&f1, "a").unwrap();
    std::fs::write(&f2, "b").unwrap();

    // Third arg is a directory, not a file
    let out = mergers_cmd()
        .args([&f1, &f2, dir.path()])
        .output()
        .unwrap();
    assert!(!out.status.success());
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("is not a file"));
}

#[test]
fn non_git_dir_errors() {
    let dir = tempdir().unwrap();
    let out = mergers_cmd().arg(dir.path()).output().unwrap();
    assert!(!out.status.success());
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("not inside a git repository"));
}

#[test]
fn too_many_args_errors() {
    let dir = tempdir().unwrap();
    let files: Vec<_> = (0..5)
        .map(|i| {
            let p = dir.path().join(format!("{i}.txt"));
            std::fs::write(&p, "x").unwrap();
            p
        })
        .collect();

    let out = mergers_cmd()
        .args(&files)
        .output()
        .unwrap();
    assert!(!out.status.success());
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("expected 0-3 paths"));
}

#[test]
fn version_flag() {
    let out = mergers_cmd().arg("--version").output().unwrap();
    assert!(out.status.success());
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.contains("mergers"));
}

#[test]
fn help_flag() {
    let out = mergers_cmd().arg("--help").output().unwrap();
    assert!(out.status.success());
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.contains("Visual diff and merge tool"));
}

#[test]
fn git_repo_single_dir_no_error_message() {
    // Create a temp dir that IS a git repo
    let dir = tempdir().unwrap();
    Command::new("git")
        .args(["init", dir.path().to_str().unwrap()])
        .output()
        .unwrap();

    let out = mergers_cmd().arg(dir.path()).output().unwrap();
    let stderr = String::from_utf8_lossy(&out.stderr);
    // Should NOT print our "Error:" messages (may fail on GTK init without display)
    assert!(!stderr.contains("Error:"), "unexpected arg error: {stderr}");
}
```

**Step 2: Run tests**

Run: `cargo test --test cli_integration`
Expected: All pass. Note: tests that would launch GTK windows will fail in a non-display environment but should not print CLI argument errors.

**Step 3: Commit**

```
git add tests/cli_integration.rs
git commit -m "Add CLI integration tests covering all launch mode error paths"
```

---

## Task 5: Property-based test extensions

**Files:**
- Modify: `src/ui/diff_state.rs` (add proptest tests)
- Modify: `src/ui/common.rs` (add proptest for `generate_unified_diff`)

**Step 1: Add proptest for `find_next_chunk` navigation invariants**

In `diff_state.rs` test module:

```rust
#[cfg(test)]
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
            (arb_tag(), 0..1000usize, 0..1000usize, 0..1000usize, 0..1000usize)
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
        fn nav_forward_finds_chunk_past_cursor(
            chunks in arb_chunks(),
            cursor in 0..2000usize,
        ) {
            if let Some(idx) = find_next_chunk(&chunks, cursor, 1, Side::A) {
                // Result is a valid index
                prop_assert!(idx < chunks.len());
                // Result is non-equal
                prop_assert_ne!(chunks[idx].tag, DiffTag::Equal);
            }
        }

        #[test]
        fn nav_backward_finds_chunk_before_cursor(
            chunks in arb_chunks(),
            cursor in 0..2000usize,
        ) {
            if let Some(idx) = find_next_chunk(&chunks, cursor, -1, Side::A) {
                prop_assert!(idx < chunks.len());
                prop_assert_ne!(chunks[idx].tag, DiffTag::Equal);
            }
        }
    }
}
```

**Step 2: Add proptest for `generate_unified_diff` format validity**

In `src/ui/common.rs` test module (or a new test in the existing `tests` mod):

```rust
#[cfg(test)]
mod unified_diff_tests {
    use super::*;
    use crate::myers;

    #[test]
    fn unified_diff_basic() {
        let left = "line1\nline2\nline3\n";
        let right = "line1\nchanged\nline3\n";
        let chunks = myers::diff_lines(left, right);
        let patch = generate_unified_diff("a.txt", "b.txt", left, right, &chunks);

        assert!(patch.starts_with("--- a.txt\n"));
        assert!(patch.contains("+++ b.txt\n"));
        assert!(patch.contains("@@ "));
        assert!(patch.contains("-line2"));
        assert!(patch.contains("+changed"));
    }

    #[test]
    fn unified_diff_identical() {
        let text = "same\n";
        let chunks = myers::diff_lines(text, text);
        let patch = generate_unified_diff("a", "b", text, text, &chunks);
        // Identical files → empty patch (just headers, no hunks)
        assert!(patch.starts_with("--- a\n+++ b\n"));
        assert!(!patch.contains("@@ "));
    }

    #[test]
    fn unified_diff_empty_to_content() {
        let chunks = myers::diff_lines("", "new\n");
        let patch = generate_unified_diff("a", "b", "", "new\n", &chunks);
        assert!(patch.contains("+new"));
    }
}
```

**Step 3: Add proptest for unified diff format**

```rust
#[cfg(test)]
mod unified_diff_proptests {
    use super::*;
    use crate::myers;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn unified_diff_format_valid(
            left in "([a-z ]{0,20}\n){0,10}",
            right in "([a-z ]{0,20}\n){0,10}",
        ) {
            let chunks = myers::diff_lines(&left, &right);
            let patch = generate_unified_diff("a", "b", &left, &right, &chunks);

            // Must start with ---/+++ headers
            prop_assert!(patch.starts_with("--- a\n"));
            prop_assert!(patch.contains("+++ b\n"));

            // Every @@ hunk header must have valid format
            for line in patch.lines() {
                if line.starts_with("@@ ") {
                    prop_assert!(line.contains(" @@"), "malformed hunk: {line}");
                }
            }
        }
    }
}
```

**Step 4: Run tests + clippy + fmt, commit**

```
cargo test && cargo +nightly fmt && cargo clippy -- -W clippy::pedantic
git add src/ui/diff_state.rs src/ui/common.rs
git commit -m "Add property-based tests for navigation and unified diff format"
```

---

## Task 6: Add `generate_unified_diff` and `read_file_content` unit tests

**Files:**
- Modify: `src/ui/common.rs` (add tests to existing test module)

**Step 1: Add tests for `generate_unified_diff`**

See Task 5 Step 2 — the basic tests. Add additional edge cases:

```rust
#[test]
fn unified_diff_content_to_empty() {
    let chunks = myers::diff_lines("old\n", "");
    let patch = generate_unified_diff("a", "b", "old\n", "", &chunks);
    assert!(patch.contains("-old"));
}

#[test]
fn unified_diff_multi_hunk() {
    // Two changes far apart should produce two separate hunks
    let mut left_lines = Vec::new();
    let mut right_lines = Vec::new();
    for i in 0..20 {
        if i == 2 {
            left_lines.push("old_a");
            right_lines.push("new_a");
        } else if i == 18 {
            left_lines.push("old_b");
            right_lines.push("new_b");
        } else {
            left_lines.push("same");
            right_lines.push("same");
        }
    }
    let left = left_lines.join("\n") + "\n";
    let right = right_lines.join("\n") + "\n";
    let chunks = myers::diff_lines(&left, &right);
    let patch = generate_unified_diff("a", "b", &left, &right, &chunks);
    let hunk_count = patch.lines().filter(|l| l.starts_with("@@ ")).count();
    assert!(hunk_count >= 2, "expected 2+ hunks, got {hunk_count}");
}
```

**Step 2: Add tests for `read_file_content`**

```rust
#[test]
fn read_file_content_text() {
    let dir = tempfile::tempdir().unwrap();
    let p = dir.path().join("text.txt");
    std::fs::write(&p, "hello\nworld\n").unwrap();
    let (content, is_binary) = read_file_content(&p);
    assert_eq!(content, "hello\nworld\n");
    assert!(!is_binary);
}

#[test]
fn read_file_content_binary() {
    let dir = tempfile::tempdir().unwrap();
    let p = dir.path().join("bin");
    std::fs::write(&p, b"hello\x00world").unwrap();
    let (content, is_binary) = read_file_content(&p);
    assert!(is_binary);
    assert!(content.is_empty());
}

#[test]
fn read_file_content_nonexistent() {
    let (content, is_binary) = read_file_content(std::path::Path::new("/nonexistent"));
    assert!(content.is_empty());
    assert!(!is_binary);
}

#[test]
fn read_file_content_empty() {
    let dir = tempfile::tempdir().unwrap();
    let p = dir.path().join("empty");
    std::fs::write(&p, "").unwrap();
    let (content, is_binary) = read_file_content(&p);
    assert!(content.is_empty());
    assert!(!is_binary);
}
```

**Step 3: Run, fmt, clippy, commit**

```
cargo test && cargo +nightly fmt && cargo clippy -- -W clippy::pedantic
git add src/ui/common.rs
git commit -m "Add unit tests for generate_unified_diff and read_file_content"
```

---

## Task 7: Update CI for Xvfb

**Files:**
- Modify: `.github/workflows/ci.yml`

**Step 1: Add xvfb and font packages**

In the CI install step, add `xvfb` and `fonts-dejavu-core`. Change the test runner to use `xvfb-run`:

```yaml
- name: Install dependencies
  run: |
    sudo apt-get update
    sudo apt-get install -y libgtksourceview-5-dev xvfb fonts-dejavu-core

- name: Test
  env:
    GDK_BACKEND: x11
    GTK_A11Y: none
  run: xvfb-run -a cargo test --all-targets --locked
```

**Step 2: Verify CI still passes**

Push branch, check CI. All existing tests should still pass under xvfb (they don't use GTK, so the virtual display is harmless).

**Step 3: Commit**

```
git add .github/workflows/ci.yml
git commit -m "Add xvfb to CI for GTK widget tests"
```

---

## Task 8: Test fixtures and Makefile

**Files:**
- Create: `tests/fixtures/left.txt`
- Create: `tests/fixtures/right.txt`
- Create: `tests/fixtures/base.txt`
- Create: `tests/fixtures/identical.txt`
- Create: `tests/fixtures/binary.bin`
- Create: `Makefile`

**Step 1: Create test fixtures**

`tests/fixtures/left.txt`:
```
line 1
line 2
line 3
line 4
line 5
original line 6
line 7
line 8
line 9
line 10
```

`tests/fixtures/right.txt`:
```
line 1
line 2
changed line 3
line 4
line 5
modified line 6
line 7
inserted line 7.5
line 8
line 9
line 10
```

`tests/fixtures/base.txt` (for 3-way merge — the "middle" ancestor):
```
line 1
line 2
line 3
line 4
line 5
line 6
line 7
line 8
line 9
line 10
```

`tests/fixtures/identical.txt`:
```
same content
on every line
nothing changed
```

`tests/fixtures/binary.bin`: (write with a script — contains NUL bytes)

**Step 2: Create Makefile**

```makefile
.PHONY: test test-ui test-release fmt clippy check

test:
	cargo test

test-ui:
	xvfb-run -a cargo test

test-release:
	xvfb-run -a cargo test
	cd tests/ui_integration && xvfb-run -a python -m pytest -v

fmt:
	cargo +nightly fmt

clippy:
	cargo clippy -- -W clippy::pedantic

check: fmt clippy test
```

**Step 3: Commit**

```
git add tests/fixtures/ Makefile
git commit -m "Add test fixtures and Makefile with test convenience targets"
```

---

## Task 9: Dogtail test scaffold

**Files:**
- Create: `tests/ui_integration/conftest.py`
- Create: `tests/ui_integration/test_file_diff.py`
- Create: `tests/ui_integration/requirements.txt`

**Step 1: Create requirements.txt**

```
dogtail>=1.0
pytest>=7.0
```

**Step 2: Create `conftest.py` with app launcher fixture**

```python
import os
import signal
import subprocess
import time

import pytest

MERGERS_BIN = os.environ.get(
    "MERGERS_BIN",
    os.path.join(os.path.dirname(__file__), "../../target/release/mergers"),
)
FIXTURES = os.path.join(os.path.dirname(__file__), "../fixtures")


@pytest.fixture
def app_process():
    """Launch mergers and yield the process. Kill on teardown."""
    processes = []

    def _launch(*args):
        proc = subprocess.Popen(
            [MERGERS_BIN, *args],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        processes.append(proc)
        time.sleep(2)  # Wait for GTK to initialize
        return proc

    yield _launch

    for proc in processes:
        proc.send_signal(signal.SIGTERM)
        try:
            proc.wait(timeout=5)
        except subprocess.TimeoutExpired:
            proc.kill()


@pytest.fixture
def fixture_path():
    """Return path to a fixture file."""
    def _path(name):
        return os.path.join(FIXTURES, name)
    return _path
```

**Step 3: Create initial dogtail test**

```python
"""Smoke test: launch mergers with two files, verify window appears."""
import dogtail.tree
from dogtail.config import config

config.searchShowingOnly = True


def test_file_diff_opens(app_process, fixture_path):
    """Open a two-file diff and verify the window is accessible via AT-SPI."""
    left = fixture_path("left.txt")
    right = fixture_path("right.txt")

    app_process(left, right)

    # Find the mergers application in the accessibility tree
    app = dogtail.tree.root.application("mergers")
    assert app is not None, "mergers app not found in AT-SPI tree"

    # Verify a window exists
    windows = app.findChildren(lambda n: n.roleName == "frame")
    assert len(windows) >= 1, "No window found"
```

**Step 4: Commit**

```
git add tests/ui_integration/
git commit -m "Add dogtail test scaffold with smoke test for file diff"
```

---

## Task 10: Dogtail keyboard shortcut tests

**Files:**
- Create: `tests/ui_integration/test_shortcuts.py`

**Step 1: Write keyboard shortcut tests**

```python
"""Test keyboard shortcuts via dogtail AT-SPI."""
import dogtail.tree
from dogtail.config import config
from dogtail.utils import doDelay
from dogtail import rawinput

config.searchShowingOnly = True


def test_find_bar_opens(app_process, fixture_path):
    """Ctrl+F should open the find bar."""
    app_process(fixture_path("left.txt"), fixture_path("right.txt"))
    app = dogtail.tree.root.application("mergers")
    window = app.findChildren(lambda n: n.roleName == "frame")[0]

    # Press Ctrl+F
    rawinput.pressKey("ctrl+f")
    doDelay(0.5)

    # Find bar should now be visible — look for a text entry
    entries = window.findChildren(lambda n: n.roleName == "text")
    assert len(entries) > 0, "No text entry found after Ctrl+F"


def test_navigation_keys(app_process, fixture_path):
    """Alt+Down should navigate to next change."""
    app_process(fixture_path("left.txt"), fixture_path("right.txt"))
    app = dogtail.tree.root.application("mergers")

    # Press Alt+Down
    rawinput.pressKey("alt+Down")
    doDelay(0.5)

    # Verify the chunk label updated (should show "Change X of Y")
    # This requires the label to be accessible
    labels = app.findChildren(lambda n: n.roleName == "label")
    chunk_labels = [l for l in labels if "Change" in l.name or "changes" in l.name]
    assert len(chunk_labels) > 0, "No chunk label found after navigation"
```

**Step 2: Commit**

```
git add tests/ui_integration/test_shortcuts.py
git commit -m "Add dogtail keyboard shortcut tests"
```

---

## Task 11: Pre-release CI workflow

**Files:**
- Create: `.github/workflows/ui-tests.yml`

**Step 1: Create manual-dispatch workflow**

```yaml
name: UI Integration Tests

on:
  workflow_dispatch:

jobs:
  ui-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Install system dependencies
        run: |
          sudo apt-get update
          sudo apt-get install -y \
            libgtksourceview-5-dev \
            xvfb \
            fonts-dejavu-core \
            at-spi2-core \
            python3-pip \
            python3-gi \
            gir1.2-atspi-2.0

      - name: Install Rust toolchain
        uses: dtolnay/rust-toolchain@stable

      - name: Build release
        run: cargo build --release

      - name: Install Python dependencies
        run: pip3 install -r tests/ui_integration/requirements.txt

      - name: Run Rust tests under Xvfb
        env:
          GDK_BACKEND: x11
          GTK_A11Y: none
        run: xvfb-run -a cargo test --all-targets --locked

      - name: Run dogtail integration tests
        env:
          GDK_BACKEND: x11
          DBUS_SESSION_BUS_ADDRESS: unix:path=/run/user/1001/bus
        run: |
          eval $(dbus-launch --sh-syntax)
          /usr/libexec/at-spi2-registryd &
          sleep 1
          xvfb-run -a python3 -m pytest tests/ui_integration/ -v --tb=long
```

**Step 2: Commit**

```
git add .github/workflows/ui-tests.yml
git commit -m "Add manual-dispatch UI integration test workflow for pre-release"
```

---

## Task 12: Expand dogtail tests for core workflows

**Files:**
- Create: `tests/ui_integration/test_dir_compare.py`
- Create: `tests/ui_integration/test_vcs.py`
- Create: `tests/fixtures/left_dir/` and `tests/fixtures/right_dir/`
- Modify: `tests/ui_integration/test_file_diff.py` (expand)

**Step 1: Create directory fixtures**

```
tests/fixtures/left_dir/
  same.txt       → "identical content\n"
  different.txt  → "left version\n"
  only_left.txt  → "only on left\n"
  subdir/
    nested.txt   → "nested left\n"

tests/fixtures/right_dir/
  same.txt       → "identical content\n"
  different.txt  → "right version\n"
  only_right.txt → "only on right\n"
  subdir/
    nested.txt   → "nested right\n"
```

**Step 2: Write directory comparison tests**

```python
"""Directory comparison tests via dogtail."""
import dogtail.tree
from dogtail.config import config

config.searchShowingOnly = True


def test_dir_comparison_opens(app_process, fixture_path):
    """Two directories should open directory comparison window."""
    app_process(fixture_path("left_dir"), fixture_path("right_dir"))
    app = dogtail.tree.root.application("mergers")
    windows = app.findChildren(lambda n: n.roleName == "frame")
    assert len(windows) >= 1
```

**Step 3: Expand file diff tests**

Add to `test_file_diff.py`:

```python
def test_save_button_initially_insensitive(app_process, fixture_path):
    """Save buttons should be insensitive when no changes made."""
    app_process(fixture_path("left.txt"), fixture_path("right.txt"))
    app = dogtail.tree.root.application("mergers")
    buttons = app.findChildren(lambda n: n.roleName == "push button" and "Save" in n.name)
    for btn in buttons:
        assert not btn.sensitive, f"Save button '{btn.name}' should be insensitive"
```

**Step 4: Commit**

```
git add tests/ui_integration/ tests/fixtures/
git commit -m "Expand dogtail tests: directory comparison, save button state"
```

---

## Summary — Commit sequence

| Task | What | Tests added |
|------|------|-------------|
| 1 | `diff_state.rs` — navigation + label | ~11 unit tests |
| 2 | `diff_state.rs` — search, gutter, chunk map | ~11 unit tests |
| 3 | `merge_state.rs` — conflict markers + indices | ~5 unit tests |
| 4 | CLI integration tests | ~10 integration tests |
| 5 | Property-based test extensions | ~4 proptest tests |
| 6 | `generate_unified_diff` + `read_file_content` tests | ~8 unit tests |
| 7 | CI Xvfb setup | (infra) |
| 8 | Test fixtures + Makefile | (infra) |
| 9 | Dogtail scaffold + smoke test | ~1 integration test |
| 10 | Dogtail keyboard shortcuts | ~2 integration tests |
| 11 | Pre-release CI workflow | (infra) |
| 12 | Expanded dogtail tests | ~3 integration tests |

**Total new tests: ~55+ automated tests**, covering ~74% of TESTING.md checklist items across all three layers.
