#[allow(clippy::wildcard_imports)]
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

// ── generate_unified_diff ─────────────────────────────────────

#[test]
fn unified_diff_basic() {
    let left = "line1\nline2\nline3\n";
    let right = "line1\nchanged\nline3\n";
    let chunks = crate::myers::diff_lines(left, right);
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
    let chunks = crate::myers::diff_lines(text, text);
    let patch = generate_unified_diff("a", "b", text, text, &chunks);
    assert!(patch.starts_with("--- a\n+++ b\n"));
    assert!(
        !patch.contains("@@ "),
        "identical files should have no hunks"
    );
}

#[test]
fn unified_diff_empty_to_content() {
    let chunks = crate::myers::diff_lines("", "new\n");
    let patch = generate_unified_diff("a", "b", "", "new\n", &chunks);
    assert!(patch.contains("+new"));
}

#[test]
fn unified_diff_content_to_empty() {
    let chunks = crate::myers::diff_lines("old\n", "");
    let patch = generate_unified_diff("a", "b", "old\n", "", &chunks);
    assert!(patch.contains("-old"));
}

#[test]
fn unified_diff_multi_hunk() {
    // Two changes far apart should produce two separate @@ hunks
    let mut left_lines: Vec<&str> = Vec::new();
    let mut right_lines: Vec<&str> = Vec::new();
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
    let chunks = crate::myers::diff_lines(&left, &right);
    let patch = generate_unified_diff("a", "b", &left, &right, &chunks);
    let hunk_count = patch.lines().filter(|l| l.starts_with("@@ ")).count();
    assert!(hunk_count >= 2, "expected 2+ hunks, got {hunk_count}");
}

// ── read_file_content ─────────────────────────────────────────

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
    let (content, is_binary) = read_file_content(std::path::Path::new("/nonexistent/path"));
    assert!(content.is_empty());
    assert!(!is_binary);
}

#[test]
fn read_file_content_empty_file() {
    let dir = tempfile::tempdir().unwrap();
    let p = dir.path().join("empty");
    std::fs::write(&p, "").unwrap();
    let (content, is_binary) = read_file_content(&p);
    assert!(content.is_empty());
    assert!(!is_binary);
}

mod unified_diff_proptests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn unified_diff_always_has_headers(
            left in "([a-z ]{0,20}\n){0,10}",
            right in "([a-z ]{0,20}\n){0,10}",
        ) {
            let chunks = myers::diff_lines(&left, &right);
            let patch = generate_unified_diff("a", "b", &left, &right, &chunks);
            prop_assert!(patch.starts_with("--- a\n"), "missing --- header");
            prop_assert!(patch.contains("+++ b\n"), "missing +++ header");
        }

        #[test]
        fn unified_diff_hunks_are_well_formed(
            left in "([a-z ]{0,20}\n){0,10}",
            right in "([a-z ]{0,20}\n){0,10}",
        ) {
            let chunks = myers::diff_lines(&left, &right);
            let patch = generate_unified_diff("a", "b", &left, &right, &chunks);
            for line in patch.lines() {
                if line.starts_with("@@ ") {
                    prop_assert!(line.contains(" @@"), "malformed hunk header: {line}");
                }
            }
        }
    }
}

// ── hex_luminance ───────────────────────────────────────────

#[test]
fn luminance_white() {
    assert!((hex_luminance("#ffffff") - 1.0).abs() < 0.001);
}

#[test]
fn luminance_black() {
    assert!(hex_luminance("#000000").abs() < 0.001);
}

#[test]
fn luminance_without_hash() {
    assert!((hex_luminance("ffffff") - 1.0).abs() < 0.001);
}

#[test]
fn luminance_dark_background() {
    // #2e3436 (Adwaita dark) should be below 0.5
    assert!(hex_luminance("#2e3436") < 0.5);
}

#[test]
fn luminance_light_background() {
    // #fafafa (typical light bg) should be above 0.5
    assert!(hex_luminance("#fafafa") > 0.5);
}

#[test]
fn luminance_short_string_defaults_light() {
    assert!((hex_luminance("#fff") - 1.0).abs() < 0.001);
}

#[test]
fn luminance_empty_defaults_light() {
    assert!((hex_luminance("") - 1.0).abs() < 0.001);
}

// ── dark-scheme colour selection ────────────────────────────

#[test]
fn colour_functions_respect_dark_flag() {
    // Light mode
    IS_DARK_SCHEME.with(|c| c.set(false));
    let light_bg = chunk_bg_insert();
    let light_inline = inline_changed();
    let light_search = search_match_bg();

    // Dark mode
    IS_DARK_SCHEME.with(|c| c.set(true));
    let dark_bg = chunk_bg_insert();
    let dark_inline = inline_changed();
    let dark_search = search_match_bg();

    assert_ne!(light_bg, dark_bg);
    assert_ne!(light_inline, dark_inline);
    assert_ne!(light_search, dark_search);

    // Restore
    IS_DARK_SCHEME.with(|c| c.set(false));
}

#[test]
fn all_colour_pairs_differ() {
    IS_DARK_SCHEME.with(|c| c.set(false));
    let light = (
        chunk_bg_insert(),
        chunk_bg_replace(),
        chunk_bg_conflict(),
        stroke_insert(),
        stroke_replace(),
        stroke_conflict(),
        band_insert(),
        band_replace(),
    );
    IS_DARK_SCHEME.with(|c| c.set(true));
    let dark = (
        chunk_bg_insert(),
        chunk_bg_replace(),
        chunk_bg_conflict(),
        stroke_insert(),
        stroke_replace(),
        stroke_conflict(),
        band_insert(),
        band_replace(),
    );
    assert_ne!(light, dark);
    IS_DARK_SCHEME.with(|c| c.set(false));
}
