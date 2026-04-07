#![allow(
    clippy::similar_names,
    clippy::format_push_string,
    clippy::format_collect
)]

use std::hint::black_box;

use criterion::{Criterion, criterion_group, criterion_main};
use mergers::{
    _bench::{self, Side},
    myers::{self, DiffChunk, DiffTag},
};

// ── Helpers ─────────────────────────────────────────────────────────────────

fn make_similar_files(n: usize, changes: usize) -> (String, String) {
    let mut a = String::new();
    let mut b = String::new();
    let step = if changes > 0 {
        n / (changes + 1)
    } else {
        n + 1
    };
    for i in 0..n {
        if changes > 0 && i % step == step / 2 && i / step < changes {
            a.push_str(&format!(
                "original line {i} with some typical source code content\n"
            ));
            b.push_str(&format!(
                "modified line {i} with some changed source code content\n"
            ));
        } else {
            let line = format!("common line {i} with typical source code padding content here\n");
            a.push_str(&line);
            b.push_str(&line);
        }
    }
    (a, b)
}

/// Generate a chunk list with interleaved Equal and non-Equal chunks.
/// Produces roughly `n_changes` Replace chunks among `n_total` total chunks.
fn make_chunks(n_total: usize, n_changes: usize) -> Vec<DiffChunk> {
    let mut chunks = Vec::new();
    let mut pos_a = 0;
    let mut pos_b = 0;
    let mut changes_placed = 0;
    let step = if n_changes > 0 {
        n_total / n_changes
    } else {
        n_total + 1
    };
    for i in 0..n_total {
        if step > 0 && i % step == step / 2 && changes_placed < n_changes {
            let len_a = 3;
            let len_b = 5;
            chunks.push(DiffChunk {
                tag: DiffTag::Replace,
                start_a: pos_a,
                end_a: pos_a + len_a,
                start_b: pos_b,
                end_b: pos_b + len_b,
            });
            pos_a += len_a;
            pos_b += len_b;
            changes_placed += 1;
        } else {
            let len = 20;
            chunks.push(DiffChunk {
                tag: DiffTag::Equal,
                start_a: pos_a,
                end_a: pos_a + len,
                start_b: pos_b,
                end_b: pos_b + len,
            });
            pos_a += len;
            pos_b += len;
        }
    }
    chunks
}

/// Build two chunk lists that share a middle pane (simulating 3-way merge).
fn make_merge_chunks(n_total: usize) -> (Vec<DiffChunk>, Vec<DiffChunk>) {
    let left = make_chunks(n_total, n_total / 3);
    let right = make_chunks(n_total, n_total / 4);
    (left, right)
}

fn make_conflict_text(n_lines: usize, n_conflicts: usize) -> String {
    let mut text = String::new();
    let step = n_lines / (n_conflicts + 1);
    let mut conflicts_placed = 0;
    for i in 0..n_lines {
        if conflicts_placed < n_conflicts && i > 0 && i % step == 0 {
            text.push_str("<<<<<<< HEAD\n");
            text.push_str("ours line content here\n");
            text.push_str("more ours content\n");
            text.push_str("=======\n");
            text.push_str("theirs line content here\n");
            text.push_str("more theirs content\n");
            text.push_str(">>>>>>> feature-branch\n");
            conflicts_placed += 1;
        } else {
            text.push_str(&format!(
                "normal source code line {i} with realistic padding\n"
            ));
        }
    }
    text
}

// ── Myers diff benchmarks ───────────────────────────────────────────────────

fn bench_diff_lines(c: &mut Criterion) {
    let (a_50k, b_50k) = make_similar_files(50_000, 500);
    let (a_200k, b_200k) = make_similar_files(200_000, 2_000);
    let (a_500k, b_500k) = make_similar_files(500_000, 5_000);

    c.bench_function("diff_lines/50k_lines_500_changes", |b| {
        b.iter(|| myers::diff_lines(black_box(&a_50k), black_box(&b_50k)));
    });
    c.bench_function("diff_lines/200k_lines_2k_changes", |b| {
        b.iter(|| myers::diff_lines(black_box(&a_200k), black_box(&b_200k)));
    });
    c.bench_function("diff_lines/500k_lines_5k_changes", |b| {
        b.iter(|| myers::diff_lines(black_box(&a_500k), black_box(&b_500k)));
    });
}

fn bench_diff_completely_different(c: &mut Criterion) {
    let a: String = (0..50_000).map(|i| format!("line_a_{i}\n")).collect();
    let b: String = (0..50_000).map(|i| format!("line_b_{i}\n")).collect();

    c.bench_function("diff_lines/50k_completely_different", |bench| {
        bench.iter(|| myers::diff_lines(black_box(&a), black_box(&b)));
    });
}

fn bench_diff_words(c: &mut Criterion) {
    // Realistic: a full function signature change
    let long_a = "    pub fn process_data(input: &[u8], config: &Config, mode: ProcessingMode, output: &mut Vec<u8>) -> Result<usize, ProcessError> {";
    let long_b = "    pub fn process_data(input: &[u8], settings: &Settings, mode: ProcessingMode, buffer: &mut Vec<u8>) -> Result<usize, DataError> {";

    c.bench_function("diff_words/long_signature", |b| {
        b.iter(|| myers::diff_words(black_box(long_a), black_box(long_b)));
    });
}

fn bench_tokenize(c: &mut Criterion) {
    // 10k lines of realistic source code
    let code: String = (0..10_000)
        .map(|i| format!("    let result_{i} = some_module::process(arg_{i}, &config.field_{i}).unwrap_or_else(|| default_{i}.clone());\n"))
        .collect();

    c.bench_function("tokenize/10k_lines", |b| {
        b.iter(|| myers::tokenize(black_box(&code)));
    });
}

// ── diff_state benchmarks ───────────────────────────────────────────────────

fn bench_find_next_chunk(c: &mut Criterion) {
    let chunks_50k = make_chunks(150_000, 50_000);
    let chunks_100k = make_chunks(300_000, 100_000);

    c.bench_function("find_next_chunk/50k_changes", |b| {
        b.iter(|| _bench::find_next_chunk(black_box(&chunks_50k), 500_000, 1, Side::A, true));
    });
    c.bench_function("find_next_chunk/100k_changes", |b| {
        b.iter(|| _bench::find_next_chunk(black_box(&chunks_100k), 1_000_000, 1, Side::A, true));
    });
}

fn bench_format_chunk_label(c: &mut Criterion) {
    let chunks_50k = make_chunks(150_000, 50_000);
    let chunks_100k = make_chunks(300_000, 100_000);

    c.bench_function("format_chunk_label/50k_changes", |b| {
        b.iter(|| _bench::format_chunk_label(black_box(&chunks_50k), Some(75_000)));
    });
    c.bench_function("format_chunk_label/100k_changes", |b| {
        b.iter(|| _bench::format_chunk_label(black_box(&chunks_100k), Some(150_000)));
    });
}

fn bench_compute_chunk_map_rects(c: &mut Criterion) {
    let chunks_50k = make_chunks(150_000, 50_000);
    let conflict_flags: Vec<bool> = chunks_50k.iter().map(|c| c.tag != DiffTag::Equal).collect();

    c.bench_function("compute_chunk_map_rects/50k_changes", |b| {
        b.iter(|| {
            _bench::compute_chunk_map_rects(
                black_box(&chunks_50k),
                3_000_000,
                800.0,
                Side::A,
                &conflict_flags,
            )
        });
    });
}

// ── Conflict detection benchmarks ───────────────────────────────────────────

fn bench_conflict_flags(c: &mut Criterion) {
    let (left_10k, right_10k) = make_merge_chunks(10_000);
    let (left_30k, right_30k) = make_merge_chunks(30_000);
    let (left_50k, right_50k) = make_merge_chunks(50_000);

    c.bench_function("conflict_flags/10k_chunks", |b| {
        b.iter(|| {
            _bench::conflict_flags(
                black_box(&left_10k),
                Side::B,
                black_box(&right_10k),
                Side::A,
            )
        });
    });
    c.bench_function("conflict_flags/30k_chunks", |b| {
        b.iter(|| {
            _bench::conflict_flags(
                black_box(&left_30k),
                Side::B,
                black_box(&right_30k),
                Side::A,
            )
        });
    });
    c.bench_function("conflict_flags/50k_chunks", |b| {
        b.iter(|| {
            _bench::conflict_flags(
                black_box(&left_50k),
                Side::B,
                black_box(&right_50k),
                Side::A,
            )
        });
    });
}

fn bench_middle_conflict_regions(c: &mut Criterion) {
    let (left_10k, right_10k) = make_merge_chunks(10_000);
    let (left_30k, right_30k) = make_merge_chunks(30_000);
    let (left_50k, right_50k) = make_merge_chunks(50_000);

    c.bench_function("middle_conflict_regions/10k_chunks", |b| {
        b.iter(|| _bench::middle_conflict_regions(black_box(&left_10k), black_box(&right_10k)));
    });
    c.bench_function("middle_conflict_regions/30k_chunks", |b| {
        b.iter(|| _bench::middle_conflict_regions(black_box(&left_30k), black_box(&right_30k)));
    });
    c.bench_function("middle_conflict_regions/50k_chunks", |b| {
        b.iter(|| _bench::middle_conflict_regions(black_box(&left_50k), black_box(&right_50k)));
    });
}

fn bench_merged_gutter_chunks(c: &mut Criterion) {
    let (left_10k, right_10k) = make_merge_chunks(10_000);
    let (left_30k, right_30k) = make_merge_chunks(30_000);

    c.bench_function("merged_gutter_chunks/10k_chunks", |b| {
        b.iter(|| {
            _bench::merged_gutter_chunks(black_box(&left_10k), black_box(&right_10k), Side::A)
        });
    });
    c.bench_function("merged_gutter_chunks/30k_chunks", |b| {
        b.iter(|| {
            _bench::merged_gutter_chunks(black_box(&left_30k), black_box(&right_30k), Side::A)
        });
    });
}

// ── VCS parsing benchmarks ──────────────────────────────────────────────────

fn bench_parse_porcelain(c: &mut Criterion) {
    // Monorepo scale: 50k entries with mixed statuses and renames
    let large: Vec<u8> = (0..50_000)
        .flat_map(|i| match i % 7 {
            0 => format!(" M src/deeply/nested/module/submodule/file_{i}.rs\0").into_bytes(),
            1 => format!("A  src/new/path/to/file_{i}.rs\0").into_bytes(),
            2 => format!("?? untracked/path/file_{i}.txt\0").into_bytes(),
            3 => format!("MM src/partially/staged/file_{i}.rs\0").into_bytes(),
            4 => format!(" D deleted/old/path/file_{i}.rs\0").into_bytes(),
            5 => {
                let mut v = format!("R  src/renamed/new_{i}.rs\0").into_bytes();
                v.extend(format!("src/renamed/old_{i}.rs\0").into_bytes());
                v
            }
            6 => format!("UU src/conflicting/file_{i}.rs\0").into_bytes(),
            _ => unreachable!(),
        })
        .collect();

    c.bench_function("parse_porcelain/50k_mixed_entries", |b| {
        b.iter(|| mergers::vcs::parse_porcelain_nul(black_box(&large)));
    });
}

// ── Merge state benchmarks ──────────────────────────────────────────────────

fn bench_merge_change_indices(c: &mut Criterion) {
    let (left, right) = make_merge_chunks(50_000);
    c.bench_function("merge_change_indices/50k_chunks", |b| {
        b.iter(|| _bench::merge_change_indices(black_box(&left), black_box(&right)));
    });
}

fn bench_find_conflict_markers(c: &mut Criterion) {
    let text_200k = make_conflict_text(200_000, 200);
    let text_500k = make_conflict_text(500_000, 500);

    c.bench_function("find_conflict_markers/200k_lines", |b| {
        b.iter(|| _bench::find_conflict_markers_in_text(black_box(&text_200k)));
    });
    c.bench_function("find_conflict_markers/500k_lines", |b| {
        b.iter(|| _bench::find_conflict_markers_in_text(black_box(&text_500k)));
    });
}

fn bench_conflict_at_cursor(c: &mut Criterion) {
    let text = make_conflict_text(500_000, 500);
    let blocks = _bench::find_conflict_blocks(&text);

    c.bench_function("conflict_at_cursor/line_1000", |b| {
        b.iter(|| _bench::conflict_at_cursor(black_box(&text), 1_000));
    });
    c.bench_function("conflict_at_cursor/line_250000", |b| {
        b.iter(|| _bench::conflict_at_cursor(black_box(&text), 250_000));
    });
    c.bench_function("conflict_at_cursor/line_490000", |b| {
        b.iter(|| _bench::conflict_at_cursor(black_box(&text), 490_000));
    });
    c.bench_function("conflict_at_cursor_fast/line_1000", |b| {
        b.iter(|| _bench::conflict_at_cursor_fast(black_box(&blocks), 1_000));
    });
    c.bench_function("conflict_at_cursor_fast/line_250000", |b| {
        b.iter(|| _bench::conflict_at_cursor_fast(black_box(&blocks), 250_000));
    });
    c.bench_function("conflict_at_cursor_fast/line_490000", |b| {
        b.iter(|| _bench::conflict_at_cursor_fast(black_box(&blocks), 490_000));
    });
}

// ── Group & main ────────────────────────────────────────────────────────────

criterion_group!(
    myers_benches,
    bench_diff_lines,
    bench_diff_completely_different,
    bench_diff_words,
    bench_tokenize,
);

criterion_group!(
    diff_state_benches,
    bench_find_next_chunk,
    bench_format_chunk_label,
    bench_compute_chunk_map_rects,
);

criterion_group!(
    conflict_benches,
    bench_conflict_flags,
    bench_middle_conflict_regions,
    bench_merged_gutter_chunks,
);

criterion_group!(vcs_benches, bench_parse_porcelain,);

criterion_group!(
    merge_state_benches,
    bench_merge_change_indices,
    bench_find_conflict_markers,
    bench_conflict_at_cursor,
);

criterion_main!(
    myers_benches,
    diff_state_benches,
    conflict_benches,
    vcs_benches,
    merge_state_benches,
);
