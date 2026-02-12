//! Myers O(NP) sequence comparison algorithm.
//!
//! Based on: "An O(NP) Sequence Comparison Algorithm" (1989)
//! by Sun Wu, Udi Manber, Gene Myers, Webb Miller
//! http://research.janelia.org/myers/Papers/np_diff.pdf

use std::collections::HashSet;
use std::hash::Hash;

/// Classification of a diff region.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiffTag {
    Equal,
    Replace,
    Delete,
    Insert,
}

/// A single diff operation with half-open ranges `[start..end)` into both sequences.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DiffChunk {
    pub tag: DiffTag,
    pub start_a: usize,
    pub end_a: usize,
    pub start_b: usize,
    pub end_b: usize,
}

/// Compute diff opcodes between two sequences using the Myers O(NP) algorithm.
///
/// Returns a list of [`DiffChunk`]s describing how to transform `a` into `b`.
pub fn diff<T: Eq + Hash>(a: &[T], b: &[T]) -> Vec<DiffChunk> {
    if a.is_empty() && b.is_empty() {
        return vec![];
    }

    // Preprocessing: strip common prefix and suffix
    let prefix = common_prefix_len(a, b);
    let suffix = common_suffix_len(&a[prefix..], &b[prefix..]);
    let a_trimmed = &a[prefix..a.len() - suffix];
    let b_trimmed = &b[prefix..b.len() - suffix];

    // Preprocessing: discard non-matching lines
    let (a_filtered, aindex, b_filtered, bindex, discarded) =
        discard_nonmatching(a_trimmed, b_trimmed);

    // Core Myers O(NP) algorithm
    let mut arena = Vec::new();
    let lastsnake = if discarded {
        myers_core(&a_filtered, &b_filtered, &mut arena)
    } else {
        myers_core(a_trimmed, b_trimmed, &mut arena)
    };

    // Build matching blocks from snake chain
    let mut blocks = build_matching_blocks(
        lastsnake, &arena, prefix, suffix, &aindex, &bindex, discarded, a.len(), b.len(),
    );

    // Postprocess: merge adjacent matching blocks
    postprocess(&mut blocks, a, b);

    // Convert to opcodes
    blocks_to_opcodes(&blocks)
}

/// Convenience: diff two strings line by line.
pub fn diff_lines(a: &str, b: &str) -> Vec<DiffChunk> {
    let a_lines: Vec<&str> = a.lines().collect();
    let b_lines: Vec<&str> = b.lines().collect();
    diff(&a_lines, &b_lines)
}

// --- Internal types ---

#[derive(Debug, Clone, Copy)]
struct MatchingBlock {
    a: usize,
    b: usize,
    len: usize,
}

#[derive(Debug)]
struct SnakeNode {
    parent: Option<usize>,
    x: usize,
    y: usize,
    len: usize,
}

/// Bounds-checked access to the fp array. Out-of-bounds reads return (-1, None),
/// matching Python's behavior where negative indices hit uninitialized positions.
struct Fp {
    data: Vec<(isize, Option<usize>)>,
}

impl Fp {
    fn new(size: usize) -> Self {
        Fp {
            data: vec![(-1, None); size],
        }
    }

    fn get(&self, km: isize) -> (isize, Option<usize>) {
        if km >= 0 && (km as usize) < self.data.len() {
            self.data[km as usize]
        } else {
            (-1, None)
        }
    }

    fn set(&mut self, km: isize, val: (isize, Option<usize>)) {
        if km >= 0 && (km as usize) < self.data.len() {
            self.data[km as usize] = val;
        }
    }
}

// --- Preprocessing ---

fn common_prefix_len<T: PartialEq>(a: &[T], b: &[T]) -> usize {
    a.iter().zip(b.iter()).take_while(|(x, y)| x == y).count()
}

fn common_suffix_len<T: PartialEq>(a: &[T], b: &[T]) -> usize {
    a.iter()
        .rev()
        .zip(b.iter().rev())
        .take_while(|(x, y)| x == y)
        .count()
}

/// Discard lines that don't appear in the other sequence.
/// Only activates when >10 lines would be discarded (heuristic from Meld).
fn discard_nonmatching<'a, T: Eq + Hash>(
    a: &'a [T],
    b: &'a [T],
) -> (Vec<&'a T>, Vec<usize>, Vec<&'a T>, Vec<usize>, bool) {
    if a.is_empty() || b.is_empty() {
        return (vec![], vec![], vec![], vec![], false);
    }

    fn index_matching<'a, T: Eq + Hash>(
        needle_set: &HashSet<&T>,
        haystack: &'a [T],
    ) -> (Vec<&'a T>, Vec<usize>) {
        let mut matches = Vec::new();
        let mut index = Vec::new();
        for (i, item) in haystack.iter().enumerate() {
            if needle_set.contains(item) {
                matches.push(item);
                index.push(i);
            }
        }
        (matches, index)
    }

    let aset: HashSet<&T> = a.iter().collect();
    let bset: HashSet<&T> = b.iter().collect();
    let (b_filtered, bindex) = index_matching(&aset, b);
    let (a_filtered, aindex) = index_matching(&bset, a);

    let discarded =
        (b.len() - b_filtered.len() > 10) || (a.len() - a_filtered.len() > 10);

    if discarded {
        (a_filtered, aindex, b_filtered, bindex, true)
    } else {
        (vec![], vec![], vec![], vec![], false)
    }
}

// --- Core O(NP) Algorithm ---

/// Run the core Myers O(NP) algorithm on two sequences.
/// Returns the arena index of the last snake node, or None if either sequence is empty.
fn myers_core<T: PartialEq>(
    a: &[T],
    b: &[T],
    arena: &mut Vec<SnakeNode>,
) -> Option<usize> {
    let m = a.len();
    let n = b.len();

    if m == 0 || n == 0 {
        return None;
    }

    let middle = (m + 1) as isize;
    let delta = n as isize - m as isize + middle;
    let dmin = middle.min(delta);
    let dmax = middle.max(delta);
    let size = n + m + 2;
    let mut fp = Fp::new(size);

    for p in 0isize.. {
        // Forward sweep: km from (dmin - p) to (delta - 1)
        let mut yv: isize = -1;
        let mut node_v: Option<usize> = None;
        {
            let mut km = dmin - p;
            while km < delta {
                let t = fp.get(km + 1);
                if yv < t.0 {
                    yv = t.0;
                    node_v = t.1;
                } else {
                    yv += 1;
                }
                let x = yv - km + middle;
                if x >= 0
                    && (x as usize) < m
                    && yv >= 0
                    && (yv as usize) < n
                    && a[x as usize] == b[yv as usize]
                {
                    let snake_x = x as usize;
                    let mut xi = x as usize + 1;
                    let mut yi = yv as usize + 1;
                    while xi < m && yi < n && a[xi] == b[yi] {
                        xi += 1;
                        yi += 1;
                    }
                    let snake_len = xi - snake_x;
                    arena.push(SnakeNode {
                        parent: node_v,
                        x: snake_x,
                        y: yv as usize,
                        len: snake_len,
                    });
                    node_v = Some(arena.len() - 1);
                    yv = yi as isize;
                }
                fp.set(km, (yv, node_v));
                km += 1;
            }
        }

        // Backward sweep: km from (dmax + p) down to (delta + 1)
        let mut yh: isize = -1;
        let mut node_h: Option<usize> = None;
        {
            let mut km = dmax + p;
            while km > delta {
                let t = fp.get(km - 1);
                if yh <= t.0 {
                    yh = t.0;
                    node_h = t.1;
                    yh += 1;
                }
                let x = yh - km + middle;
                if x >= 0
                    && (x as usize) < m
                    && yh >= 0
                    && (yh as usize) < n
                    && a[x as usize] == b[yh as usize]
                {
                    let snake_x = x as usize;
                    let mut xi = x as usize + 1;
                    let mut yi = yh as usize + 1;
                    while xi < m && yi < n && a[xi] == b[yi] {
                        xi += 1;
                        yi += 1;
                    }
                    let snake_len = xi - snake_x;
                    arena.push(SnakeNode {
                        parent: node_h,
                        x: snake_x,
                        y: yh as usize,
                        len: snake_len,
                    });
                    node_h = Some(arena.len() - 1);
                    yh = yi as isize;
                }
                fp.set(km, (yh, node_h));
                km -= 1;
            }
        }

        // Delta diagonal
        let (mut y, mut node) = if yv < yh {
            fp.get(delta + 1)
        } else {
            let t = fp.get(delta - 1);
            (t.0 + 1, t.1)
        };
        let x = y - delta + middle;
        if x >= 0
            && (x as usize) < m
            && y >= 0
            && (y as usize) < n
            && a[x as usize] == b[y as usize]
        {
            let snake_x = x as usize;
            let mut xi = x as usize + 1;
            let mut yi = y as usize + 1;
            while xi < m && yi < n && a[xi] == b[yi] {
                xi += 1;
                yi += 1;
            }
            let snake_len = xi - snake_x;
            arena.push(SnakeNode {
                parent: node,
                x: snake_x,
                y: y as usize,
                len: snake_len,
            });
            node = Some(arena.len() - 1);
            y = yi as isize;
        }
        fp.set(delta, (y, node));

        if y >= n as isize {
            return node;
        }
    }

    unreachable!()
}

// --- Build Matching Blocks ---

fn build_matching_blocks(
    lastsnake: Option<usize>,
    arena: &[SnakeNode],
    common_prefix: usize,
    common_suffix: usize,
    aindex: &[usize],
    bindex: &[usize],
    lines_discarded: bool,
    total_a: usize,
    total_b: usize,
) -> Vec<MatchingBlock> {
    // Collect blocks in reverse order (walking snake chain from last to first),
    // then reverse at the end.
    let mut blocks = Vec::new();

    let mut current = lastsnake;
    while let Some(idx) = current {
        let node = &arena[idx];
        let snake = node.len;
        current = node.parent;

        if lines_discarded {
            let mut xi = node.x + snake - 1;
            let mut yi = node.y + snake - 1;
            let mut xprev = aindex[xi] + common_prefix;
            let mut yprev = bindex[yi] + common_prefix;

            if snake > 1 {
                let mut newsnake = 1usize;
                for _ in 1..snake {
                    xi -= 1;
                    yi -= 1;
                    let xnext = aindex[xi] + common_prefix;
                    let ynext = bindex[yi] + common_prefix;
                    if xprev - xnext != 1 || yprev - ynext != 1 {
                        blocks.push(MatchingBlock {
                            a: xprev,
                            b: yprev,
                            len: newsnake,
                        });
                        newsnake = 0;
                    }
                    xprev = xnext;
                    yprev = ynext;
                    newsnake += 1;
                }
                blocks.push(MatchingBlock {
                    a: xprev,
                    b: yprev,
                    len: newsnake,
                });
            } else {
                blocks.push(MatchingBlock {
                    a: xprev,
                    b: yprev,
                    len: snake,
                });
            }
        } else {
            blocks.push(MatchingBlock {
                a: node.x + common_prefix,
                b: node.y + common_prefix,
                len: snake,
            });
        }
    }

    blocks.reverse();

    // Prepend common prefix block
    if common_prefix > 0 {
        blocks.insert(0, MatchingBlock {
            a: 0,
            b: 0,
            len: common_prefix,
        });
    }

    // Append common suffix block
    if common_suffix > 0 {
        blocks.push(MatchingBlock {
            a: total_a - common_suffix,
            b: total_b - common_suffix,
            len: common_suffix,
        });
    }

    // Sentinel
    blocks.push(MatchingBlock {
        a: total_a,
        b: total_b,
        len: 0,
    });

    blocks
}

// --- Postprocessing ---

/// Merge adjacent matching blocks where the gap between them is actually equal content.
/// This reduces "chaff" from the greedy algorithm, making results more human-readable.
fn postprocess<T: PartialEq>(blocks: &mut Vec<MatchingBlock>, a: &[T], b: &[T]) {
    let mut result = vec![*blocks.last().unwrap()]; // start with sentinel
    let mut i = blocks.len() as isize - 2;

    while i >= 0 {
        let mut cur = blocks[i as usize];
        i -= 1;

        while i >= 0 {
            let prev = blocks[i as usize];
            if prev.b + prev.len == cur.b || prev.a + prev.len == cur.a {
                if cur.a >= prev.len && cur.b >= prev.len {
                    let slice_a = &a[cur.a - prev.len..cur.a];
                    let slice_b = &b[cur.b - prev.len..cur.b];
                    if slice_a == slice_b {
                        cur.a -= prev.len;
                        cur.b -= prev.len;
                        cur.len += prev.len;
                        i -= 1;
                        continue;
                    }
                }
            }
            break;
        }

        result.push(cur);
    }

    result.reverse();
    *blocks = result;
}

// --- Opcode Generation ---

fn blocks_to_opcodes(blocks: &[MatchingBlock]) -> Vec<DiffChunk> {
    let mut opcodes = Vec::new();
    let mut i = 0usize;
    let mut j = 0usize;

    for block in blocks {
        let ai = block.a;
        let bj = block.b;
        let size = block.len;

        if i < ai && j < bj {
            opcodes.push(DiffChunk {
                tag: DiffTag::Replace,
                start_a: i,
                end_a: ai,
                start_b: j,
                end_b: bj,
            });
        } else if i < ai {
            opcodes.push(DiffChunk {
                tag: DiffTag::Delete,
                start_a: i,
                end_a: ai,
                start_b: j,
                end_b: bj,
            });
        } else if j < bj {
            opcodes.push(DiffChunk {
                tag: DiffTag::Insert,
                start_a: i,
                end_a: ai,
                start_b: j,
                end_b: bj,
            });
        }

        i = ai + size;
        j = bj + size;

        if size > 0 {
            opcodes.push(DiffChunk {
                tag: DiffTag::Equal,
                start_a: ai,
                end_a: i,
                start_b: bj,
                end_b: j,
            });
        }
    }

    opcodes
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_both_empty() {
        let a: Vec<&str> = vec![];
        let b: Vec<&str> = vec![];
        assert!(diff(&a, &b).is_empty());
    }

    #[test]
    fn test_identical() {
        let a = vec!["a", "b", "c"];
        let chunks = diff(&a, &a);
        assert_eq!(chunks, vec![DiffChunk {
            tag: DiffTag::Equal,
            start_a: 0, end_a: 3,
            start_b: 0, end_b: 3,
        }]);
    }

    #[test]
    fn test_completely_different() {
        let a = vec!["a", "b"];
        let b = vec!["c", "d"];
        let chunks = diff(&a, &b);
        assert_eq!(chunks.len(), 1);
        assert_eq!(chunks[0].tag, DiffTag::Replace);
    }

    #[test]
    fn test_empty_a() {
        let a: Vec<&str> = vec![];
        let b = vec!["a", "b"];
        let chunks = diff(&a, &b);
        assert_eq!(chunks, vec![DiffChunk {
            tag: DiffTag::Insert,
            start_a: 0, end_a: 0,
            start_b: 0, end_b: 2,
        }]);
    }

    #[test]
    fn test_empty_b() {
        let a = vec!["a", "b"];
        let b: Vec<&str> = vec![];
        let chunks = diff(&a, &b);
        assert_eq!(chunks, vec![DiffChunk {
            tag: DiffTag::Delete,
            start_a: 0, end_a: 2,
            start_b: 0, end_b: 0,
        }]);
    }

    #[test]
    fn test_insert_at_end() {
        let a = vec!["a", "b"];
        let b = vec!["a", "b", "c"];
        let chunks = diff(&a, &b);
        assert_eq!(chunks, vec![
            DiffChunk { tag: DiffTag::Equal, start_a: 0, end_a: 2, start_b: 0, end_b: 2 },
            DiffChunk { tag: DiffTag::Insert, start_a: 2, end_a: 2, start_b: 2, end_b: 3 },
        ]);
    }

    #[test]
    fn test_insert_at_start() {
        let a = vec!["b", "c"];
        let b = vec!["a", "b", "c"];
        let chunks = diff(&a, &b);
        assert_eq!(chunks, vec![
            DiffChunk { tag: DiffTag::Insert, start_a: 0, end_a: 0, start_b: 0, end_b: 1 },
            DiffChunk { tag: DiffTag::Equal, start_a: 0, end_a: 2, start_b: 1, end_b: 3 },
        ]);
    }

    #[test]
    fn test_delete_from_middle() {
        let a = vec!["a", "b", "c"];
        let b = vec!["a", "c"];
        let chunks = diff(&a, &b);
        assert_eq!(chunks, vec![
            DiffChunk { tag: DiffTag::Equal, start_a: 0, end_a: 1, start_b: 0, end_b: 1 },
            DiffChunk { tag: DiffTag::Delete, start_a: 1, end_a: 2, start_b: 1, end_b: 1 },
            DiffChunk { tag: DiffTag::Equal, start_a: 2, end_a: 3, start_b: 1, end_b: 2 },
        ]);
    }

    #[test]
    fn test_replace_in_middle() {
        let a = vec!["a", "b", "c"];
        let b = vec!["a", "X", "c"];
        let chunks = diff(&a, &b);
        assert_eq!(chunks.len(), 3);
        assert_eq!(chunks[0].tag, DiffTag::Equal);
        assert_eq!(chunks[1].tag, DiffTag::Replace);
        assert_eq!(chunks[2].tag, DiffTag::Equal);
    }

    #[test]
    fn test_diff_lines() {
        let a = "line1\nline2\nline3";
        let b = "line1\nchanged\nline3";
        let chunks = diff_lines(a, b);
        assert_eq!(chunks.len(), 3);
        assert_eq!(chunks[0].tag, DiffTag::Equal);
        assert_eq!(chunks[1].tag, DiffTag::Replace);
        assert_eq!(chunks[2].tag, DiffTag::Equal);
    }

    #[test]
    fn test_opcodes_cover_full_range() {
        let a = vec!["a", "b", "c", "d", "e"];
        let b = vec!["a", "x", "c", "y", "e"];
        let chunks = diff(&a, &b);

        let mut pos_a = 0;
        let mut pos_b = 0;
        for chunk in &chunks {
            assert_eq!(chunk.start_a, pos_a, "gap in a coverage");
            assert_eq!(chunk.start_b, pos_b, "gap in b coverage");
            pos_a = chunk.end_a;
            pos_b = chunk.end_b;
        }
        assert_eq!(pos_a, a.len(), "a not fully covered");
        assert_eq!(pos_b, b.len(), "b not fully covered");
    }

    #[test]
    fn test_large_with_discarding() {
        // Build sequences where many lines are unique to one side,
        // triggering the discard heuristic (>10 lines discarded).
        let mut a: Vec<String> = (0..50).map(|i| format!("unique_a_{i}")).collect();
        let mut b: Vec<String> = (0..50).map(|i| format!("unique_b_{i}")).collect();
        // Add common lines at the end
        for i in 0..5 {
            let common = format!("common_{i}");
            a.push(common.clone());
            b.push(common);
        }
        let chunks = diff(&a, &b);
        assert!(!chunks.is_empty());

        // Verify coverage
        let mut pos_a = 0;
        let mut pos_b = 0;
        for chunk in &chunks {
            assert_eq!(chunk.start_a, pos_a);
            assert_eq!(chunk.start_b, pos_b);
            pos_a = chunk.end_a;
            pos_b = chunk.end_b;
        }
        assert_eq!(pos_a, a.len());
        assert_eq!(pos_b, b.len());
    }

    #[test]
    fn test_single_element_sequences() {
        assert_eq!(diff(&["a"], &["a"]), vec![
            DiffChunk { tag: DiffTag::Equal, start_a: 0, end_a: 1, start_b: 0, end_b: 1 },
        ]);
        assert_eq!(diff(&["a"], &["b"]), vec![
            DiffChunk { tag: DiffTag::Replace, start_a: 0, end_a: 1, start_b: 0, end_b: 1 },
        ]);
    }

    #[test]
    fn test_common_prefix_only() {
        let a = vec!["a", "b", "c"];
        let b = vec!["a", "b", "x", "y"];
        let chunks = diff(&a, &b);
        assert_eq!(chunks[0].tag, DiffTag::Equal);
        assert_eq!(chunks[0].end_a, 2);
    }

    #[test]
    fn test_common_suffix_only() {
        let a = vec!["x", "b", "c"];
        let b = vec!["y", "b", "c"];
        let chunks = diff(&a, &b);
        let last = chunks.last().unwrap();
        assert_eq!(last.tag, DiffTag::Equal);
    }
}
