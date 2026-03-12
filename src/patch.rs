use std::fmt;

#[derive(Debug, Clone)]
pub struct Hunk {
    pub old_start: usize,
    pub old_count: usize,
    pub new_start: usize,
    pub new_count: usize,
    pub lines: Vec<HunkLine>,
}

#[derive(Debug, Clone)]
pub enum HunkLine {
    Context(String),
    Add(String),
    Remove(String),
}

#[derive(Debug, Clone)]
pub struct FilePatch {
    pub original_path: String,
    pub hunks: Vec<Hunk>,
}

#[derive(Debug)]
pub struct PatchError(pub String);

impl fmt::Display for PatchError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for PatchError {}

/// Returns true if the input looks like a context diff.
fn is_context_diff(input: &str) -> bool {
    input.lines().any(|l| l == "***************")
}

/// Parse a unified or context diff into a list of per-file patches.
///
/// # Errors
///
/// Returns `PatchError` if the input is empty or contains an unsupported diff format.
pub fn parse_patch(input: &str) -> Result<Vec<FilePatch>, PatchError> {
    if input.trim().is_empty() {
        return Err(PatchError("empty patch input".to_string()));
    }

    if is_context_diff(input) {
        return parse_context_diff(input);
    }

    parse_unified_diff(input)
}

/// Strip an optional `a/` or `b/` prefix from a path.
fn strip_ab_prefix(path: &str) -> &str {
    path.strip_prefix("a/")
        .or_else(|| path.strip_prefix("b/"))
        .unwrap_or(path)
}

/// Strip a trailing tab-separated timestamp from a path (e.g. `file.txt\t2024-01-01 ...`).
fn strip_timestamp(path: &str) -> &str {
    match path.find('\t') {
        Some(pos) => &path[..pos],
        None => path,
    }
}

/// Parse `@@ -old_start[,old_count] +new_start[,new_count] @@` into `(old_start, old_count,
/// new_start, new_count)`.
fn parse_hunk_header(line: &str) -> Result<(usize, usize, usize, usize), PatchError> {
    let line = line.trim();
    let rest = line
        .strip_prefix("@@")
        .ok_or_else(|| PatchError(format!("invalid hunk header: {line}")))?;
    // Find the closing @@
    let rest = match rest.find("@@") {
        Some(pos) => &rest[..pos],
        None => return Err(PatchError(format!("invalid hunk header: {line}"))),
    };
    let rest = rest.trim();

    let parts: Vec<&str> = rest.split_whitespace().collect();
    if parts.len() != 2 {
        return Err(PatchError(format!("invalid hunk header: {line}")));
    }

    let old = parts[0]
        .strip_prefix('-')
        .ok_or_else(|| PatchError(format!("invalid hunk header old range: {line}")))?;
    let new = parts[1]
        .strip_prefix('+')
        .ok_or_else(|| PatchError(format!("invalid hunk header new range: {line}")))?;

    let (old_start, old_count) = parse_range(old, line)?;
    let (new_start, new_count) = parse_range(new, line)?;

    Ok((old_start, old_count, new_start, new_count))
}

fn parse_range(range: &str, line: &str) -> Result<(usize, usize), PatchError> {
    if let Some((start, count)) = range.split_once(',') {
        let start = start
            .parse::<usize>()
            .map_err(|_| PatchError(format!("invalid range number in: {line}")))?;
        let count = count
            .parse::<usize>()
            .map_err(|_| PatchError(format!("invalid range number in: {line}")))?;
        Ok((start, count))
    } else {
        let start = range
            .parse::<usize>()
            .map_err(|_| PatchError(format!("invalid range number in: {line}")))?;
        Ok((start, 1))
    }
}

/// Parse a context-range header like `*** 1,4 ****` or `--- 5 ----` into `(start, count)`.
/// Context diff ranges are inclusive (start through end), so count = end - start + 1.
/// A single number means one line, so count = 1.
fn parse_context_range(line: &str) -> Result<(usize, usize), PatchError> {
    // Extract the numeric part between the prefix stars/dashes and the trailing stars/dashes.
    let inner = line
        .trim_start_matches(['*', '-', ' '])
        .trim_end_matches(['*', '-', ' ']);
    if inner.is_empty() {
        // Empty section (e.g. pure addition with `*** 0 ****`)
        return Ok((0, 0));
    }
    if let Some((start_s, end_s)) = inner.split_once(',') {
        let start = start_s
            .trim()
            .parse::<usize>()
            .map_err(|_| PatchError(format!("invalid context range: {line}")))?;
        let end = end_s
            .trim()
            .parse::<usize>()
            .map_err(|_| PatchError(format!("invalid context range: {line}")))?;
        Ok((start, end - start + 1))
    } else {
        let start = inner
            .trim()
            .parse::<usize>()
            .map_err(|_| PatchError(format!("invalid context range: {line}")))?;
        if start == 0 {
            Ok((0, 0))
        } else {
            Ok((start, 1))
        }
    }
}

fn parse_context_diff(input: &str) -> Result<Vec<FilePatch>, PatchError> {
    let lines: Vec<&str> = input.lines().collect();
    let mut patches = Vec::new();
    let mut i = 0;

    while i < lines.len() {
        // Look for file header: `*** old_file` followed by `--- new_file`.
        if lines[i].starts_with("*** ") && !lines[i].contains("****") {
            if i + 1 >= lines.len() || !lines[i + 1].starts_with("--- ") {
                i += 1;
                continue;
            }
            // Check this is a file header, not a hunk range line.
            // Hunk range lines look like `*** 1,4 ****` — they end with `****`.
            let old_path = strip_timestamp(lines[i][4..].trim());
            let _new_path = strip_timestamp(lines[i + 1][4..].trim());
            i += 2;

            let mut hunks = Vec::new();

            // Parse hunks separated by `***************`.
            while i < lines.len() && lines[i] == "***************" {
                i += 1;
                if i >= lines.len() {
                    break;
                }

                // Old section: `*** start,end ****`
                if !lines[i].starts_with("*** ") {
                    return Err(PatchError(format!(
                        "expected old section header, got: {}",
                        lines[i]
                    )));
                }
                let (old_start, old_count) = parse_context_range(lines[i])?;
                i += 1;

                // Collect old section lines until we hit `--- start,end ----`.
                let mut old_lines: Vec<&str> = Vec::new();
                while i < lines.len() && !lines[i].starts_with("--- ") {
                    old_lines.push(lines[i]);
                    i += 1;
                }

                // New section: `--- start,end ----`
                if i >= lines.len() || !lines[i].starts_with("--- ") {
                    return Err(PatchError(
                        "expected new section header in context hunk".to_string(),
                    ));
                }
                let (new_start, new_count) = parse_context_range(lines[i])?;
                i += 1;

                // Collect new section lines until next hunk separator or file header.
                let mut new_lines: Vec<&str> = Vec::new();
                while i < lines.len()
                    && lines[i] != "***************"
                    && !(lines[i].starts_with("*** ")
                        && !lines[i].contains("****")
                        && i + 1 < lines.len()
                        && lines[i + 1].starts_with("--- "))
                {
                    new_lines.push(lines[i]);
                    i += 1;
                }

                // Merge old and new sections into HunkLine list.
                let hunk_lines = merge_context_sections(&old_lines, &new_lines)?;

                hunks.push(Hunk {
                    old_start,
                    old_count,
                    new_start,
                    new_count,
                    lines: hunk_lines,
                });
            }

            patches.push(FilePatch {
                original_path: old_path.to_string(),
                hunks,
            });
        } else {
            i += 1;
        }
    }

    Ok(patches)
}

/// Classify a context diff line by its two-character prefix.
enum CtxLineKind {
    Context,
    Changed,
    Added,
    Removed,
}

fn classify_context_line(line: &str) -> Option<(CtxLineKind, &str)> {
    if let Some(rest) = line.strip_prefix("  ") {
        Some((CtxLineKind::Context, rest))
    } else if let Some(rest) = line.strip_prefix("! ") {
        Some((CtxLineKind::Changed, rest))
    } else if let Some(rest) = line.strip_prefix("+ ") {
        Some((CtxLineKind::Added, rest))
    } else if let Some(rest) = line.strip_prefix("- ") {
        Some((CtxLineKind::Removed, rest))
    } else {
        None
    }
}

/// Merge old-section and new-section lines into a unified `HunkLine` list.
///
/// Context lines appear in both sections (we take them from whichever has them).
/// `!` (changed) lines in the old section become `Remove`, in the new section become `Add`.
/// `-` lines in the old section become `Remove`, `+` lines in the new section become `Add`.
fn merge_context_sections(
    old_lines: &[&str],
    new_lines: &[&str],
) -> Result<Vec<HunkLine>, PatchError> {
    let mut result = Vec::new();
    let mut oi = 0;
    let mut ni = 0;

    while oi < old_lines.len() || ni < new_lines.len() {
        // If old section is exhausted, drain new section.
        if oi >= old_lines.len() {
            while ni < new_lines.len() {
                let (kind, text) = classify_context_line(new_lines[ni])
                    .ok_or_else(|| PatchError(format!("bad context line: {}", new_lines[ni])))?;
                match kind {
                    CtxLineKind::Context => result.push(HunkLine::Context(text.to_string())),
                    CtxLineKind::Added | CtxLineKind::Changed => {
                        result.push(HunkLine::Add(text.to_string()));
                    }
                    CtxLineKind::Removed => result.push(HunkLine::Remove(text.to_string())),
                }
                ni += 1;
            }
            break;
        }
        // If new section is exhausted, drain old section.
        if ni >= new_lines.len() {
            while oi < old_lines.len() {
                let (kind, text) = classify_context_line(old_lines[oi])
                    .ok_or_else(|| PatchError(format!("bad context line: {}", old_lines[oi])))?;
                match kind {
                    CtxLineKind::Context => result.push(HunkLine::Context(text.to_string())),
                    CtxLineKind::Removed | CtxLineKind::Changed => {
                        result.push(HunkLine::Remove(text.to_string()));
                    }
                    CtxLineKind::Added => result.push(HunkLine::Add(text.to_string())),
                }
                oi += 1;
            }
            break;
        }

        let old_class = classify_context_line(old_lines[oi]);
        let new_class = classify_context_line(new_lines[ni]);

        match (&old_class, &new_class) {
            // Changed lines: old `!` become Remove, new `!` become Add.
            (Some((CtxLineKind::Changed, _)), _) => {
                // Emit all consecutive old `!` lines as Remove.
                while oi < old_lines.len() {
                    if let Some((CtxLineKind::Changed, text)) = classify_context_line(old_lines[oi])
                    {
                        result.push(HunkLine::Remove(text.to_string()));
                        oi += 1;
                    } else {
                        break;
                    }
                }
                // Emit all consecutive new `!` lines as Add.
                while ni < new_lines.len() {
                    if let Some((CtxLineKind::Changed, text)) = classify_context_line(new_lines[ni])
                    {
                        result.push(HunkLine::Add(text.to_string()));
                        ni += 1;
                    } else {
                        break;
                    }
                }
            }
            // Old section has removed lines.
            (Some((CtxLineKind::Removed, text)), _) => {
                result.push(HunkLine::Remove(text.to_string()));
                oi += 1;
            }
            // New section has added lines.
            (_, Some((CtxLineKind::Added, text))) => {
                result.push(HunkLine::Add(text.to_string()));
                ni += 1;
            }
            // Context in old, something else in new — emit old context and advance both.
            (Some((CtxLineKind::Context, text)), _) => {
                result.push(HunkLine::Context(text.to_string()));
                oi += 1;
                ni += 1;
            }
            _ => {
                return Err(PatchError(format!(
                    "unexpected context diff line combination: {:?} / {:?}",
                    old_lines.get(oi),
                    new_lines.get(ni)
                )));
            }
        }
    }

    Ok(result)
}

/// Apply a list of hunks to the original file content, producing the patched text.
///
/// # Errors
///
/// Returns `PatchError` if a hunk references lines beyond the original content or if
/// context/remove lines don't match the original.
pub fn apply_hunks(original: &str, hunks: &[Hunk]) -> Result<String, PatchError> {
    let orig_lines: Vec<&str> = if original.is_empty() {
        Vec::new()
    } else {
        original.lines().collect()
    };
    let mut output = Vec::new();
    // 0-based position in orig_lines
    let mut pos: usize = 0;

    for hunk in hunks {
        // old_start is 1-based; convert to 0-based index
        let hunk_start = if hunk.old_start == 0 {
            0
        } else {
            hunk.old_start - 1
        };

        if hunk_start < pos {
            return Err(PatchError(format!(
                "overlapping hunks: expected position >= {}, but hunk starts at {}",
                pos + 1,
                hunk.old_start
            )));
        }

        // Copy unchanged lines before this hunk
        for line in &orig_lines[pos..hunk_start] {
            output.push((*line).to_string());
        }
        pos = hunk_start;

        for hl in &hunk.lines {
            match hl {
                HunkLine::Context(text) | HunkLine::Remove(text) => {
                    if pos >= orig_lines.len() {
                        return Err(PatchError(format!(
                            "hunk references line {} but original has only {} lines",
                            pos + 1,
                            orig_lines.len()
                        )));
                    }
                    if orig_lines[pos] != text.as_str() {
                        return Err(PatchError(format!(
                            "mismatch at line {}: expected {:?}, got {:?}",
                            pos + 1,
                            text,
                            orig_lines[pos]
                        )));
                    }
                    if matches!(hl, HunkLine::Context(_)) {
                        output.push(text.clone());
                    }
                    pos += 1;
                }
                HunkLine::Add(text) => {
                    output.push(text.clone());
                }
            }
        }
    }

    // Copy remaining original lines after the last hunk
    for line in &orig_lines[pos..] {
        output.push((*line).to_string());
    }

    Ok(output.join("\n"))
}

fn parse_unified_diff(input: &str) -> Result<Vec<FilePatch>, PatchError> {
    let lines: Vec<&str> = input.lines().collect();
    let mut patches = Vec::new();
    let mut i = 0;

    while i < lines.len() {
        // Look for a --- / +++ header pair.
        if lines[i].starts_with("--- ") && i + 1 < lines.len() && lines[i + 1].starts_with("+++ ") {
            let old_path = &lines[i][4..];
            let old_path = strip_timestamp(strip_ab_prefix(old_path.trim()));

            // Skip the +++ line (we use the old path as canonical).
            i += 2;

            let mut hunks = Vec::new();

            // Parse consecutive hunks.
            while i < lines.len() && lines[i].starts_with("@@") {
                let (old_start, old_count, new_start, new_count) = parse_hunk_header(lines[i])?;
                i += 1;

                let mut hunk_lines = Vec::new();

                while i < lines.len() {
                    let line = lines[i];
                    if line.starts_with("--- ") || line.starts_with("@@") {
                        break;
                    }
                    if line.starts_with("\\ ") {
                        // "\ No newline at end of file" — skip.
                        i += 1;
                        continue;
                    }
                    if let Some(rest) = line.strip_prefix('+') {
                        hunk_lines.push(HunkLine::Add(rest.to_string()));
                    } else if let Some(rest) = line.strip_prefix('-') {
                        hunk_lines.push(HunkLine::Remove(rest.to_string()));
                    } else if let Some(rest) = line.strip_prefix(' ') {
                        hunk_lines.push(HunkLine::Context(rest.to_string()));
                    } else if line.is_empty() {
                        // Empty line in a hunk is context with empty content.
                        hunk_lines.push(HunkLine::Context(String::new()));
                    } else {
                        // Unknown line — end of this hunk.
                        break;
                    }
                    i += 1;
                }

                hunks.push(Hunk {
                    old_start,
                    old_count,
                    new_start,
                    new_count,
                    lines: hunk_lines,
                });
            }

            patches.push(FilePatch {
                original_path: old_path.to_string(),
                hunks,
            });
        } else {
            i += 1;
        }
    }

    Ok(patches)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_unified_single_file() {
        let diff = "\
--- a/hello.txt
+++ b/hello.txt
@@ -1,3 +1,4 @@
 line1
-line2
+line2modified
+line2b
 line3
";
        let patches = parse_patch(diff).unwrap();
        assert_eq!(patches.len(), 1);
        assert_eq!(patches[0].original_path, "hello.txt");
        assert_eq!(patches[0].hunks.len(), 1);

        let hunk = &patches[0].hunks[0];
        assert_eq!(hunk.old_start, 1);
        assert_eq!(hunk.old_count, 3);
        assert_eq!(hunk.new_start, 1);
        assert_eq!(hunk.new_count, 4);
        assert_eq!(hunk.lines.len(), 5);

        assert!(matches!(&hunk.lines[0], HunkLine::Context(s) if s == "line1"));
        assert!(matches!(&hunk.lines[1], HunkLine::Remove(s) if s == "line2"));
        assert!(matches!(&hunk.lines[2], HunkLine::Add(s) if s == "line2modified"));
        assert!(matches!(&hunk.lines[3], HunkLine::Add(s) if s == "line2b"));
        assert!(matches!(&hunk.lines[4], HunkLine::Context(s) if s == "line3"));
    }

    #[test]
    fn parse_unified_multi_file() {
        let diff = "\
--- a/file1.txt
+++ b/file1.txt
@@ -1,2 +1,2 @@
-old
+new
 same
--- a/file2.txt
+++ b/file2.txt
@@ -1 +1 @@
-alpha
+beta
";
        let patches = parse_patch(diff).unwrap();
        assert_eq!(patches.len(), 2);
        assert_eq!(patches[0].original_path, "file1.txt");
        assert_eq!(patches[1].original_path, "file2.txt");
        assert_eq!(patches[0].hunks.len(), 1);
        assert_eq!(patches[1].hunks.len(), 1);

        // Second file: single-number range means count=1.
        let hunk = &patches[1].hunks[0];
        assert_eq!(hunk.old_start, 1);
        assert_eq!(hunk.old_count, 1);
        assert_eq!(hunk.new_start, 1);
        assert_eq!(hunk.new_count, 1);
    }

    #[test]
    fn parse_unified_no_prefix() {
        let diff = "\
--- hello.txt\t2024-01-01 00:00:00.000000000 +0000
+++ hello.txt\t2024-01-02 00:00:00.000000000 +0000
@@ -1,2 +1,2 @@
-aaa
+bbb
 ccc
";
        let patches = parse_patch(diff).unwrap();
        assert_eq!(patches.len(), 1);
        assert_eq!(patches[0].original_path, "hello.txt");
    }

    #[test]
    fn parse_context_single_file() {
        let diff = "\
*** hello.txt\t2024-01-01 00:00:00.000000000 +0000
--- hello.txt\t2024-01-02 00:00:00.000000000 +0000
***************
*** 1,4 ****
  line1
! line2
  line3
  line4
--- 1,4 ----
  line1
! line2modified
  line3
  line4
";
        let patches = parse_patch(diff).unwrap();
        assert_eq!(patches.len(), 1);
        assert_eq!(patches[0].original_path, "hello.txt");
        assert_eq!(patches[0].hunks.len(), 1);

        let hunk = &patches[0].hunks[0];
        assert_eq!(hunk.old_start, 1);
        assert_eq!(hunk.old_count, 4);
        assert_eq!(hunk.new_start, 1);
        assert_eq!(hunk.new_count, 4);
        assert_eq!(hunk.lines.len(), 5);

        assert!(matches!(&hunk.lines[0], HunkLine::Context(s) if s == "line1"));
        assert!(matches!(&hunk.lines[1], HunkLine::Remove(s) if s == "line2"));
        assert!(matches!(&hunk.lines[2], HunkLine::Add(s) if s == "line2modified"));
        assert!(matches!(&hunk.lines[3], HunkLine::Context(s) if s == "line3"));
        assert!(matches!(&hunk.lines[4], HunkLine::Context(s) if s == "line4"));
    }

    #[test]
    fn parse_context_add_only() {
        let diff = "\
*** hello.txt\t2024-01-01 00:00:00.000000000 +0000
--- hello.txt\t2024-01-02 00:00:00.000000000 +0000
***************
*** 1,2 ****
--- 1,4 ----
  line1
+ added1
+ added2
  line2
";
        let patches = parse_patch(diff).unwrap();
        assert_eq!(patches.len(), 1);

        let hunk = &patches[0].hunks[0];
        assert_eq!(hunk.old_start, 1);
        assert_eq!(hunk.old_count, 2);
        assert_eq!(hunk.new_start, 1);
        assert_eq!(hunk.new_count, 4);
        assert_eq!(hunk.lines.len(), 4);

        assert!(matches!(&hunk.lines[0], HunkLine::Context(s) if s == "line1"));
        assert!(matches!(&hunk.lines[1], HunkLine::Add(s) if s == "added1"));
        assert!(matches!(&hunk.lines[2], HunkLine::Add(s) if s == "added2"));
        assert!(matches!(&hunk.lines[3], HunkLine::Context(s) if s == "line2"));
    }

    #[test]
    fn parse_patch_empty_input() {
        let result = parse_patch("");
        assert!(result.is_err());
        let result = parse_patch("   \n  \n");
        assert!(result.is_err());
    }

    #[test]
    fn apply_hunks_simple_replace() {
        let original = "line1\nline2\nline3";
        let hunks = vec![Hunk {
            old_start: 2,
            old_count: 1,
            new_start: 2,
            new_count: 1,
            lines: vec![
                HunkLine::Remove("line2".to_string()),
                HunkLine::Add("replaced".to_string()),
            ],
        }];
        let result = apply_hunks(original, &hunks).unwrap();
        assert_eq!(result, "line1\nreplaced\nline3");
    }

    #[test]
    fn apply_hunks_addition() {
        let original = "line1\nline2\nline3";
        let hunks = vec![Hunk {
            old_start: 2,
            old_count: 1,
            new_start: 2,
            new_count: 2,
            lines: vec![
                HunkLine::Context("line2".to_string()),
                HunkLine::Add("inserted".to_string()),
            ],
        }];
        let result = apply_hunks(original, &hunks).unwrap();
        assert_eq!(result, "line1\nline2\ninserted\nline3");
    }

    #[test]
    fn apply_hunks_deletion() {
        let original = "line1\nline2\nline3";
        let hunks = vec![Hunk {
            old_start: 2,
            old_count: 1,
            new_start: 2,
            new_count: 0,
            lines: vec![HunkLine::Remove("line2".to_string())],
        }];
        let result = apply_hunks(original, &hunks).unwrap();
        assert_eq!(result, "line1\nline3");
    }

    #[test]
    fn apply_hunks_multiple() {
        let original = "a\nb\nc\nd\ne\nf";
        let hunks = vec![
            Hunk {
                old_start: 2,
                old_count: 1,
                new_start: 2,
                new_count: 1,
                lines: vec![
                    HunkLine::Remove("b".to_string()),
                    HunkLine::Add("B".to_string()),
                ],
            },
            Hunk {
                old_start: 5,
                old_count: 1,
                new_start: 5,
                new_count: 1,
                lines: vec![
                    HunkLine::Remove("e".to_string()),
                    HunkLine::Add("E".to_string()),
                ],
            },
        ];
        let result = apply_hunks(original, &hunks).unwrap();
        assert_eq!(result, "a\nB\nc\nd\nE\nf");
    }

    #[test]
    fn apply_hunks_to_empty_original() {
        let original = "";
        let hunks = vec![Hunk {
            old_start: 0,
            old_count: 0,
            new_start: 1,
            new_count: 2,
            lines: vec![
                HunkLine::Add("new1".to_string()),
                HunkLine::Add("new2".to_string()),
            ],
        }];
        let result = apply_hunks(original, &hunks).unwrap();
        assert_eq!(result, "new1\nnew2");
    }
}
