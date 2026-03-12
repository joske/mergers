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

/// Returns true if the input looks like a context diff (not yet supported).
fn is_context_diff(_input: &str) -> bool {
    false
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
        return Err(PatchError(
            "context diffs are not yet supported".to_string(),
        ));
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
    fn parse_patch_empty_input() {
        let result = parse_patch("");
        assert!(result.is_err());
        let result = parse_patch("   \n  \n");
        assert!(result.is_err());
    }
}
