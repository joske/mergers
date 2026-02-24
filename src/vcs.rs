use std::{
    path::{Path, PathBuf},
    process::Command,
};

#[derive(Debug, Clone, PartialEq)]
pub enum VcsStatus {
    Modified,
    Added,
    Deleted,
    Renamed,
    Untracked,
}

#[derive(Debug, Clone)]
pub struct VcsEntry {
    pub rel_path: String,
    pub status: VcsStatus,
}

pub fn is_git_repo(dir: &Path) -> bool {
    Command::new("git")
        .args([
            "-C",
            &dir.to_string_lossy(),
            "rev-parse",
            "--is-inside-work-tree",
        ])
        .output()
        .map(|o| String::from_utf8_lossy(&o.stdout).trim() == "true")
        .unwrap_or(false)
}

pub fn repo_root(dir: &Path) -> Option<PathBuf> {
    let output = Command::new("git")
        .args(["-C", &dir.to_string_lossy(), "rev-parse", "--show-toplevel"])
        .output()
        .ok()?;
    if output.status.success() {
        Some(PathBuf::from(
            String::from_utf8_lossy(&output.stdout).trim(),
        ))
    } else {
        None
    }
}

pub fn changed_files(repo_root: &Path) -> Vec<VcsEntry> {
    let output = match Command::new("git")
        .args(["-C", &repo_root.to_string_lossy(), "status", "--porcelain"])
        .output()
    {
        Ok(o) if o.status.success() => o,
        _ => return Vec::new(),
    };

    let text = String::from_utf8_lossy(&output.stdout);
    parse_porcelain(&text)
}

/// Parse `git status --porcelain` output into `VcsEntry` items.
pub fn parse_porcelain(text: &str) -> Vec<VcsEntry> {
    let mut entries = Vec::new();

    for line in text.lines() {
        if line.len() < 3 {
            continue;
        }
        let xy = &line[..2];
        let path_part = &line[3..];

        let status = match xy {
            "??" => VcsStatus::Untracked,
            _ if xy.starts_with('R') => VcsStatus::Renamed,
            _ if xy.starts_with('D') || xy.ends_with('D') => VcsStatus::Deleted,
            _ if xy.starts_with('A') => VcsStatus::Added,
            _ if xy.contains('M') => VcsStatus::Modified,
            _ => continue,
        };

        let rel_path = if status == VcsStatus::Renamed {
            // Porcelain format for renames: "old -> new"
            path_part
                .split(" -> ")
                .nth(1)
                .unwrap_or(path_part)
                .to_string()
        } else {
            path_part.to_string()
        };

        entries.push(VcsEntry { rel_path, status });
    }

    entries
}

pub fn head_content(repo_root: &Path, rel_path: &str) -> Option<String> {
    let arg = format!("HEAD:{rel_path}");
    let output = Command::new("git")
        .args(["-C", &repo_root.to_string_lossy(), "show", &arg])
        .output()
        .ok()?;
    if output.status.success() {
        Some(String::from_utf8_lossy(&output.stdout).into_owned())
    } else {
        None
    }
}

pub fn discard_changes(repo_root: &Path, rel_path: &str) -> bool {
    Command::new("git")
        .args([
            "-C",
            &repo_root.to_string_lossy(),
            "checkout",
            "HEAD",
            "--",
            rel_path,
        ])
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}

pub fn stage_file(repo_root: &Path, rel_path: &str) -> bool {
    Command::new("git")
        .args(["-C", &repo_root.to_string_lossy(), "add", rel_path])
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_porcelain_modified() {
        let entries = parse_porcelain(" M src/main.rs\n");
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].status, VcsStatus::Modified);
        assert_eq!(entries[0].rel_path, "src/main.rs");
    }

    #[test]
    fn test_parse_porcelain_staged_modified() {
        let entries = parse_porcelain("M  src/main.rs\n");
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].status, VcsStatus::Modified);
    }

    #[test]
    fn test_parse_porcelain_added() {
        let entries = parse_porcelain("A  new_file.rs\n");
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].status, VcsStatus::Added);
        assert_eq!(entries[0].rel_path, "new_file.rs");
    }

    #[test]
    fn test_parse_porcelain_deleted() {
        let entries = parse_porcelain("D  old_file.rs\n");
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].status, VcsStatus::Deleted);
    }

    #[test]
    fn test_parse_porcelain_unstaged_delete() {
        let entries = parse_porcelain(" D old_file.rs\n");
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].status, VcsStatus::Deleted);
    }

    #[test]
    fn test_parse_porcelain_renamed() {
        let entries = parse_porcelain("R  old.rs -> new.rs\n");
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].status, VcsStatus::Renamed);
        assert_eq!(entries[0].rel_path, "new.rs");
    }

    #[test]
    fn test_parse_porcelain_untracked() {
        let entries = parse_porcelain("?? untracked.txt\n");
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].status, VcsStatus::Untracked);
        assert_eq!(entries[0].rel_path, "untracked.txt");
    }

    #[test]
    fn test_parse_porcelain_multiple() {
        let input = " M src/main.rs\nA  new.rs\n?? tmp.txt\n";
        let entries = parse_porcelain(input);
        assert_eq!(entries.len(), 3);
        assert_eq!(entries[0].status, VcsStatus::Modified);
        assert_eq!(entries[1].status, VcsStatus::Added);
        assert_eq!(entries[2].status, VcsStatus::Untracked);
    }

    #[test]
    fn test_parse_porcelain_empty() {
        assert!(parse_porcelain("").is_empty());
    }

    #[test]
    fn test_parse_porcelain_short_lines_ignored() {
        assert!(parse_porcelain("X\nAB\n").is_empty());
    }

    #[test]
    fn test_parse_porcelain_unknown_status_ignored() {
        let entries = parse_porcelain("!! ignored_file\n");
        assert!(entries.is_empty());
    }
}
