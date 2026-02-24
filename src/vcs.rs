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
        .args([
            "-C",
            &repo_root.to_string_lossy(),
            "status",
            "--porcelain",
            "-z",
        ])
        .output()
    {
        Ok(o) if o.status.success() => o,
        _ => return Vec::new(),
    };

    parse_porcelain_nul(&output.stdout)
}

/// Parse NUL-delimited `git status --porcelain -z` output into `VcsEntry` items.
///
/// Format: each entry is `XY PATH\0`, except renames which are `XY PATH\0NEWPATH\0`.
pub fn parse_porcelain_nul(raw: &[u8]) -> Vec<VcsEntry> {
    let text = String::from_utf8_lossy(raw);
    let mut entries = Vec::new();
    let mut fields = text.split('\0').peekable();

    while let Some(field) = fields.next() {
        if field.len() < 3 {
            continue;
        }
        let xy = &field[..2];
        let path_part = &field[3..];

        let status = match xy {
            "??" => VcsStatus::Untracked,
            _ if xy.starts_with('R') => VcsStatus::Renamed,
            _ if xy.starts_with('D') || xy.ends_with('D') => VcsStatus::Deleted,
            _ if xy.starts_with('A') => VcsStatus::Added,
            _ if xy.contains('M') => VcsStatus::Modified,
            _ => continue,
        };

        let rel_path = if status == VcsStatus::Renamed {
            // With -z, renames emit: "XY old_path\0new_path\0"
            // Consume the next field as the new path.
            fields.next().unwrap_or(path_part).to_string()
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
    fn test_parse_nul_modified() {
        let entries = parse_porcelain_nul(b" M src/main.rs\0");
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].status, VcsStatus::Modified);
        assert_eq!(entries[0].rel_path, "src/main.rs");
    }

    #[test]
    fn test_parse_nul_staged_modified() {
        let entries = parse_porcelain_nul(b"M  src/main.rs\0");
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].status, VcsStatus::Modified);
    }

    #[test]
    fn test_parse_nul_added() {
        let entries = parse_porcelain_nul(b"A  new_file.rs\0");
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].status, VcsStatus::Added);
        assert_eq!(entries[0].rel_path, "new_file.rs");
    }

    #[test]
    fn test_parse_nul_deleted() {
        let entries = parse_porcelain_nul(b"D  old_file.rs\0");
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].status, VcsStatus::Deleted);
    }

    #[test]
    fn test_parse_nul_unstaged_delete() {
        let entries = parse_porcelain_nul(b" D old_file.rs\0");
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].status, VcsStatus::Deleted);
    }

    #[test]
    fn test_parse_nul_renamed() {
        // With -z: "R  old.rs\0new.rs\0"
        let entries = parse_porcelain_nul(b"R  old.rs\0new.rs\0");
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].status, VcsStatus::Renamed);
        assert_eq!(entries[0].rel_path, "new.rs");
    }

    #[test]
    fn test_parse_nul_untracked() {
        let entries = parse_porcelain_nul(b"?? untracked.txt\0");
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].status, VcsStatus::Untracked);
        assert_eq!(entries[0].rel_path, "untracked.txt");
    }

    #[test]
    fn test_parse_nul_multiple() {
        let input = b" M src/main.rs\0A  new.rs\0?? tmp.txt\0";
        let entries = parse_porcelain_nul(input);
        assert_eq!(entries.len(), 3);
        assert_eq!(entries[0].status, VcsStatus::Modified);
        assert_eq!(entries[1].status, VcsStatus::Added);
        assert_eq!(entries[2].status, VcsStatus::Untracked);
    }

    #[test]
    fn test_parse_nul_empty() {
        assert!(parse_porcelain_nul(b"").is_empty());
    }

    #[test]
    fn test_parse_nul_unknown_status_ignored() {
        let entries = parse_porcelain_nul(b"!! ignored_file\0");
        assert!(entries.is_empty());
    }

    #[test]
    fn test_parse_nul_path_with_spaces() {
        let entries = parse_porcelain_nul(b" M path with spaces/file name.rs\0");
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].status, VcsStatus::Modified);
        assert_eq!(entries[0].rel_path, "path with spaces/file name.rs");
    }

    #[test]
    fn test_parse_nul_path_containing_arrow() {
        // A file whose name literally contains " -> " — with -z this is unambiguous
        let entries = parse_porcelain_nul(b" M docs/a -> b/readme.md\0");
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].status, VcsStatus::Modified);
        assert_eq!(entries[0].rel_path, "docs/a -> b/readme.md");
    }

    #[test]
    fn test_parse_nul_rename_with_spaces() {
        let entries = parse_porcelain_nul(b"R  old name.rs\0new name.rs\0");
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].status, VcsStatus::Renamed);
        assert_eq!(entries[0].rel_path, "new name.rs");
    }
}
