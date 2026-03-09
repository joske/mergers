use std::{
    fs,
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
    Conflict,
    Ignored,
}

#[derive(Debug, Clone)]
pub struct VcsEntry {
    pub rel_path: String,
    pub status: VcsStatus,
    pub extra: String,
}

/// Check if a directory is part of a Git repository.
#[must_use]
pub fn is_git_repo(dir: &Path) -> bool {
    Command::new("git")
        .args([
            "-C",
            &dir.to_string_lossy(),
            "rev-parse",
            "--is-inside-work-tree",
        ])
        .output()
        .is_ok_and(|o| String::from_utf8_lossy(&o.stdout).trim() == "true")
}

/// Find the root of the Git repository containing the directory.
#[must_use]
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

/// List files with changes in the repository, including ignored files.
#[must_use]
pub fn changed_files_with_ignored(repo_root: &Path) -> Vec<VcsEntry> {
    let output = match Command::new("git")
        .args([
            "-C",
            &repo_root.to_string_lossy(),
            "status",
            "--porcelain",
            "-z",
            "--ignored",
        ])
        .output()
    {
        Ok(o) if o.status.success() => o,
        _ => return Vec::new(),
    };

    parse_porcelain_nul(&output.stdout)
}

/// List files with changes in the repository.
#[must_use]
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
#[must_use]
pub fn parse_porcelain_nul(raw: &[u8]) -> Vec<VcsEntry> {
    let mut entries = Vec::new();
    let mut i = 0;
    while i < raw.len() {
        // Find the next NUL
        let next_nul = raw[i..]
            .iter()
            .position(|&b| b == 0)
            .map_or(raw.len(), |p| i + p);
        let entry_str = String::from_utf8_lossy(&raw[i..next_nul]);
        if entry_str.len() < 3 {
            i = next_nul + 1;
            continue;
        }

        let xy = &entry_str[..2];
        let path = entry_str[2..].trim_start().to_string();

        let status = match xy {
            "??" => VcsStatus::Untracked,
            "!!" => VcsStatus::Ignored,
            _ if xy.contains('U') || xy == "AA" || xy == "DD" => VcsStatus::Conflict,
            _ if xy.starts_with('R') => VcsStatus::Renamed,
            _ if xy.starts_with('D') || xy.ends_with('D') => VcsStatus::Deleted,
            _ if xy.starts_with('A') => VcsStatus::Added,
            _ if xy.contains('M') => VcsStatus::Modified,
            _ => {
                i = next_nul + 1;
                continue;
            }
        };

        let rel_path = if status == VcsStatus::Renamed {
            // In -z porcelain, the first path is the NEW path, second is OLD path.
            let next_start = next_nul + 1;
            let next_nul2 = raw[next_start..]
                .iter()
                .position(|&b| b == 0)
                .map_or(raw.len(), |p| next_start + p);
            // Consume the second NUL (old path)
            i = next_nul2 + 1;
            path // 'path' here is the first part after XY, which is the NEW path
        } else {
            i = next_nul + 1;
            path
        };

        let x_byte = xy.as_bytes()[0];
        let y_byte = xy.as_bytes()[1];
        let extra = if status != VcsStatus::Conflict && x_byte != b' ' && x_byte != b'?' {
            if y_byte != b' ' && y_byte != b'?' {
                "Partially staged".to_string()
            } else {
                "Staged".to_string()
            }
        } else {
            String::new()
        };
        entries.push(VcsEntry {
            rel_path,
            status,
            extra,
        });
    }

    entries
}

fn git_show(repo_root: &Path, refspec: &str) -> Option<String> {
    let output = Command::new("git")
        .args(["-C", &repo_root.to_string_lossy(), "show", refspec])
        .output()
        .ok()?;
    if output.status.success() {
        Some(String::from_utf8_lossy(&output.stdout).into_owned())
    } else {
        None
    }
}

/// Get the content of a file at HEAD.
#[must_use]
pub fn head_content(repo_root: &Path, rel_path: &str) -> Option<String> {
    git_show(repo_root, &format!("HEAD:{rel_path}"))
}

/// Get the content of a file at a specific merge stage (1=base, 2=ours, 3=theirs).
#[must_use]
pub fn stage_content(repo_root: &Path, rel_path: &str, stage: u8) -> Option<String> {
    git_show(repo_root, &format!(":{stage}:{rel_path}"))
}

/// Discard local changes to a file.
#[must_use]
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
        .is_ok_and(|s| s.success())
}

/// Stage a file (git add).
#[must_use]
pub fn stage_file(repo_root: &Path, rel_path: &str) -> bool {
    Command::new("git")
        .args(["-C", &repo_root.to_string_lossy(), "add", "--", rel_path])
        .status()
        .is_ok_and(|s| s.success())
}

#[must_use]
pub fn unstage_file(repo_root: &Path, rel_path: &str) -> bool {
    Command::new("git")
        .args([
            "-C",
            &repo_root.to_string_lossy(),
            "restore",
            "--staged",
            "--",
            rel_path,
        ])
        .status()
        .is_ok_and(|s| s.success())
}

/// Get the name of the current Git branch.
#[must_use]
pub fn current_branch(repo_root: &Path) -> Option<String> {
    let output = Command::new("git")
        .args([
            "-C",
            &repo_root.to_string_lossy(),
            "branch",
            "--show-current",
        ])
        .output()
        .ok()?;
    if output.status.success() {
        let name = String::from_utf8_lossy(&output.stdout).trim().to_string();
        if name.is_empty() { None } else { Some(name) }
    } else {
        None
    }
}

/// List files that are staged for commit.
#[must_use]
pub fn staged_files(repo_root: &Path) -> Vec<String> {
    let output = match Command::new("git")
        .args([
            "-C",
            &repo_root.to_string_lossy(),
            "diff",
            "--cached",
            "--name-only",
        ])
        .output()
    {
        Ok(o) if o.status.success() => o,
        _ => return Vec::new(),
    };
    String::from_utf8_lossy(&output.stdout)
        .lines()
        .filter(|l| !l.is_empty())
        .map(String::from)
        .collect()
}

/// Create a commit with the given message.
#[must_use]
pub fn commit(repo_root: &Path, message: &str) -> bool {
    Command::new("git")
        .args(["-C", &repo_root.to_string_lossy(), "commit", "-m", message])
        .status()
        .is_ok_and(|s| s.success())
}

/// Read a pre-filled commit message (e.g. `.git/MERGE_MSG`) if one exists.
#[must_use]
pub fn commit_message_prefill(repo_root: &Path) -> Option<String> {
    let path = repo_root.join(".git").join("MERGE_MSG");
    let content = fs::read_to_string(path).ok()?;
    let trimmed = content.trim().to_string();
    if trimmed.is_empty() {
        None
    } else {
        Some(trimmed)
    }
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
        assert_eq!(entries[0].extra, "Staged");
    }

    #[test]
    fn test_parse_nul_partially_staged() {
        let entries = parse_porcelain_nul(b"MM src/main.rs\0");
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].status, VcsStatus::Modified);
        assert_eq!(entries[0].extra, "Partially staged");
    }

    #[test]
    fn test_parse_nul_unstaged_not_staged() {
        let entries = parse_porcelain_nul(b" M src/main.rs\0");
        assert_eq!(entries.len(), 1);
        assert!(entries[0].extra.is_empty());
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
        // With -z: "R  new.rs\0old.rs\0"
        let entries = parse_porcelain_nul(b"R  new.rs\0old.rs\0");
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
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].status, VcsStatus::Ignored);
        assert_eq!(entries[0].rel_path, "ignored_file");
    }

    #[test]
    fn test_parse_nul_ignored() {
        let entries = parse_porcelain_nul(b"!! build/output.o\0!! .env\0");
        assert_eq!(entries.len(), 2);
        assert_eq!(entries[0].status, VcsStatus::Ignored);
        assert_eq!(entries[0].rel_path, "build/output.o");
        assert_eq!(entries[1].status, VcsStatus::Ignored);
        assert_eq!(entries[1].rel_path, ".env");
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
    fn test_parse_nul_conflict_uu() {
        let entries = parse_porcelain_nul(b"UU conflicting.rs\0");
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].status, VcsStatus::Conflict);
        assert_eq!(entries[0].rel_path, "conflicting.rs");
    }

    #[test]
    fn test_parse_nul_conflict_aa() {
        let entries = parse_porcelain_nul(b"AA both_added.rs\0");
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].status, VcsStatus::Conflict);
    }

    #[test]
    fn test_parse_nul_conflict_du() {
        let entries = parse_porcelain_nul(b"DU deleted_by_us.rs\0");
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].status, VcsStatus::Conflict);
        assert!(entries[0].extra.is_empty());
    }

    #[test]
    fn test_parse_nul_rename_with_spaces() {
        let entries = parse_porcelain_nul(b"R  new name.rs\0old name.rs\0");
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].status, VcsStatus::Renamed);
        assert_eq!(entries[0].rel_path, "new name.rs");
    }

    #[test]
    fn test_current_branch_returns_some_in_repo() {
        // We're running inside the mergers repo, so there should be a branch.
        let root = repo_root(Path::new(env!("CARGO_MANIFEST_DIR"))).unwrap();
        assert!(current_branch(&root).is_some());
    }

    #[test]
    fn test_staged_files_returns_vec() {
        let root = repo_root(Path::new(env!("CARGO_MANIFEST_DIR"))).unwrap();
        // Should not panic; result may be empty or non-empty.
        let _ = staged_files(&root);
    }

    #[test]
    fn test_commit_message_prefill_no_merge() {
        let root = repo_root(Path::new(env!("CARGO_MANIFEST_DIR"))).unwrap();
        // Outside of a merge, MERGE_MSG shouldn't exist.
        // This just verifies it doesn't panic.
        let _ = commit_message_prefill(&root);
    }
}
