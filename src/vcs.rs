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
