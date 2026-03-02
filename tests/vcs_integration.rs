use mergers::vcs::{VcsStatus, changed_files, is_git_repo, repo_root};
use std::{fs, process::Command};
use tempfile::tempdir;

fn git_init(path: &std::path::Path) {
    let status = Command::new("git")
        .arg("init")
        .current_dir(path)
        .status()
        .expect("Failed to execute git init");
    assert!(status.success());

    // Set user name and email for commits in tests
    Command::new("git")
        .args(["config", "user.email", "test@example.com"])
        .current_dir(path)
        .status()
        .unwrap();
    Command::new("git")
        .args(["config", "user.name", "Test User"])
        .current_dir(path)
        .status()
        .unwrap();
}

#[test]
fn test_git_repo_detection() {
    let dir = tempdir().unwrap();
    let path = dir.path();

    // Not a repo yet
    assert!(!is_git_repo(path));
    assert_eq!(repo_root(path), None);

    git_init(path);

    assert!(is_git_repo(path));
    assert!(repo_root(path).is_some());
}

#[test]
fn test_changed_files_states() {
    let dir = tempdir().unwrap();
    let path = dir.path();
    git_init(path);

    // 1. Untracked file
    let file1 = "untracked.txt";
    fs::write(path.join(file1), "hello").unwrap();

    let changes = changed_files(path);
    assert!(
        changes
            .iter()
            .any(|c| c.rel_path == file1 && c.status == VcsStatus::Untracked)
    );

    // 2. Added (staged)
    Command::new("git")
        .args(["add", file1])
        .current_dir(path)
        .status()
        .unwrap();
    let changes = changed_files(path);
    assert!(
        changes
            .iter()
            .any(|c| c.rel_path == file1 && c.status == VcsStatus::Added)
    );

    // 3. Modified
    Command::new("git")
        .args(["commit", "-m", "initial"])
        .current_dir(path)
        .status()
        .unwrap();
    fs::write(path.join(file1), "world").unwrap();
    let changes = changed_files(path);
    assert!(
        changes
            .iter()
            .any(|c| c.rel_path == file1 && c.status == VcsStatus::Modified)
    );

    // 4. Deleted
    fs::remove_file(path.join(file1)).unwrap();
    let changes = changed_files(path);
    assert!(
        changes
            .iter()
            .any(|c| c.rel_path == file1 && c.status == VcsStatus::Deleted)
    );

    // 5. Renamed
    fs::write(path.join("old.txt"), "content").unwrap();
    Command::new("git")
        .args(["add", "old.txt"])
        .current_dir(path)
        .status()
        .unwrap();
    Command::new("git")
        .args(["commit", "-m", "old"])
        .current_dir(path)
        .status()
        .unwrap();
    Command::new("git")
        .args(["mv", "old.txt", "new.txt"])
        .current_dir(path)
        .status()
        .unwrap();

    let raw = Command::new("git")
        .args(["status", "--porcelain", "-z"])
        .current_dir(path)
        .output()
        .unwrap()
        .stdout;
    println!("Raw git status: {:?}", String::from_utf8_lossy(&raw));

    let changes = changed_files(path);
    println!("Changes: {changes:?}");
    assert!(
        changes
            .iter()
            .any(|c| c.rel_path == "new.txt" && c.status == VcsStatus::Renamed)
    );
}
