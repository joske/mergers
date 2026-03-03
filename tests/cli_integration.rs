use std::process::Command;

use tempfile::tempdir;

fn mergers_cmd() -> Command {
    Command::new(env!("CARGO_BIN_EXE_mergers"))
}

/// Returns a command with display environment variables removed so GTK init
/// fails immediately instead of opening a window and hanging.
fn mergers_cmd_no_display() -> Command {
    let mut cmd = mergers_cmd();
    cmd.env_remove("DISPLAY")
        .env_remove("WAYLAND_DISPLAY")
        .env("GDK_BACKEND", "x11");
    cmd
}

#[test]
fn two_files_no_error() {
    let dir = tempdir().unwrap();
    let left = dir.path().join("a.txt");
    let right = dir.path().join("b.txt");
    std::fs::write(&left, "hello").unwrap();
    std::fs::write(&right, "world").unwrap();

    let output = mergers_cmd_no_display()
        .args([&left, &right])
        .output()
        .expect("failed to run mergers");

    let stderr = String::from_utf8_lossy(&output.stderr);
    // Should not print our custom error prefix. GTK init will fail without a
    // display, but that is a different kind of error.
    assert!(
        !stderr.contains("Error:"),
        "unexpected Error on stderr: {stderr}"
    );
}

#[test]
fn single_file_errors() {
    let dir = tempdir().unwrap();
    let file = dir.path().join("only.txt");
    std::fs::write(&file, "data").unwrap();

    let output = mergers_cmd()
        .arg(&file)
        .output()
        .expect("failed to run mergers");

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("single file argument not supported"),
        "expected 'single file argument not supported', got: {stderr}"
    );
}

#[test]
fn nonexistent_path_errors() {
    let dir = tempdir().unwrap();
    let left = dir.path().join("nope1");
    let right = dir.path().join("nope2");

    let output = mergers_cmd()
        .args([&left, &right])
        .output()
        .expect("failed to run mergers");

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("does not exist"),
        "expected 'does not exist', got: {stderr}"
    );
}

#[test]
fn file_dir_mix_errors() {
    let dir = tempdir().unwrap();
    let file = dir.path().join("file.txt");
    std::fs::write(&file, "data").unwrap();
    let subdir = dir.path().join("subdir");
    std::fs::create_dir(&subdir).unwrap();

    let output = mergers_cmd()
        .args([&file, &subdir])
        .output()
        .expect("failed to run mergers");

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("cannot compare a file with a directory"),
        "expected 'cannot compare a file with a directory', got: {stderr}"
    );
}

#[test]
fn three_files_non_file_errors() {
    let dir = tempdir().unwrap();
    let a = dir.path().join("a.txt");
    let b = dir.path().join("b.txt");
    std::fs::write(&a, "data").unwrap();
    std::fs::write(&b, "data").unwrap();
    let subdir = dir.path().join("subdir");
    std::fs::create_dir(&subdir).unwrap();

    let output = mergers_cmd()
        .args([&a, &b, &subdir])
        .output()
        .expect("failed to run mergers");

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("is not a file"),
        "expected 'is not a file', got: {stderr}"
    );
}

#[test]
fn non_git_dir_errors() {
    let dir = tempdir().unwrap();

    let output = mergers_cmd()
        .arg(dir.path())
        .output()
        .expect("failed to run mergers");

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("not inside a git repository"),
        "expected 'not inside a git repository', got: {stderr}"
    );
}

#[test]
fn too_many_args_errors() {
    let dir = tempdir().unwrap();
    let paths: Vec<_> = (0..5)
        .map(|i| {
            let p = dir.path().join(format!("{i}.txt"));
            std::fs::write(&p, "x").unwrap();
            p
        })
        .collect();

    let output = mergers_cmd()
        .args(&paths)
        .output()
        .expect("failed to run mergers");

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("expected 0-3 paths"),
        "expected 'expected 0-3 paths', got: {stderr}"
    );
}

#[test]
fn version_flag() {
    let output = mergers_cmd()
        .arg("--version")
        .output()
        .expect("failed to run mergers");

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("mergers"),
        "expected stdout to contain 'mergers', got: {stdout}"
    );
}

#[test]
fn help_flag() {
    let output = mergers_cmd()
        .arg("--help")
        .output()
        .expect("failed to run mergers");

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("Visual diff and merge tool"),
        "expected stdout to contain 'Visual diff and merge tool', got: {stdout}"
    );
}

#[test]
fn git_repo_single_dir_no_error() {
    let dir = tempdir().unwrap();
    let path = dir.path();

    Command::new("git")
        .arg("init")
        .current_dir(path)
        .status()
        .expect("failed to run git init");

    let output = mergers_cmd_no_display()
        .arg(path)
        .output()
        .expect("failed to run mergers");

    let stderr = String::from_utf8_lossy(&output.stderr);
    // Should not print our custom error prefix. GTK init will fail without a
    // display, but that is a different kind of error.
    assert!(
        !stderr.contains("Error:"),
        "unexpected Error on stderr: {stderr}"
    );
}
