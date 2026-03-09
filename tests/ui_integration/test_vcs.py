"""VCS (git) window tests via dogtail."""
import os
import subprocess
import tempfile

from dogtail.utils import doDelay

from conftest import find_app, find_labels, is_button, wait_for_label


def _init_repo(repo):
    """Initialise a git repo with a user config."""
    subprocess.run(["git", "init", repo], check=True, capture_output=True)
    subprocess.run(
        ["git", "-C", repo, "config", "user.email", "test@test.com"],
        check=True, capture_output=True,
    )
    subprocess.run(
        ["git", "-C", repo, "config", "user.name", "Test"],
        check=True, capture_output=True,
    )


def _git(repo, *args):
    subprocess.run(
        ["git", "-C", repo, *args],
        check=True, capture_output=True,
    )



def test_vcs_window_opens(app_process):
    """A git repo directory should open the VCS window."""
    with tempfile.TemporaryDirectory() as repo:
        _init_repo(repo)
        filepath = os.path.join(repo, "file.txt")
        with open(filepath, "w") as f:
            f.write("original\n")
        _git(repo, "add", ".")
        _git(repo, "commit", "-m", "init")
        with open(filepath, "w") as f:
            f.write("modified\n")

        app_process(repo)
        app = find_app()
        doDelay(1)
        windows = app.findChildren(lambda n: n.roleName == "frame")
        assert len(windows) >= 1, "No VCS window found"


def test_vcs_shows_modified_file(app_process):
    """Modified file should appear with 'Modified' status."""
    with tempfile.TemporaryDirectory() as repo:
        _init_repo(repo)
        filepath = os.path.join(repo, "hello.txt")
        with open(filepath, "w") as f:
            f.write("original\n")
        _git(repo, "add", ".")
        _git(repo, "commit", "-m", "init")
        with open(filepath, "w") as f:
            f.write("modified\n")

        app_process(repo)
        app = find_app()
        doDelay(2)
        labels = find_labels(app)
        assert any("hello.txt" in t for t in labels), \
            f"Expected 'hello.txt' in labels: {labels}"
        assert any("Modified" in t for t in labels), \
            f"Expected 'Modified' in labels: {labels}"


def test_vcs_shows_untracked_file(app_process):
    """Untracked file should appear with 'Untracked' status."""
    with tempfile.TemporaryDirectory() as repo:
        _init_repo(repo)
        # Need at least one commit for git status to work
        filepath = os.path.join(repo, "committed.txt")
        with open(filepath, "w") as f:
            f.write("committed\n")
        _git(repo, "add", ".")
        _git(repo, "commit", "-m", "init")
        # Create an untracked file
        untracked = os.path.join(repo, "newfile.txt")
        with open(untracked, "w") as f:
            f.write("I am untracked\n")

        app_process(repo)
        app = find_app()
        doDelay(2)
        labels = find_labels(app)
        assert any("newfile.txt" in t for t in labels), \
            f"Expected 'newfile.txt' in labels: {labels}"
        assert any("Untracked" in t for t in labels), \
            f"Expected 'Untracked' in labels: {labels}"


def test_vcs_shows_added_file(app_process):
    """Staged new file should appear with 'Added' status."""
    with tempfile.TemporaryDirectory() as repo:
        _init_repo(repo)
        filepath = os.path.join(repo, "first.txt")
        with open(filepath, "w") as f:
            f.write("first\n")
        _git(repo, "add", ".")
        _git(repo, "commit", "-m", "init")
        added = os.path.join(repo, "added.txt")
        with open(added, "w") as f:
            f.write("new file\n")
        _git(repo, "add", "added.txt")

        app_process(repo)
        app = find_app()
        doDelay(2)
        labels = find_labels(app)
        assert any("added.txt" in t for t in labels), \
            f"Expected 'added.txt' in labels: {labels}"
        assert any("Added" in t for t in labels), \
            f"Expected 'Added' in labels: {labels}"


def test_vcs_shows_deleted_file(app_process):
    """Deleted tracked file should appear with 'Deleted' status."""
    with tempfile.TemporaryDirectory() as repo:
        _init_repo(repo)
        filepath = os.path.join(repo, "doomed.txt")
        with open(filepath, "w") as f:
            f.write("goodbye\n")
        _git(repo, "add", ".")
        _git(repo, "commit", "-m", "init")
        os.remove(filepath)

        app_process(repo)
        app = find_app()
        doDelay(2)
        labels = find_labels(app)
        assert any("doomed.txt" in t for t in labels), \
            f"Expected 'doomed.txt' in labels: {labels}"
        assert any("Deleted" in t for t in labels), \
            f"Expected 'Deleted' in labels: {labels}"


def test_vcs_shows_staged_extra(app_process):
    """A fully staged file should show 'Staged' in the Extra column."""
    with tempfile.TemporaryDirectory() as repo:
        _init_repo(repo)
        filepath = os.path.join(repo, "staged.txt")
        with open(filepath, "w") as f:
            f.write("original\n")
        _git(repo, "add", ".")
        _git(repo, "commit", "-m", "init")
        with open(filepath, "w") as f:
            f.write("staged change\n")
        _git(repo, "add", "staged.txt")

        app_process(repo)
        app = find_app()
        doDelay(2)
        labels = find_labels(app)
        assert any("staged.txt" in t for t in labels), \
            f"Expected 'staged.txt' in labels: {labels}"
        assert any("Staged" == t for t in labels), \
            f"Expected 'Staged' in labels: {labels}"


def test_vcs_shows_partially_staged(app_process):
    """A file with both staged and unstaged changes shows 'Partially staged'."""
    with tempfile.TemporaryDirectory() as repo:
        _init_repo(repo)
        filepath = os.path.join(repo, "partial.txt")
        with open(filepath, "w") as f:
            f.write("original\n")
        _git(repo, "add", ".")
        _git(repo, "commit", "-m", "init")
        # Stage a change
        with open(filepath, "w") as f:
            f.write("staged version\n")
        _git(repo, "add", "partial.txt")
        # Make another unstaged change
        with open(filepath, "w") as f:
            f.write("unstaged version\n")

        app_process(repo)
        app = find_app()
        doDelay(2)
        labels = find_labels(app)
        assert any("partial.txt" in t for t in labels), \
            f"Expected 'partial.txt' in labels: {labels}"
        assert any("Partially staged" in t for t in labels), \
            f"Expected 'Partially staged' in labels: {labels}"


def test_vcs_multiple_statuses(app_process):
    """Repo with mixed statuses should show all files correctly."""
    with tempfile.TemporaryDirectory() as repo:
        _init_repo(repo)
        # Create and commit two files
        for name in ["keep.txt", "remove.txt"]:
            with open(os.path.join(repo, name), "w") as f:
                f.write(f"{name} content\n")
        _git(repo, "add", ".")
        _git(repo, "commit", "-m", "init")

        # Modify one
        with open(os.path.join(repo, "keep.txt"), "w") as f:
            f.write("modified\n")
        # Delete one
        os.remove(os.path.join(repo, "remove.txt"))
        # Add an untracked one
        with open(os.path.join(repo, "brand_new.txt"), "w") as f:
            f.write("new\n")

        app_process(repo)
        app = find_app()
        doDelay(2)
        labels = find_labels(app)
        assert any("keep.txt" in t for t in labels), \
            f"Expected 'keep.txt' in labels: {labels}"
        assert any("remove.txt" in t for t in labels), \
            f"Expected 'remove.txt' in labels: {labels}"
        assert any("brand_new.txt" in t for t in labels), \
            f"Expected 'brand_new.txt' in labels: {labels}"
        assert any("Modified" in t for t in labels), \
            f"Expected 'Modified' in labels: {labels}"
        assert any("Deleted" in t for t in labels), \
            f"Expected 'Deleted' in labels: {labels}"
        assert any("Untracked" in t for t in labels), \
            f"Expected 'Untracked' in labels: {labels}"


def test_vcs_conflict_file(app_process):
    """Conflicting file should appear with 'Conflict' status."""
    with tempfile.TemporaryDirectory() as repo:
        _init_repo(repo)
        filepath = os.path.join(repo, "conflict.txt")
        with open(filepath, "w") as f:
            f.write("base\n")
        _git(repo, "add", ".")
        _git(repo, "commit", "-m", "init")
        # Create a branch with a change
        _git(repo, "checkout", "-b", "feature")
        with open(filepath, "w") as f:
            f.write("feature change\n")
        _git(repo, "add", ".")
        _git(repo, "commit", "-m", "feature")
        # Go back to the initial branch and make a conflicting change
        _git(repo, "checkout", "-")
        with open(filepath, "w") as f:
            f.write("master change\n")
        _git(repo, "add", ".")
        _git(repo, "commit", "-m", "master")
        # Merge — this will conflict
        try:
            _git(repo, "merge", "feature")
        except subprocess.CalledProcessError:
            pass  # Expected to fail due to conflict

        app_process(repo)
        app = find_app()
        doDelay(2)
        labels = find_labels(app)
        assert any("conflict.txt" in t for t in labels), \
            f"Expected 'conflict.txt' in labels: {labels}"
        assert any("Conflict" in t for t in labels), \
            f"Expected 'Conflict' in labels: {labels}"


def test_vcs_empty_repo_no_crash(app_process):
    """Opening a git repo with no commits should not crash."""
    with tempfile.TemporaryDirectory() as repo:
        _init_repo(repo)
        # Add a file but don't commit
        with open(os.path.join(repo, "readme.txt"), "w") as f:
            f.write("hello\n")

        app_process(repo)
        app = find_app()
        doDelay(1)
        windows = app.findChildren(lambda n: n.roleName == "frame")
        assert len(windows) >= 1, "No VCS window found for empty repo"


def test_vcs_shows_changes_tab(app_process):
    """VCS window should have a 'Changes' tab."""
    with tempfile.TemporaryDirectory() as repo:
        _init_repo(repo)
        filepath = os.path.join(repo, "file.txt")
        with open(filepath, "w") as f:
            f.write("original\n")
        _git(repo, "add", ".")
        _git(repo, "commit", "-m", "init")
        with open(filepath, "w") as f:
            f.write("modified\n")

        app_process(repo)
        app = find_app()
        doDelay(2)

        tabs = app.findChildren(lambda n: n.roleName == "page tab" and n.showing)
        tab_names = [t.name for t in tabs]
        assert any("Changes" in n or "change" in n.lower() for n in tab_names), \
            f"Expected 'Changes' tab, got tabs: {tab_names}"


def test_vcs_changed_file_count_label(app_process):
    """VCS window should show a changed file count."""
    with tempfile.TemporaryDirectory() as repo:
        _init_repo(repo)
        for name in ["a.txt", "b.txt"]:
            with open(os.path.join(repo, name), "w") as f:
                f.write(f"{name}\n")
        _git(repo, "add", ".")
        _git(repo, "commit", "-m", "init")
        # Modify both files
        for name in ["a.txt", "b.txt"]:
            with open(os.path.join(repo, name), "w") as f:
                f.write(f"{name} modified\n")

        app_process(repo)
        app = find_app()
        doDelay(2)

        labels = find_labels(app)
        # Should show "2" somewhere in a count label
        assert any("2" in t for t in labels), \
            f"Expected file count with '2' in labels: {labels}"


def test_vcs_subdirectory_file_shows_path(app_process):
    """A modified file in a subdirectory should show its relative path."""
    with tempfile.TemporaryDirectory() as repo:
        _init_repo(repo)
        subdir = os.path.join(repo, "src")
        os.makedirs(subdir)
        filepath = os.path.join(subdir, "main.rs")
        with open(filepath, "w") as f:
            f.write("fn main() {}\n")
        _git(repo, "add", ".")
        _git(repo, "commit", "-m", "init")
        with open(filepath, "w") as f:
            f.write("fn main() { println!(\"hello\"); }\n")

        app_process(repo)
        app = find_app()
        doDelay(2)

        labels = find_labels(app)
        assert any("main.rs" in t for t in labels), \
            f"Expected 'main.rs' in labels: {labels}"


def test_vcs_shows_branch_name(app_process):
    """VCS window should show the current branch name."""
    with tempfile.TemporaryDirectory() as repo:
        _init_repo(repo)
        filepath = os.path.join(repo, "file.txt")
        with open(filepath, "w") as f:
            f.write("content\n")
        _git(repo, "add", ".")
        _git(repo, "commit", "-m", "init")

        app_process(repo)
        app = find_app()
        doDelay(2)
        labels = find_labels(app)
        # Default branch is usually "main" or "master"
        assert any("main" in t or "master" in t for t in labels), \
            f"Expected branch name in labels: {labels}"


def test_vcs_filter_untracked_toggle(app_process):
    """Toggling off Untracked should hide untracked files."""
    with tempfile.TemporaryDirectory() as repo:
        _init_repo(repo)
        filepath = os.path.join(repo, "committed.txt")
        with open(filepath, "w") as f:
            f.write("committed\n")
        _git(repo, "add", ".")
        _git(repo, "commit", "-m", "init")
        untracked = os.path.join(repo, "untracked.txt")
        with open(untracked, "w") as f:
            f.write("untracked\n")

        app_process(repo)
        app = find_app()
        doDelay(2)

        labels = find_labels(app)
        assert any("untracked.txt" in t for t in labels), \
            f"Expected untracked.txt visible: {labels}"

        # Toggle off Untracked - it's a toggle button
        toggle = app.findChild(
            lambda n: n.roleName == "toggle button" and "Untracked" in n.name and n.showing
        )
        assert toggle is not None, "Untracked toggle not found"
        toggle.do_action(0)
        doDelay(1)

        labels = find_labels(app)
        assert not any("untracked.txt" in t for t in labels), \
            f"Expected untracked.txt hidden after toggle: {labels}"


def test_vcs_commit_button_exists(app_process):
    """VCS view should have a Commit button."""
    with tempfile.TemporaryDirectory() as repo:
        _init_repo(repo)
        filepath = os.path.join(repo, "file.txt")
        with open(filepath, "w") as f:
            f.write("content\n")
        _git(repo, "add", ".")
        _git(repo, "commit", "-m", "init")

        app_process(repo)
        app = find_app()
        doDelay(2)

        commit_btn = app.findChild(
            lambda n: is_button(n) and "Commit" in n.name and n.showing
        )
        assert commit_btn is not None, "Commit button not found"
