"""Advanced VCS window tests: title format, double-click diff, renamed files."""
import os
import subprocess
import tempfile

from dogtail.utils import doDelay

from conftest import find_app, find_labels, send_keys


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



def test_vcs_window_title_contains_repo_name_and_branch(app_process):
    """VCS window title should contain repo name and branch (or 'git')."""
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

        frames = app.findChildren(lambda n: n.roleName == "frame")
        assert frames, "No window frame found"
        title = frames[0].name

        repo_name = os.path.basename(repo)
        assert repo_name in title, (
            f"Expected repo name '{repo_name}' in title '{title}'"
        )
        # Title shows branch name (e.g. "master") or "git" as fallback
        assert "(git)" in title or "(main)" in title or "(master)" in title, (
            f"Expected branch or '(git)' in title '{title}'"
        )


def test_vcs_double_click_opens_diff_tab(app_process):
    """Double-clicking (Enter) on a modified file should open a diff tab."""
    with tempfile.TemporaryDirectory() as repo:
        _init_repo(repo)
        filepath = os.path.join(repo, "hello.txt")
        with open(filepath, "w") as f:
            f.write("original\n")
        _git(repo, "add", ".")
        _git(repo, "commit", "-m", "init")
        with open(filepath, "w") as f:
            f.write("modified\n")

        proc = app_process(repo)
        app = find_app()
        doDelay(2)

        # Verify the file is listed
        labels = find_labels(app)
        assert any("hello.txt" in t for t in labels), (
            f"Expected 'hello.txt' in labels: {labels}"
        )

        # Send Enter to activate the selected row (first row should be selected)
        send_keys("Return", proc.pid)
        doDelay(3)

        # After opening, there should be a new tab with diff content.
        # The diff tab will show additional labels (file names in pane headers).
        labels_after = find_labels(app)
        # A diff tab for a modified file shows "changes" or chunk info
        has_diff_indicators = (
            any("changes" in t.lower() for t in labels_after)
            or any("of " in t for t in labels_after)
            or any("No changes" in t for t in labels_after)
        )
        # Also check for tab labels -- there should be more than one tab now
        page_tabs = app.findChildren(
            lambda n: n.roleName == "page tab" and n.showing
        )
        assert len(page_tabs) >= 2 or has_diff_indicators, (
            f"Expected a diff tab to open after Enter. "
            f"Page tabs: {len(page_tabs)}, labels: {labels_after}"
        )


def test_vcs_renamed_file_shows_renamed_status(app_process):
    """A renamed file should appear with 'Renamed' status."""
    with tempfile.TemporaryDirectory() as repo:
        _init_repo(repo)
        filepath = os.path.join(repo, "old_name.txt")
        # Create file with enough content for git to detect rename
        content = "This is a file with enough content for rename detection.\n" * 5
        with open(filepath, "w") as f:
            f.write(content)
        _git(repo, "add", ".")
        _git(repo, "commit", "-m", "init")

        # Rename using git mv for reliable detection
        _git(repo, "mv", "old_name.txt", "new_name.txt")

        proc = app_process(repo)
        app = find_app()
        doDelay(2)

        labels = find_labels(app)
        assert any("new_name.txt" in t for t in labels), (
            f"Expected 'new_name.txt' in labels: {labels}"
        )
        assert any("Renamed" in t for t in labels), (
            f"Expected 'Renamed' in labels: {labels}"
        )
