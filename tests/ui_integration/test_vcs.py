"""VCS (git) window tests via dogtail."""
import os
import subprocess
import tempfile

from dogtail.utils import doDelay

from conftest import find_app


def test_vcs_window_opens(app_process):
    """A git repo directory should open the VCS window."""
    with tempfile.TemporaryDirectory() as repo:
        subprocess.run(["git", "init", repo], check=True, capture_output=True)
        subprocess.run(
            ["git", "-C", repo, "config", "user.email", "test@test.com"],
            check=True, capture_output=True,
        )
        subprocess.run(
            ["git", "-C", repo, "config", "user.name", "Test"],
            check=True, capture_output=True,
        )
        filepath = os.path.join(repo, "file.txt")
        with open(filepath, "w") as f:
            f.write("original\n")
        subprocess.run(["git", "-C", repo, "add", "."], check=True, capture_output=True)
        subprocess.run(
            ["git", "-C", repo, "commit", "-m", "init"],
            check=True, capture_output=True,
        )
        with open(filepath, "w") as f:
            f.write("modified\n")

        app_process(repo)
        app = find_app()
        doDelay(1)
        windows = app.findChildren(lambda n: n.roleName == "frame")
        assert len(windows) >= 1, "No VCS window found"
