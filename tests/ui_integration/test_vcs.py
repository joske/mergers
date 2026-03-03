"""VCS (git) window tests via dogtail."""
import os
import subprocess
import tempfile

import dogtail.tree
from dogtail.config import config
from dogtail.utils import doDelay

config.searchShowingOnly = True


def test_vcs_window_opens(app_process):
    """A git repo directory should open the VCS window."""
    # Create a temp git repo with a modified file
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
        # Create and commit a file
        filepath = os.path.join(repo, "file.txt")
        with open(filepath, "w") as f:
            f.write("original\n")
        subprocess.run(["git", "-C", repo, "add", "."], check=True, capture_output=True)
        subprocess.run(
            ["git", "-C", repo, "commit", "-m", "init"],
            check=True, capture_output=True,
        )
        # Modify the file
        with open(filepath, "w") as f:
            f.write("modified\n")

        app_process(repo)
        app = dogtail.tree.root.application("mergers")
        doDelay(1)
        windows = app.findChildren(lambda n: n.roleName == "frame")
        assert len(windows) >= 1, "No VCS window found"
