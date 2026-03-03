"""Smoke tests for file diff window via dogtail AT-SPI."""
import dogtail.tree
from dogtail.config import config

config.searchShowingOnly = True


def test_file_diff_opens(app_process, fixture_path):
    """Open a two-file diff and verify the window is accessible via AT-SPI."""
    left = fixture_path("left.txt")
    right = fixture_path("right.txt")

    app_process(left, right)

    # Find the mergers application in the accessibility tree
    app = dogtail.tree.root.application("mergers")
    assert app is not None, "mergers app not found in AT-SPI tree"

    # Verify a window exists
    windows = app.findChildren(lambda n: n.roleName == "frame")
    assert len(windows) >= 1, "No window found"
