"""Directory comparison tests via dogtail."""
import dogtail.tree
from dogtail.config import config
from dogtail.utils import doDelay

config.searchShowingOnly = True


def test_dir_comparison_opens(app_process, fixture_path):
    """Two directories should open directory comparison window."""
    app_process(fixture_path("left_dir"), fixture_path("right_dir"))
    app = dogtail.tree.root.application("mergers")
    windows = app.findChildren(lambda n: n.roleName == "frame")
    assert len(windows) >= 1, "No window found for directory comparison"


def test_dir_window_has_tree_view(app_process, fixture_path):
    """Directory window should contain a tree/table view."""
    app_process(fixture_path("left_dir"), fixture_path("right_dir"))
    app = dogtail.tree.root.application("mergers")
    doDelay(1)
    # Look for table or tree-like widgets
    tables = app.findChildren(
        lambda n: n.roleName in ("table", "tree table", "tree")
    )
    assert len(tables) > 0, "No tree/table view found in directory window"
