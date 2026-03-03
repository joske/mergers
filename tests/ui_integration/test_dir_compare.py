"""Directory comparison tests via dogtail."""
from dogtail.utils import doDelay

from conftest import find_app


def test_dir_comparison_opens(app_process, fixture_path):
    """Two directories should open directory comparison window."""
    app_process(fixture_path("left_dir"), fixture_path("right_dir"))
    app = find_app()
    windows = app.findChildren(lambda n: n.roleName == "frame")
    assert len(windows) >= 1, "No window found for directory comparison"


def test_dir_window_has_tree_view(app_process, fixture_path):
    """Directory window should contain a tree/table view."""
    app_process(fixture_path("left_dir"), fixture_path("right_dir"))
    app = find_app()
    doDelay(1)
    # GTK4 ColumnView exposes various AT-SPI roles depending on version
    views = app.findChildren(
        lambda n: n.roleName in ("table", "tree table", "tree", "list", "list box", "panel")
        and n.showing
    )
    assert len(views) > 0, "No list/table/tree view found in directory window"
