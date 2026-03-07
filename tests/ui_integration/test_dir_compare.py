"""Directory comparison tests via dogtail."""
from dogtail.utils import doDelay

from conftest import find_app


def _find_labels(app):
    """Return text of all visible labels."""
    labels = app.findChildren(lambda n: n.roleName == "label" and n.showing)
    texts = []
    for lbl in labels:
        try:
            texts.append(lbl.text or lbl.name or "")
        except Exception:
            pass
    return texts


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


def test_dir_shows_different_file(app_process, fixture_path):
    """Files that differ should be listed."""
    app_process(fixture_path("left_dir"), fixture_path("right_dir"))
    app = find_app()
    doDelay(2)
    labels = _find_labels(app)
    assert any("different.txt" in t for t in labels), \
        f"Expected 'different.txt' in labels: {labels}"


def test_dir_shows_only_left_file(app_process, fixture_path):
    """File only in left dir should be listed."""
    app_process(fixture_path("left_dir"), fixture_path("right_dir"))
    app = find_app()
    doDelay(2)
    labels = _find_labels(app)
    assert any("only_left.txt" in t for t in labels), \
        f"Expected 'only_left.txt' in labels: {labels}"


def test_dir_shows_only_right_file(app_process, fixture_path):
    """File only in right dir should be listed."""
    app_process(fixture_path("left_dir"), fixture_path("right_dir"))
    app = find_app()
    doDelay(2)
    labels = _find_labels(app)
    assert any("only_right.txt" in t for t in labels), \
        f"Expected 'only_right.txt' in labels: {labels}"
