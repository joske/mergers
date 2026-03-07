"""3-way merge view tests via dogtail AT-SPI."""
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


def _open_merge(app_process, fixture_path):
    """Launch mergers in 3-way merge mode and return (app, proc)."""
    proc = app_process(
        fixture_path("left.txt"),
        fixture_path("base.txt"),
        fixture_path("right.txt"),
    )
    app = find_app()
    return app, proc


def test_merge_opens_with_three_panes(app_process, fixture_path):
    """3-way merge should create a window with 3 scrollable text panes."""
    app, proc = _open_merge(app_process, fixture_path)
    doDelay(2)

    # In merge mode, there are 3 text views (left, middle, right)
    text_views = app.findChildren(
        lambda n: n.roleName == "text" and n.showing
    )
    # There should be at least 3 text views for the 3 panes
    # (there may be more if find bar entries are present)
    assert len(text_views) >= 3, (
        f"Expected at least 3 text panes in merge view, found {len(text_views)}"
    )


def test_merge_shows_file_names(app_process, fixture_path):
    """Merge view should display all three file names in labels."""
    app, proc = _open_merge(app_process, fixture_path)
    doDelay(2)

    labels = _find_labels(app)
    assert any("left.txt" in t for t in labels), (
        f"Expected 'left.txt' in labels: {labels}"
    )
    assert any("base.txt" in t for t in labels), (
        f"Expected 'base.txt' in labels: {labels}"
    )
    assert any("right.txt" in t for t in labels), (
        f"Expected 'right.txt' in labels: {labels}"
    )


def test_merge_window_title_contains_file_names(app_process, fixture_path):
    """Merge tab label should contain all three file names."""
    app, proc = _open_merge(app_process, fixture_path)
    doDelay(2)

    labels = _find_labels(app)
    # The tab title format is "left.txt \u2014 base.txt \u2014 right.txt"
    tab_labels = [t for t in labels if "left.txt" in t and "right.txt" in t]
    assert tab_labels, (
        f"Expected a tab label containing both file names, got: {labels}"
    )


def test_merge_has_one_save_button(app_process, fixture_path):
    """Only the middle pane should have a save button in merge mode."""
    app, proc = _open_merge(app_process, fixture_path)
    doDelay(2)

    save_buttons = app.findChildren(
        lambda n: n.roleName == "push button" and "Save" in n.name and n.showing
    )
    # In merge mode, only middle pane has a save button
    assert len(save_buttons) == 1, (
        f"Expected exactly 1 save button in merge view, found {len(save_buttons)}: "
        f"{[b.name for b in save_buttons]}"
    )
