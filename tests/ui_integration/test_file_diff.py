"""Smoke tests for file diff window via dogtail AT-SPI."""
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


def test_file_diff_opens(app_process, fixture_path):
    """Open a two-file diff and verify the window is accessible via AT-SPI."""
    app_process(fixture_path("left.txt"), fixture_path("right.txt"))
    app = find_app()

    # Verify a window exists
    windows = app.findChildren(lambda n: n.roleName == "frame")
    assert len(windows) >= 1, "No window found"


def test_save_button_initially_insensitive(app_process, fixture_path):
    """Save buttons should be insensitive when no changes have been made."""
    app_process(fixture_path("left.txt"), fixture_path("right.txt"))
    app = find_app()
    buttons = app.findChildren(
        lambda n: n.roleName == "push button" and "Save" in n.name
    )
    for btn in buttons:
        assert not btn.sensitive, f"Save button '{btn.name}' should be insensitive initially"


def test_diff_shows_file_names(app_process, fixture_path):
    """The diff window should show the file names being compared."""
    app_process(fixture_path("left.txt"), fixture_path("right.txt"))
    app = find_app()
    doDelay(1)
    labels = _find_labels(app)
    assert any("left.txt" in t for t in labels), \
        f"Expected 'left.txt' in labels: {labels}"
    assert any("right.txt" in t for t in labels), \
        f"Expected 'right.txt' in labels: {labels}"


def test_three_way_merge_opens(app_process, fixture_path):
    """Opening three files should create a merge view."""
    app_process(
        fixture_path("left.txt"),
        fixture_path("base.txt"),
        fixture_path("right.txt"),
    )
    app = find_app()
    windows = app.findChildren(lambda n: n.roleName == "frame")
    assert len(windows) >= 1, "No window found for 3-way merge"


def test_identical_files_no_chunks(app_process, fixture_path):
    """Diffing a file against itself should show no diff chunks."""
    app_process(fixture_path("left.txt"), fixture_path("left.txt"))
    app = find_app()
    doDelay(1)
    labels = _find_labels(app)
    # The chunk navigation label should show "0 of 0" or similar
    assert any("0" in t for t in labels), \
        f"Expected chunk count with 0 in labels: {labels}"


def test_chunk_navigation_label(app_process, fixture_path):
    """Diff with changes should show chunk count in navigation label."""
    app_process(fixture_path("left.txt"), fixture_path("right.txt"))
    app = find_app()
    doDelay(1)
    labels = _find_labels(app)
    # Should show something like "1 of 3" or "0 of 3"
    chunk_labels = [t for t in labels if " of " in t]
    assert len(chunk_labels) > 0, \
        f"Expected chunk navigation label with ' of ' in labels: {labels}"


def test_blank_comparison(app_process):
    """Starting with no arguments should open a blank comparison."""
    app_process()
    app = find_app()
    doDelay(1)
    windows = app.findChildren(lambda n: n.roleName == "frame")
    assert len(windows) >= 1, "No window found for blank comparison"
