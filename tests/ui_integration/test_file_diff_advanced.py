"""Advanced file diff tests: binary detection, swap panes, go-to-line."""
from dogtail.utils import doDelay

from conftest import find_app, send_keys


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


def _open_diff(app_process, fixture_path):
    """Launch mergers with left/right fixtures and return (app, proc)."""
    proc = app_process(fixture_path("left.txt"), fixture_path("right.txt"))
    app = find_app()
    return app, proc


def test_binary_file_shows_info_bar(app_process, fixture_path):
    """Opening a binary file should show a 'Binary file' info bar message."""
    app_process(fixture_path("binary.bin"), fixture_path("left.txt"))
    app = find_app()
    doDelay(2)
    labels = _find_labels(app)
    assert any("Binary" in t for t in labels), (
        f"Expected 'Binary' info bar label, got labels: {labels}"
    )


def test_swap_panes(app_process, fixture_path):
    """Swap panes should reverse the file path labels."""
    app, proc = _open_diff(app_process, fixture_path)
    doDelay(1)

    labels_before = _find_labels(app)
    # Find the path labels (contain fixtures/)
    path_labels_before = [t for t in labels_before if "fixtures/" in t]
    assert len(path_labels_before) >= 2, (
        f"Expected at least 2 path labels, got: {labels_before}"
    )

    # Click the swap button
    swap_btn = app.findChild(
        lambda n: n.roleName == "push button" and n.name == "Swap panes"
    )
    assert swap_btn is not None, "Swap panes button not found"
    swap_btn.do_action(0)
    doDelay(1)

    labels_after = _find_labels(app)
    path_labels_after = [t for t in labels_after if "fixtures/" in t]
    assert len(path_labels_after) >= 2, (
        f"Lost path labels after swap: {labels_after}"
    )
    # After swap the order should be reversed
    assert path_labels_after != path_labels_before, (
        f"Path labels did not change after swap: {path_labels_after}"
    )


def test_go_to_line_opens_with_ctrl_l(app_process, fixture_path):
    """Ctrl+L should reveal the go-to-line entry."""
    app, proc = _open_diff(app_process, fixture_path)

    send_keys("ctrl+l", proc.pid)
    doDelay(1)

    entries = app.findChildren(
        lambda n: n.roleName == "text" and n.showing
    )
    assert len(entries) >= 1, "No text entry found after Ctrl+L (go-to-line)"
