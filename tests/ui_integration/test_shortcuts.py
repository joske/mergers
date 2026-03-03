"""Keyboard shortcut tests for file diff window via dogtail AT-SPI."""
import dogtail.rawinput
from dogtail.utils import doDelay

from conftest import find_app


def _open_diff(app_process, fixture_path):
    """Launch mergers with left/right fixtures and return the AT-SPI app node."""
    app_process(fixture_path("left.txt"), fixture_path("right.txt"))
    app = find_app()
    # Grab focus so keyboard events go to the app, not the terminal
    frame = app.findChildren(lambda n: n.roleName == "frame")[0]
    frame.grabFocus()
    doDelay(0.5)
    return app


def test_ctrl_f_opens_find_bar(app_process, fixture_path):
    """Ctrl+F should reveal the find bar with a text entry."""
    app = _open_diff(app_process, fixture_path)

    dogtail.rawinput.keyCombo("<Control>f")
    doDelay(1)

    entries = app.findChildren(
        lambda n: n.roleName == "text" and n.showing
    )
    assert len(entries) >= 1, "No text entry found after Ctrl+F"


def test_alt_down_navigates_to_next_chunk(app_process, fixture_path):
    """Alt+Down should navigate to the next diff chunk."""
    app = _open_diff(app_process, fixture_path)

    dogtail.rawinput.keyCombo("<Alt>Down")
    doDelay(1)

    labels = app.findChildren(
        lambda n: n.roleName == "label" and n.showing
    )
    chunk_label = None
    for label in labels:
        try:
            text = label.text or label.name or ""
            if " of " in text:
                chunk_label = label
                break
        except Exception:
            continue

    assert chunk_label is not None, (
        "Chunk navigation label not found after Alt+Down. "
        f"Labels found: {[getattr(l, 'name', '') for l in labels]}"
    )
    assert "1 of " in (chunk_label.text or chunk_label.name), (
        f"Expected '1 of ...' but got '{chunk_label.text or chunk_label.name}'"
    )


def test_escape_closes_find_bar(app_process, fixture_path):
    """Escape should close the find bar after Ctrl+F opens it."""
    app = _open_diff(app_process, fixture_path)

    # Open find bar
    dogtail.rawinput.keyCombo("<Control>f")
    doDelay(1)

    entries_before = app.findChildren(
        lambda n: n.roleName == "text" and n.showing
    )
    assert len(entries_before) >= 1, "Find bar did not open"

    # Press Escape to close it
    dogtail.rawinput.pressKey("Escape")
    doDelay(1)
