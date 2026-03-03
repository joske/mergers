"""Keyboard shortcut tests for file diff window via dogtail AT-SPI."""
import dogtail.rawinput
from dogtail.utils import doDelay

from conftest import find_app


def _open_diff(app_process, fixture_path):
    """Launch mergers with left/right fixtures and return the AT-SPI app node."""
    app_process(fixture_path("left.txt"), fixture_path("right.txt"))
    return find_app()


def test_ctrl_f_opens_find_bar(app_process, fixture_path):
    """Ctrl+F should reveal the find bar with a text entry."""
    app = _open_diff(app_process, fixture_path)

    dogtail.rawinput.keyCombo("<Control>f")
    doDelay(1)

    entries = app.findChildren(
        lambda n: n.roleName == "text" and n.showing
    )
    assert len(entries) >= 1, "No text entry found after Ctrl+F"


def test_next_chunk_navigation(app_process, fixture_path):
    """Clicking 'Next change' button should update the chunk label."""
    app = _open_diff(app_process, fixture_path)

    # Find and click the "Next change" button
    buttons = app.findChildren(lambda n: n.roleName == "push button")
    next_btn = None
    for btn in buttons:
        if "Next change" in (btn.name or ""):
            next_btn = btn
            break

    assert next_btn is not None, (
        f"Next change button not found. Buttons: {[b.name for b in buttons]}"
    )

    next_btn.click()
    doDelay(1)

    # After navigating, the chunk label should show "Change 1 of N"
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
        "Chunk navigation label not found after clicking Next. "
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
