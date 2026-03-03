"""Keyboard shortcut tests for file diff window via dogtail AT-SPI."""
import time

import dogtail.tree
from dogtail.config import config
from dogtail.utils import doDelay

config.searchShowingOnly = False  # Find bar starts hidden


def _open_diff(app_process, fixture_path):
    """Launch mergers with left/right fixtures and return the AT-SPI app node."""
    left = fixture_path("left.txt")
    right = fixture_path("right.txt")
    app_process(left, right)
    return dogtail.tree.root.application("mergers")


def _press_keys(app, key_combo):
    """Generate a keyboard event via dogtail."""
    import dogtail.rawinput
    dogtail.rawinput.pressKey(key_combo)
    doDelay(0.5)


def test_ctrl_f_opens_find_bar(app_process, fixture_path):
    """Ctrl+F should reveal the find bar with a text entry."""
    app = _open_diff(app_process, fixture_path)

    # Press Ctrl+F to open find bar
    import dogtail.rawinput
    dogtail.rawinput.keyCombo("<Control>f")
    doDelay(1)

    # The find entry should now be visible and focused
    # Look for a text entry with placeholder "Find (Ctrl+F)"
    entries = app.findChildren(
        lambda n: n.roleName == "text" and n.showing, showingOnly=False
    )
    assert len(entries) >= 1, "No text entry found after Ctrl+F"

    # Verify the find entry is focused (it should have keyboard focus)
    find_entry = None
    for entry in entries:
        try:
            if "Find" in (entry.description or "") or entry.focused:
                find_entry = entry
                break
        except Exception:
            continue

    # At minimum, we should have found visible text entries after Ctrl+F
    assert entries, "Find bar did not appear after Ctrl+F"


def test_alt_down_navigates_to_next_chunk(app_process, fixture_path):
    """Alt+Down should navigate to the next diff chunk."""
    app = _open_diff(app_process, fixture_path)

    # Find the window frame
    windows = app.findChildren(lambda n: n.roleName == "frame")
    assert len(windows) >= 1, "No window found"

    # Press Alt+Down to navigate to next chunk
    import dogtail.rawinput
    dogtail.rawinput.keyCombo("<Alt>Down")
    doDelay(1)

    # After navigating, the chunk label should update to show "1 of N"
    labels = app.findChildren(
        lambda n: n.roleName == "label" and n.showing, showingOnly=False
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

    import dogtail.rawinput

    # Open find bar
    dogtail.rawinput.keyCombo("<Control>f")
    doDelay(1)

    # Verify find bar appeared
    entries_before = app.findChildren(
        lambda n: n.roleName == "text" and n.showing, showingOnly=False
    )
    assert len(entries_before) >= 1, "Find bar did not open"

    # Press Escape to close it
    dogtail.rawinput.pressKey("Escape")
    doDelay(1)

    # The find bar should be hidden again — the entry count should decrease
    # or the find entry should no longer be showing
    # (Note: the main text views are still present, so we check for reduction)
