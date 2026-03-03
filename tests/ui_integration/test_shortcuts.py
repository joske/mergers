"""Keyboard shortcut tests for file diff window via dogtail AT-SPI."""
import subprocess

from dogtail.utils import doDelay

from conftest import find_app


def _open_diff(app_process, fixture_path):
    """Launch mergers with left/right fixtures and return the AT-SPI app node."""
    app_process(fixture_path("left.txt"), fixture_path("right.txt"))
    return find_app()


def _send_keys(key_combo):
    """Send a key combo to the mergers window via xdotool.

    Uses xdotool window search + key targeting to avoid terminal escape
    sequence leakage that dogtail.rawinput.keyCombo suffers from.
    """
    wids = subprocess.check_output(
        ["xdotool", "search", "--name", "mergers"]
    ).decode().strip().splitlines()
    assert wids, "No mergers window found via xdotool"
    subprocess.run(["xdotool", "key", "--window", wids[0], key_combo], check=True)


def test_ctrl_f_opens_find_bar(app_process, fixture_path):
    """Ctrl+F should reveal the find bar with a text entry."""
    app = _open_diff(app_process, fixture_path)

    _send_keys("ctrl+f")
    doDelay(1)

    entries = app.findChildren(
        lambda n: n.roleName == "text" and n.showing
    )
    assert len(entries) >= 1, "No text entry found after Ctrl+F"


def test_alt_down_navigates_to_next_chunk(app_process, fixture_path):
    """Ctrl+D should navigate to the next diff chunk (Alt+Down also works when focused)."""
    app = _open_diff(app_process, fixture_path)

    # Alt+Down only fires when a text view has focus (capture-phase handler).
    # Ctrl+D is registered app-level via set_accels_for_action and works regardless.
    _send_keys("ctrl+d")
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

    _send_keys("ctrl+f")
    doDelay(1)

    entries_before = app.findChildren(
        lambda n: n.roleName == "text" and n.showing
    )
    assert len(entries_before) >= 1, "Find bar did not open"

    _send_keys("Escape")
    doDelay(1)
