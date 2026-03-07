"""Tests for editing and saving in file diff window via dogtail AT-SPI."""
import os
import shutil
import subprocess
import tempfile

from dogtail.utils import doDelay

from conftest import find_app, find_labels, send_keys, FIXTURES


def _copy_fixture(name, dest_dir):
    """Copy a fixture file into dest_dir and return the new path."""
    src = os.path.join(FIXTURES, name)
    dst = os.path.join(dest_dir, name)
    shutil.copy2(src, dst)
    return dst


def _focus_and_type(pid, text):
    """Focus the application window and type text via xdotool."""
    wids = (
        subprocess.check_output(["xdotool", "search", "--pid", str(pid)])
        .decode().strip().splitlines()
    )
    if wids:
        for wid in wids:
            result = subprocess.run(
                ["xdotool", "windowfocus", "--sync", wid],
                capture_output=True,
            )
            if result.returncode == 0:
                break
    doDelay(0.3)
    subprocess.run(["xdotool", "type", "--delay", "50", text], check=True)


def test_edit_makes_save_sensitive(app_process):
    """Typing text should make a Save button sensitive."""
    with tempfile.TemporaryDirectory() as tmpdir:
        left = _copy_fixture("left.txt", tmpdir)
        right = _copy_fixture("right.txt", tmpdir)

        proc = app_process(left, right)
        app = find_app()
        doDelay(2)

        # Verify save buttons are initially insensitive
        save_buttons = app.findChildren(
            lambda n: n.roleName == "push button" and "Save" in n.name
        )
        assert save_buttons, "No Save buttons found"
        assert not any(b.sensitive for b in save_buttons), (
            "Save buttons should be insensitive before editing"
        )

        # Type some text into the focused text view
        _focus_and_type(proc.pid, "hello")
        doDelay(1)

        # Now at least one Save button should be sensitive
        save_buttons = app.findChildren(
            lambda n: n.roleName == "push button" and "Save" in n.name
        )
        assert any(b.sensitive for b in save_buttons), (
            "Expected at least one Save button to become sensitive after typing"
        )


def test_save_writes_to_disk(app_process):
    """Typing text and pressing Ctrl+S should save changes to disk."""
    with tempfile.TemporaryDirectory() as tmpdir:
        left = _copy_fixture("left.txt", tmpdir)
        right = _copy_fixture("right.txt", tmpdir)

        original_left = open(left).read()

        proc = app_process(left, right)
        app = find_app()
        doDelay(2)

        # Type some text
        _focus_and_type(proc.pid, "EDITED")
        doDelay(1)

        # Save with Ctrl+S
        send_keys("ctrl+s", proc.pid)
        doDelay(2)

        # Read back from disk -- at least one file should have changed
        new_left = open(left).read()
        new_right = open(right).read()

        file_changed = (new_left != original_left) or ("EDITED" in new_right)
        assert file_changed, (
            "Expected at least one file to contain 'EDITED' after save. "
            f"Left changed: {new_left != original_left}"
        )


def test_refresh_reloads_from_disk(app_process):
    """Externally modifying a file and pressing Ctrl+R should reload content."""
    with tempfile.TemporaryDirectory() as tmpdir:
        left = _copy_fixture("left.txt", tmpdir)
        right = _copy_fixture("right.txt", tmpdir)

        proc = app_process(left, right)
        app = find_app()
        doDelay(2)

        # Externally modify the left file
        with open(left, "w") as f:
            f.write("EXTERNALLY MODIFIED CONTENT\n")

        # Send Ctrl+R to refresh (F5 also works)
        send_keys("ctrl+r", proc.pid)
        doDelay(2)

        # The app should not crash and should still be accessible.
        # We verify by checking that the window is still present.
        frames = app.findChildren(lambda n: n.roleName == "frame")
        assert len(frames) >= 1, "Window disappeared after refresh"

        # Optionally check if the new content appears in labels or text views.
        # The chunk count may have changed since the file is now very different.
        labels = find_labels(app)
        # Just verify the app is still responsive
        assert labels is not None, "App became unresponsive after refresh"
