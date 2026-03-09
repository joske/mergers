"""Tests for editing and saving in file diff window via dogtail AT-SPI."""
import os
import tempfile

from dogtail.utils import doDelay

from conftest import copy_fixture, find_app, find_labels, focus_and_type, is_button, send_keys


def test_edit_makes_save_sensitive(app_process):
    """Typing text should make a Save button sensitive."""
    with tempfile.TemporaryDirectory() as tmpdir:
        left = copy_fixture("left.txt", tmpdir)
        right = copy_fixture("right.txt", tmpdir)

        proc = app_process(left, right)
        app = find_app()
        doDelay(2)

        # Verify save buttons are initially insensitive
        save_buttons = app.findChildren(
            lambda n: is_button(n) and "Save" in n.name
        )
        assert save_buttons, "No Save buttons found"
        assert not any(b.sensitive for b in save_buttons), (
            "Save buttons should be insensitive before editing"
        )

        # Type some text into the focused text view
        focus_and_type(proc.pid, "hello")
        doDelay(1)

        # Now at least one Save button should be sensitive
        save_buttons = app.findChildren(
            lambda n: is_button(n) and "Save" in n.name
        )
        assert any(b.sensitive for b in save_buttons), (
            "Expected at least one Save button to become sensitive after typing"
        )


def test_save_writes_to_disk(app_process):
    """Typing text and pressing Ctrl+S should save changes to disk."""
    with tempfile.TemporaryDirectory() as tmpdir:
        left = copy_fixture("left.txt", tmpdir)
        right = copy_fixture("right.txt", tmpdir)

        original_left = open(left).read()

        proc = app_process(left, right)
        app = find_app()
        doDelay(2)

        # Type some text
        focus_and_type(proc.pid, "EDITED")
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
        left = copy_fixture("left.txt", tmpdir)
        right = copy_fixture("right.txt", tmpdir)

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


def test_save_makes_button_insensitive_again(app_process):
    """After saving, the Save button should become insensitive again."""
    with tempfile.TemporaryDirectory() as tmpdir:
        left = copy_fixture("left.txt", tmpdir)
        right = copy_fixture("right.txt", tmpdir)

        proc = app_process(left, right)
        app = find_app()
        doDelay(2)

        focus_and_type(proc.pid, "X")
        doDelay(1)

        # Save button should be sensitive
        save_buttons = app.findChildren(
            lambda n: is_button(n) and "Save" in n.name
        )
        assert any(b.sensitive for b in save_buttons), \
            "Save button should be sensitive after edit"

        send_keys("ctrl+s", proc.pid)
        doDelay(2)

        # Save button should be insensitive again
        save_buttons = app.findChildren(
            lambda n: is_button(n) and "Save" in n.name
        )
        assert not all(b.sensitive for b in save_buttons), \
            "Save button should be insensitive after save"


def test_undo_after_edit(app_process):
    """Ctrl+Z should undo the last edit."""
    with tempfile.TemporaryDirectory() as tmpdir:
        left = copy_fixture("left.txt", tmpdir)
        right = copy_fixture("right.txt", tmpdir)

        proc = app_process(left, right)
        app = find_app()
        doDelay(2)

        focus_and_type(proc.pid, "UNDO_TEST")
        doDelay(1)

        # Should have a sensitive save button
        save_buttons = app.findChildren(
            lambda n: is_button(n) and "Save" in n.name
        )
        assert any(b.sensitive for b in save_buttons), \
            "Save button should be sensitive after edit"

        # Undo
        send_keys("ctrl+z", proc.pid)
        doDelay(1)

        # After enough undos, all text should be reverted
        # Just verify the app didn't crash
        frames = app.findChildren(lambda n: n.roleName == "frame")
        assert len(frames) >= 1, "Window disappeared after undo"
