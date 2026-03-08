"""Merge save test — needs its own app instance with writable temp files."""
import os
import tempfile

from dogtail.utils import doDelay

from conftest import copy_fixture, find_app, focus_and_type, send_keys


def test_merge_ctrl_s_saves_middle(app_process):
    """In merge view, typing and Ctrl+S should save the middle pane."""
    with tempfile.TemporaryDirectory() as tmpdir:
        left = copy_fixture("left.txt", tmpdir)
        base = copy_fixture("base.txt", tmpdir)
        right = copy_fixture("right.txt", tmpdir)

        original_base = open(base).read()

        proc = app_process(left, base, right)
        app = find_app()
        doDelay(2)

        # Try to focus the middle text view
        text_views = app.findChildren(lambda n: n.roleName == "text" and n.showing)
        if len(text_views) >= 3:
            try:
                text_views[1].grabFocus()
            except Exception:
                pass
        doDelay(1)

        focus_and_type(proc.pid, "MERGE_EDIT")
        doDelay(1)

        send_keys("ctrl+s", proc.pid)
        doDelay(2)

        new_base = open(base).read()
        assert "MERGE_EDIT" in new_base, (
            f"Expected base file to contain 'MERGE_EDIT' after save. "
            f"Content: {new_base[:200]}"
        )
