"""Merge save test — needs its own app instance with writable temp files."""
import os
import shutil
import subprocess
import tempfile

from dogtail.utils import doDelay

from conftest import find_app, send_keys, FIXTURES


def _copy_fixture(name, dest_dir):
    src = os.path.join(FIXTURES, name)
    dst = os.path.join(dest_dir, name)
    shutil.copy2(src, dst)
    return dst


def _focus_and_type(pid, text):
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


def test_merge_ctrl_s_saves_middle(app_process):
    """In merge view, typing and Ctrl+S should save the middle pane."""
    with tempfile.TemporaryDirectory() as tmpdir:
        left = _copy_fixture("left.txt", tmpdir)
        base = _copy_fixture("base.txt", tmpdir)
        right = _copy_fixture("right.txt", tmpdir)

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

        _focus_and_type(proc.pid, "MERGE_EDIT")
        doDelay(1)

        send_keys("ctrl+s", proc.pid)
        doDelay(2)

        new_base = open(base).read()
        assert new_base != original_base or "MERGE_EDIT" in new_base, (
            f"Expected base file to change after save. "
            f"Original length: {len(original_base)}, new length: {len(new_base)}"
        )
