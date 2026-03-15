"""UI integration tests for patch detection and misc behavior."""
import os
import pytest

from conftest import (
    FIXTURES,
    find_app,
    send_keys,
)
from dogtail.utils import doDelay


def test_diff_extension_detected(app_process, fixture_path):
    """A .diff file should trigger patch mode."""
    import shutil
    import tempfile

    with tempfile.TemporaryDirectory() as tmp:
        diff_file = os.path.join(tmp, "changes.diff")
        shutil.copy2(fixture_path("left_to_right.patch"), diff_file)
        proc = app_process(fixture_path("left.txt"), diff_file)
        app = find_app()
        doDelay(1)
        frames = app.findChildren(lambda n: n.roleName == "frame")
        assert any("patch" in (f.name or "").lower() for f in frames), \
            f"No patch mode with .diff extension: {[f.name for f in frames]}"


def test_ctrl_w_closes_patch_window(app_process, fixture_path):
    """Ctrl+W should close a patch window cleanly."""
    proc = app_process(
        fixture_path("left.txt"),
        fixture_path("left_to_right.patch"),
    )
    app = find_app()
    doDelay(1)
    send_keys("ctrl+w", proc.pid)
    doDelay(1)
    retcode = proc.poll()
    assert retcode is not None, "Patch window didn't close on Ctrl+W"
