"""UI integration tests for multi-file patch viewing."""
import os
import pytest

from conftest import (
    FIXTURES,
    _launch_and_wait,
    _kill_proc,
    find_app,
)
from dogtail.utils import doDelay


@pytest.fixture(scope="module")
def shared_multi_patch_app():
    """Multi-file patch: left_dir + dirs.patch."""
    base = os.path.join(FIXTURES, "left_dir")
    patch = os.path.join(FIXTURES, "dirs.patch")
    proc, app = _launch_and_wait(base, patch)
    yield proc, app
    _kill_proc(proc)


def test_window_opens(shared_multi_patch_app):
    proc, app = shared_multi_patch_app
    frames = app.findChildren(lambda n: n.roleName == "frame")
    assert len(frames) >= 1, "No window frame found"


def test_shows_patched_files(shared_multi_patch_app):
    proc, app = shared_multi_patch_app
    all_text = []
    for node in app.findChildren(lambda n: n.showing):
        try:
            text = node.text or node.name or ""
            if text:
                all_text.append(text)
        except Exception:
            pass
    combined = " ".join(all_text)
    assert "different.txt" in combined, \
        f"'different.txt' not found in window content"


def test_shows_subdirectory(shared_multi_patch_app):
    """Directory view should show subdirectories from the patch."""
    proc, app = shared_multi_patch_app
    all_text = []
    for node in app.findChildren(lambda n: n.showing):
        try:
            text = node.text or node.name or ""
            if text:
                all_text.append(text)
        except Exception:
            pass
    combined = " ".join(all_text)
    assert "subdir" in combined, \
        f"'subdir' not found in window content"
