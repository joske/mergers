"""UI integration tests for single-file patch viewing."""
import os
import pytest

from conftest import (
    FIXTURES,
    _launch_and_wait,
    _kill_proc,
    find_app,
    find_labels,
    send_keys,
    wait_for_label,
)
from dogtail.utils import doDelay


@pytest.fixture(scope="module")
def shared_single_patch_app():
    """Single-file patch: left.txt + left_to_right.patch."""
    base = os.path.join(FIXTURES, "left.txt")
    patch = os.path.join(FIXTURES, "left_to_right.patch")
    proc, app = _launch_and_wait(base, patch)
    yield proc, app
    _kill_proc(proc)


def test_window_title_contains_patch(shared_single_patch_app):
    proc, app = shared_single_patch_app
    frames = app.findChildren(lambda n: n.roleName == "frame")
    assert any("patch" in (f.name or "").lower() for f in frames), \
        f"No frame with 'patch' in title: {[f.name for f in frames]}"


def test_window_title_contains_filename(shared_single_patch_app):
    proc, app = shared_single_patch_app
    frames = app.findChildren(lambda n: n.roleName == "frame")
    assert any("left.txt" in (f.name or "") for f in frames), \
        f"No frame with 'left.txt' in title: {[f.name for f in frames]}"


def test_has_two_text_panes(shared_single_patch_app):
    proc, app = shared_single_patch_app
    texts = app.findChildren(lambda n: n.roleName == "text")
    assert len(texts) >= 2, f"Expected >=2 text panes, got {len(texts)}"


def test_left_pane_contains_original(shared_single_patch_app):
    proc, app = shared_single_patch_app
    texts = app.findChildren(lambda n: n.roleName == "text")
    left_text = texts[0].text if texts else ""
    assert "original line 6" in left_text, \
        f"Left pane missing original content: {left_text[:200]}"


def test_right_pane_contains_patched(shared_single_patch_app):
    proc, app = shared_single_patch_app
    texts = app.findChildren(lambda n: n.roleName == "text")
    right_text = texts[1].text if len(texts) > 1 else ""
    assert "modified line 6" in right_text, \
        f"Right pane missing patched content: {right_text[:200]}"


def test_right_pane_has_insertion(shared_single_patch_app):
    proc, app = shared_single_patch_app
    texts = app.findChildren(lambda n: n.roleName == "text")
    right_text = texts[1].text if len(texts) > 1 else ""
    assert "inserted line 7.5" in right_text, \
        f"Right pane missing inserted line: {right_text[:200]}"


def test_chunk_label_shows_changes(shared_single_patch_app):
    proc, app = shared_single_patch_app
    result = wait_for_label(app, lambda t: "change" in t.lower())
    assert result is not None, \
        f"No label with 'change' found: {find_labels(app)}"


def test_tab_label_contains_patch(shared_single_patch_app):
    proc, app = shared_single_patch_app
    tabs = app.findChildren(lambda n: n.roleName == "page tab")
    tab_names = [t.name or "" for t in tabs]
    assert any("patch" in name.lower() for name in tab_names), \
        f"No tab with 'patch' in label: {tab_names}"
