"""Directory comparison tests that need their own app instance."""
import os

from dogtail.utils import doDelay

from conftest import find_app, find_labels, send_keys, FIXTURES


def test_dir_alt_tab_switching(app_process):
    """Alt+1 should switch to the first (dir) tab."""
    left = os.path.join(FIXTURES, "left_dir")
    right = os.path.join(FIXTURES, "right_dir")
    proc = app_process(left, right)
    app = find_app()
    doDelay(1)

    # Open a file tab by navigating to a file row
    send_keys("Down", proc.pid)
    doDelay(0.3)
    send_keys("Down", proc.pid)
    doDelay(0.3)
    send_keys("Return", proc.pid)
    doDelay(2)

    tabs = app.findChildren(lambda n: n.roleName == "page tab" and n.showing)
    assert len(tabs) >= 2, f"Expected at least 2 tabs, got {len(tabs)}"

    # Switch to first tab with Alt+1
    send_keys("alt+1", proc.pid)
    doDelay(1)

    # Dir tab should have dir labels visible
    labels = find_labels(app)
    all_children = app.findChildren(lambda n: n.showing and n.name)
    all_names = [n.name for n in all_children]
    has_dir = any("left_dir" in t or "right_dir" in t or "different" in t
                  for t in labels + all_names)
    assert has_dir, f"Expected dir content after Alt+1: {labels}"


def test_dir_enter_on_dir_row_expands(app_process):
    """Enter on a directory row should expand it, not open a tab."""
    left = os.path.join(FIXTURES, "left_dir")
    right = os.path.join(FIXTURES, "right_dir")
    proc = app_process(left, right)
    app = find_app()
    doDelay(1)

    def _count_tabs():
        return len(app.findChildren(lambda n: n.roleName == "page tab" and n.showing))

    tabs_before = _count_tabs()

    # First row should be "subdir" directory
    send_keys("Return", proc.pid)
    doDelay(1)

    tabs_after = _count_tabs()
    # Enter on a dir row should NOT open a new tab (just expand/collapse)
    assert tabs_after == tabs_before, \
        f"Expected no new tab from dir row. Before: {tabs_before}, after: {tabs_after}"


def test_dir_collapse_expand_all(app_process):
    """Collapse all / Expand all buttons should work without crash."""
    left = os.path.join(FIXTURES, "left_dir")
    right = os.path.join(FIXTURES, "right_dir")
    proc = app_process(left, right)
    app = find_app()
    doDelay(1)

    # Look for collapse/expand buttons
    collapse_btn = app.findChild(
        lambda n: n.roleName == "push button" and "Collapse" in n.name and n.showing
    )
    expand_btn = app.findChild(
        lambda n: n.roleName == "push button" and "Expand" in n.name and n.showing
    )

    if collapse_btn:
        collapse_btn.do_action(0)
        doDelay(0.5)
    if expand_btn:
        expand_btn.do_action(0)
        doDelay(0.5)

    # Just verify app didn't crash
    frames = app.findChildren(lambda n: n.roleName == "frame")
    assert len(frames) >= 1, "Window disappeared after collapse/expand"
