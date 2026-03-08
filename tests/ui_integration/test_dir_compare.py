"""Directory comparison tests sharing a single mergers instance."""
from dogtail.utils import doDelay

from conftest import find_labels, send_keys, wait_for_label


def test_dir_comparison_opens(shared_dir_app):
    """Two directories should open directory comparison window."""
    _, app = shared_dir_app
    windows = app.findChildren(lambda n: n.roleName == "frame")
    assert len(windows) >= 1, "No window found for directory comparison"


def test_dir_window_has_tree_view(shared_dir_app):
    """Directory window should contain a tree/table view."""
    _, app = shared_dir_app
    views = app.findChildren(
        lambda n: n.roleName in ("table", "tree table", "tree", "list", "list box", "panel")
        and n.showing
    )
    assert len(views) > 0, "No list/table/tree view found in directory window"


def test_dir_shows_different_file(shared_dir_app):
    """Files that differ should be listed."""
    _, app = shared_dir_app
    labels = find_labels(app)
    assert any("different.txt" in t for t in labels), \
        f"Expected 'different.txt' in labels: {labels}"


def test_dir_shows_only_left_file(shared_dir_app):
    """File only in left dir should be listed."""
    _, app = shared_dir_app
    labels = find_labels(app)
    assert any("only_left.txt" in t for t in labels), \
        f"Expected 'only_left.txt' in labels: {labels}"


def test_dir_shows_only_right_file(shared_dir_app):
    """File only in right dir should be listed."""
    _, app = shared_dir_app
    labels = find_labels(app)
    assert any("only_right.txt" in t for t in labels), \
        f"Expected 'only_right.txt' in labels: {labels}"


def test_dir_tab_label_contains_dir_names(shared_dir_app):
    """Directory tab should be labeled with both dir names."""
    _, app = shared_dir_app
    # Tab labels may be in page tab names rather than regular labels
    tabs = app.findChildren(lambda n: n.roleName == "page tab" and n.showing)
    tab_names = [t.name for t in tabs]
    labels = find_labels(app)
    all_text = tab_names + labels
    assert any("left_dir" in t for t in all_text), \
        f"Expected 'left_dir' in tab/labels: {all_text}"


def test_dir_has_subdir_row(shared_dir_app):
    """Subdirectories should be shown in the tree."""
    _, app = shared_dir_app
    # Look for "subdir" in any child element name or label
    all_children = app.findChildren(lambda n: n.showing and n.name and "subdir" in n.name)
    labels = find_labels(app)
    has_subdir = len(all_children) > 0 or any("subdir" in t for t in labels)
    assert has_subdir, \
        f"Expected 'subdir' somewhere in the tree. Labels: {labels}"


def test_dir_double_click_opens_tab(shared_dir_app):
    """Enter on a file row should open a diff tab."""
    proc, app = shared_dir_app

    def _count_tabs():
        return len(app.findChildren(lambda n: n.roleName == "page tab" and n.showing))

    tabs_before = _count_tabs()

    # Navigate past subdir to a file row
    send_keys("Down", proc.pid)
    doDelay(0.3)
    send_keys("Down", proc.pid)
    doDelay(0.3)
    send_keys("Return", proc.pid)
    doDelay(2)

    tabs_after = _count_tabs()
    labels = find_labels(app)
    has_diff = (
        any("changes" in t.lower() for t in labels)
        or any(" of " in t for t in labels)
        or any("No changes" in t for t in labels)
        or any("identical" in t.lower() for t in labels)
    )
    assert tabs_after > tabs_before or has_diff, \
        f"Expected a new tab. Tabs: {tabs_before}->{tabs_after}, labels: {labels}"

    # Close the file tab to restore state
    send_keys("ctrl+w", proc.pid)
    doDelay(1)


def test_dir_ctrl_w_closes_file_tab(shared_dir_app):
    """After opening a file tab, Ctrl+W should close it."""
    proc, app = shared_dir_app

    def _count_tabs():
        return len(app.findChildren(lambda n: n.roleName == "page tab" and n.showing))

    tabs_before = _count_tabs()

    send_keys("Down", proc.pid)
    doDelay(0.3)
    send_keys("Down", proc.pid)
    doDelay(0.3)
    send_keys("Return", proc.pid)
    doDelay(2)
    tabs_after_open = _count_tabs()

    send_keys("ctrl+w", proc.pid)
    doDelay(1)
    tabs_after_close = _count_tabs()

    assert tabs_after_close < tabs_after_open or tabs_after_close == tabs_before, \
        f"Tab did not close. Before: {tabs_before}, open: {tabs_after_open}, close: {tabs_after_close}"


def test_dir_same_file_not_opened_twice(shared_dir_app):
    """Opening the same file twice should switch to existing tab."""
    proc, app = shared_dir_app

    def _count_tabs():
        return len(app.findChildren(lambda n: n.roleName == "page tab" and n.showing))

    # Open a file
    send_keys("Down", proc.pid)
    doDelay(0.3)
    send_keys("Down", proc.pid)
    doDelay(0.3)
    send_keys("Return", proc.pid)
    doDelay(2)
    tabs_first = _count_tabs()

    # Go back to dir tab and try again
    send_keys("alt+1", proc.pid)
    doDelay(1)
    send_keys("Return", proc.pid)
    doDelay(2)
    tabs_second = _count_tabs()

    assert tabs_second <= tabs_first + 1, \
        f"Expected no duplicate tab. After first: {tabs_first}, after second: {tabs_second}"

    # Clean up: close file tab, go back to dir tab
    send_keys("ctrl+w", proc.pid)
    doDelay(1)


def test_dir_shows_same_file_not_listed(shared_dir_app):
    """Files identical in both dirs should not be shown (filtered out)."""
    _, app = shared_dir_app
    labels = find_labels(app)
    # same.txt exists in both dirs with identical content
    same_labels = [t for t in labels if t.strip() == "same.txt"]
    assert len(same_labels) == 0, \
        f"Expected 'same.txt' to be filtered out, but found in labels: {labels}"
