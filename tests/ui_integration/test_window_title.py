"""Tests that the window title stays 'Mergers' regardless of pane swaps."""
from dogtail.utils import doDelay

from conftest import find_app


def _open_diff(app_process, fixture_path):
    """Launch mergers with left/right fixtures and return (app, proc)."""
    proc = app_process(fixture_path("left.txt"), fixture_path("right.txt"))
    app = find_app()
    return app, proc


def _get_window_title(app):
    """Return the title (name) of the first frame in the app."""
    frames = app.findChildren(lambda n: n.roleName == "frame")
    assert frames, "No window frame found"
    return frames[0].name


def test_window_title_is_mergers(app_process, fixture_path):
    """Window title should be 'Mergers' on open."""
    app, _ = _open_diff(app_process, fixture_path)
    assert _get_window_title(app) == "Mergers"


def test_window_title_unchanged_after_swap(app_process, fixture_path):
    """Window title should remain 'Mergers' after clicking the swap button."""
    app, _ = _open_diff(app_process, fixture_path)

    assert _get_window_title(app) == "Mergers"

    swap_btn = app.findChild(
        lambda n: n.roleName == "push button" and n.name == "Swap panes"
    )
    assert swap_btn is not None, "Swap panes button not found"
    swap_btn.do_action(0)
    doDelay(1)

    assert _get_window_title(app) == "Mergers", (
        f"Window title changed to '{_get_window_title(app)}' after swap"
    )
