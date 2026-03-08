"""Shared fixtures for dogtail UI integration tests."""
import os
import signal
import subprocess
import time

import dogtail.tree
from dogtail.utils import doDelay
import pytest

MERGERS_BIN = os.environ.get(
    "MERGERS_BIN",
    os.path.join(os.path.dirname(__file__), "../../target/release/mergers"),
)
FIXTURES = os.path.join(os.path.dirname(__file__), "../fixtures")


def find_app(name="mergers", retries=10):
    """Find the application in the AT-SPI tree, polling until it appears."""
    for i in range(retries):
        try:
            app = dogtail.tree.root.application(name)
            doDelay(0.5)  # Brief settle time for AT-SPI tree
            return app
        except Exception:
            if i == retries - 1:
                raise
            doDelay(0.5)
    return None


def wait_for_label(app, predicate, timeout=5, interval=0.5):
    """Poll AT-SPI labels until `predicate(text)` returns True or timeout."""
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        labels = app.findChildren(lambda n: n.roleName == "label" and n.showing)
        for label in labels:
            try:
                text = label.text or label.name or ""
                if predicate(text):
                    return text
            except Exception:
                continue
        doDelay(interval)
    return None


def find_labels(app):
    """Return text of all visible labels."""
    labels = app.findChildren(lambda n: n.roleName == "label" and n.showing)
    texts = []
    for lbl in labels:
        try:
            texts.append(lbl.text or lbl.name or "")
        except Exception:
            pass
    return texts


def send_keys_until(app, key_combo, pid, predicate, retries=3, delay=1):
    """Send a key combo and retry if the expected label change doesn't happen."""
    for _ in range(retries):
        _send_keys_impl(key_combo, pid)
        result = wait_for_label(app, predicate, timeout=delay)
        if result is not None:
            return result
    return None


def send_keys(key_combo, pid):
    """Public wrapper for sending keys."""
    _send_keys_impl(key_combo, pid)


def _send_keys_impl(key_combo, pid):
    """Focus the mergers window by PID and send a key combo via xdotool."""
    wids = (
        subprocess.check_output(["xdotool", "search", "--pid", str(pid)])
        .decode()
        .strip()
        .splitlines()
    )
    if not wids:
        return
    for wid in wids:
        result = subprocess.run(
            ["xdotool", "windowfocus", "--sync", wid],
            capture_output=True,
        )
        if result.returncode == 0:
            break
    time.sleep(0.2)  # Let GTK process the focus event
    subprocess.run(["xdotool", "key", key_combo], check=True)


def copy_fixture(name, dest_dir):
    """Copy a fixture file to dest_dir, return the destination path."""
    import shutil
    src = os.path.join(FIXTURES, name)
    dst = os.path.join(dest_dir, name)
    shutil.copy2(src, dst)
    return dst


def focus_and_type(pid, text):
    """Focus the mergers window and type text via xdotool."""
    wids = (
        subprocess.check_output(["xdotool", "search", "--pid", str(pid)])
        .decode().strip().splitlines()
    )
    if not wids:
        raise RuntimeError(f"No X11 windows found for pid {pid}")
    for wid in wids:
        result = subprocess.run(
            ["xdotool", "windowfocus", "--sync", wid],
            capture_output=True,
        )
        if result.returncode == 0:
            break
    time.sleep(0.3)
    subprocess.run(["xdotool", "type", "--delay", "50", text], check=True)


def _launch_and_wait(*args):
    """Launch mergers with args, poll for AT-SPI readiness, return (proc, app)."""
    proc = subprocess.Popen(
        [MERGERS_BIN, *args],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    app = find_app()
    return proc, app


def _kill_proc(proc):
    """Terminate a process gracefully, then force-kill if needed."""
    proc.send_signal(signal.SIGTERM)
    try:
        proc.wait(timeout=3)
    except subprocess.TimeoutExpired:
        proc.kill()
        proc.wait(timeout=2)
    time.sleep(0.5)  # Let AT-SPI deregister


@pytest.fixture
def app_process():
    """Launch mergers per test. Use for tests that need unique state."""
    processes = []

    def _launch(*args):
        proc, _ = _launch_and_wait(*args)
        processes.append(proc)
        return proc

    yield _launch

    for proc in processes:
        _kill_proc(proc)


@pytest.fixture
def fixture_path():
    """Return path to a fixture file."""
    def _path(name):
        return os.path.join(FIXTURES, name)
    return _path


# ---------------------------------------------------------------------------
# Module-scoped shared fixtures for read-only test batching
# ---------------------------------------------------------------------------

@pytest.fixture(scope="module")
def shared_diff_app():
    """One mergers instance for left.txt vs right.txt, shared across a module."""
    left = os.path.join(FIXTURES, "left.txt")
    right = os.path.join(FIXTURES, "right.txt")
    proc, app = _launch_and_wait(left, right)
    yield proc, app
    _kill_proc(proc)


@pytest.fixture(scope="module")
def shared_dir_app():
    """One mergers instance for left_dir vs right_dir, shared across a module."""
    left = os.path.join(FIXTURES, "left_dir")
    right = os.path.join(FIXTURES, "right_dir")
    proc, app = _launch_and_wait(left, right)
    yield proc, app
    _kill_proc(proc)


@pytest.fixture(scope="module")
def shared_merge_app():
    """One mergers instance for 3-way merge, shared across a module."""
    left = os.path.join(FIXTURES, "left.txt")
    base = os.path.join(FIXTURES, "base.txt")
    right = os.path.join(FIXTURES, "right.txt")
    proc, app = _launch_and_wait(left, base, right)
    yield proc, app
    _kill_proc(proc)
