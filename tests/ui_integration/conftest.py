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


def find_app(name="mergers", retries=5):
    """Find the application in the AT-SPI tree, retrying on failure."""
    for i in range(retries):
        try:
            app = dogtail.tree.root.application(name)
            doDelay(1)  # Let the AT-SPI tree populate
            return app
        except Exception:
            if i == retries - 1:
                raise
            doDelay(1)
    return None


@pytest.fixture
def app_process():
    """Launch mergers and yield the process. Kill on teardown."""
    processes = []

    def _launch(*args):
        proc = subprocess.Popen(
            [MERGERS_BIN, *args],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        processes.append(proc)
        time.sleep(3)  # Wait for GTK to initialize
        return proc

    yield _launch

    for proc in processes:
        proc.send_signal(signal.SIGTERM)
        try:
            proc.wait(timeout=3)
        except subprocess.TimeoutExpired:
            proc.kill()
            proc.wait(timeout=2)
    # Give AT-SPI time to deregister the app before the next test
    time.sleep(1)


@pytest.fixture
def fixture_path():
    """Return path to a fixture file."""
    def _path(name):
        return os.path.join(FIXTURES, name)
    return _path
