"""Shared fixtures for dogtail UI integration tests."""
import os
import signal
import subprocess
import time

import pytest

MERGERS_BIN = os.environ.get(
    "MERGERS_BIN",
    os.path.join(os.path.dirname(__file__), "../../target/release/mergers"),
)
FIXTURES = os.path.join(os.path.dirname(__file__), "../fixtures")


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
        time.sleep(2)  # Wait for GTK to initialize
        return proc

    yield _launch

    for proc in processes:
        proc.send_signal(signal.SIGTERM)
        try:
            proc.wait(timeout=5)
        except subprocess.TimeoutExpired:
            proc.kill()


@pytest.fixture
def fixture_path():
    """Return path to a fixture file."""
    def _path(name):
        return os.path.join(FIXTURES, name)
    return _path
