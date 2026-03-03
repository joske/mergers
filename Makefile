.PHONY: test test-ui test-release fmt clippy check

VENV := tests/ui_integration/.venv
PYTEST := $(VENV)/bin/python3 -m pytest

test:
	cargo test

test-ui:
	xvfb-run -a cargo test

$(VENV): tests/ui_integration/requirements.txt
	python3 -m venv --system-site-packages $(VENV)
	$(VENV)/bin/pip install -r tests/ui_integration/requirements.txt
	@touch $(VENV)

test-release: $(VENV)
	xvfb-run -a cargo test
	xvfb-run -a $(PYTEST) tests/ui_integration/ -v

fmt:
	cargo +nightly fmt

clippy:
	cargo clippy -- -W clippy::pedantic

check: fmt clippy test
