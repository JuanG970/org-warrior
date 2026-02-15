.PHONY: test test-integration lint install

test:
	python3 -m pytest tests/ -v

test-integration:
	./tests/test_org_warrior.sh

lint:
	ruff check src/org-warrior tests/


build:
	poetry build

install: build
	pipx install dist/*.whl --force
