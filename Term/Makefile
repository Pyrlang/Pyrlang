ROOT=$(shell pwd)
PY=PYTHONPATH=$(ROOT):$(ROOT)/target/debug python3

.PHONY: compile
compile:
	cargo build 
# && cargo build --release

.PHONY: test
test:
	for f in $(shell ls test/*_test.py); do \
		echo "RUNNING $$f"; \
		$(PY) $$f; \
	done
