ROOT=$(shell pwd)
PY=PYTHONPATH=$(ROOT):$(ROOT)/target/debug python3

.PHONY: compile
compile:
	cargo build && \
	ln -fs $(ROOT)/target/debug/libterm_codec.so $(ROOT)/target/debug/term_codec.so
# && cargo build --release

.PHONY: test
test: compile
	for f in $(shell ls test/*_test.py); do \
		echo "RUNNING $$f"; \
		$(PY) $$f; \
	done

.PHONY: dtest
dtest: compile
	PYTHONPATH=$(ROOT):$(ROOT)/target/debug gdb --args python3 test/rust_extension_test.py
