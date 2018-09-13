LIBNAME=native_codec_impl
ROOT=$(shell pwd)
PY=PYTHONPATH=$(ROOT):$(ROOT)/target/debug python3

.PHONY: compile
compile:
	cargo build && \
	ln -fs $(ROOT)/target/debug/lib$(LIBNAME).so $(ROOT)/target/debug/$(LIBNAME).so
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
