PY=python3

.PHONY: test example1
example1:
	$(PY) examples/01_simple_node.py

.PHONY: test example2
example2:
	$(PY) examples/02_registered_process.py

.PHONY: test
test:
	$(PY) test/dist_etf_decode_test.py && \
	$(PY) test/dist_etf_transitive_test.py
