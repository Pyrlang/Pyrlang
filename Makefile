PY=python3

.PHONY: test example1
example1:
	$(PY) examples/simple_node.py

test:
	$(PY) test/dist_etf_decode_test.py && \
	$(PY) test/dist_etf_transitive_test.py
