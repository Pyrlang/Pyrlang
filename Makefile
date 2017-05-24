PY=python3

.PHONY: test run
run:
	$(PY) test1.py

test:
	$(PY) test/dist_etf_decode_test.py && \
	$(PY) test/dist_etf_transitive_test.py
