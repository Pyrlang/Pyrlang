PY=python3

.PHONY: test run
run:
	$(PY) test1.py

test:
	$(PY) test/dist_etf_test.py
