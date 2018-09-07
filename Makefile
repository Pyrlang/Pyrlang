PY=PYRLANG_ENABLE_LOG_FORMAT=1 PYRLANG_LOG_LEVEL=DEBUG python3

.PHONY: example1
example1:
	$(PY) examples/01_simple_node.py

.PHONY: example2
example2:
	$(PY) examples/02_registered_process.py

# Run `make example10a` to run Python node
# Run `make example10b` to run Elixir client
.PHONY: example10a example10b
example10a:
	$(PY) examples/elixir/10_test.py
example10b:
	elixir --name elixir@127.0.0.1 --cookie COOKIE \
		examples/elixir/test10.exs

.PHONY: deps
deps:
	pip3 install -r requirements.txt

.PHONY: docs
docs:
	cd docs && $(MAKE) html

.PHONY: test
test:
	for f in $(shell ls test/*_test.py); do \
		echo "RUNNING $$f"; \
		$(PY) $$f; \
	done
