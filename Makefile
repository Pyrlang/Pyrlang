PY=python3

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

.PHONY: test
test:
	$(PY) test/dist_etf_decode_test.py && \
	$(PY) test/dist_etf_transitive_test.py
