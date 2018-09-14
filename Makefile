ROOT=$(shell pwd)
PYPATH_SET=PYTHONPATH=$(ROOT):$(ROOT)/Term
PY=$(PYPATH_SET) PYRLANG_ENABLE_LOG_FORMAT=1 PYRLANG_LOG_LEVEL=DEBUG python3
ERLLIBDIR=$(ROOT)/ErlangLib
ERL=erl -pa $(ERLLIBDIR) $(ROOT)/examples -name erl@127.0.0.1 -setcookie COOKIE

.PHONY: example1
example1:
	$(PY) examples/e01_simple_node.py

# `make pynode` is same as example2, will run an idle Python node as py@127.0.0.1
.PHONY: example2 pynode
pynode: example2
example2:
	$(PY) examples/e02_registered_process.py

.PHONY: example3
example3: $(ERLLIBDIR)/py.beam $(ROOT)/examples/e03_call_python.beam
	$(ERL) -s e03_call_python -noshell

.PHONY: example4
example4: $(ERLLIBDIR)/py.beam $(ROOT)/examples/e04_batch_call_python.beam
	$(ERL) -s e04_batch_call_python -noshell

$(ROOT)/examples/%.beam: $(ROOT)/examples/%.erl
	cd $(ROOT)/examples && erlc $<

$(ERLLIBDIR)/%.beam: $(ERLLIBDIR)/%.erl
	cd $(ERLLIBDIR) && erlc $<

# Run `make example10a` to run Python node
# Run `make example10b` to run Elixir client
.PHONY: example10a example10b
example10a:
	$(PY) examples/elixir/e10_test.py
example10b:
	elixir --name elixir@127.0.0.1 --cookie COOKIE \
		examples/elixir/test10.exs

.PHONY: erlshell
erlshell:
	$(ERL)

.PHONY: deps
deps:
	pip3 install -r requirements.txt

.PHONY: docs
docs:
	cd docs && $(PYPATH_SET) $(MAKE) html

.PHONY: test
test:
	for f in $(shell ls test/*_test.py); do \
		echo "RUNNING $$f"; \
		$(PY) $$f || exit 1; \
	done
