ERL_PORT_LIB=deps/erlport/priv/python3
LIB=python
VENV=$(LIB)/.venv
REQS=$(LIB)/requirements.txt

venv:
	python3 -m venv $(VENV)

get-py-deps:
	. $(VENV)/bin/activate && \
	pip3 install -r $(REQS)

python: venv get-py-deps

interp:
	@. $(VENV)/bin/activate && \
	PYTHONPATH=$(LIB):$(ERL_PORT_LIB) \
	python3
