ERL_PORT_LIB=deps/erlport/priv/python3
LIB=python
VENV=$(LIB)/.venv
REQS=$(LIB)/requirements.txt
GET_PIP=$(LIB)/get-pip.py

venv:
	python3 -m venv --without-pip $(VENV)

$(GET_PIP):
	wget -O $(GET_PIP) https://bootstrap.pypa.io/get-pip.py
	. $(VENV)/bin/activate && \
	python3 $(GET_PIP)

get-py-deps: $(GET_PIP)
	. $(VENV)/bin/activate && \
	pip install -r $(REQS)

python: venv get-py-deps

interp:
	@. $(VENV)/bin/activate && \
	PYTHONPATH=$(LIB):$(ERL_PORT_LIB) \
	python3
