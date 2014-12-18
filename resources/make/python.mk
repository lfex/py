VENV=python/.venv
REQS=python/requirements.txt

venv:
	python3 -m venv $(VENV)

get-py-deps:
	. $(VENV)/bin/activate && \
	pip3 install -r $(REQS)

python: venv get-py-deps
