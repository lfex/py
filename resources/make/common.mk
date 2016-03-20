ifeq ($(shell which erl),)
$(error Can't find Erlang executable 'erl')
exit 1
endif
OS := $(shell uname -s)
ifeq ($(OS),Linux)
HOST=$(HOSTNAME)
endif
ifeq ($(OS),Darwin)
HOST = $(shell scutil --get ComputerName)
endif

LIB = $(PROJECT)
BIN_DIR = ./bin
SOURCE_DIR = ./src
OUT_DIR = ./ebin
LFE=_build/default/lib/lfe/bin/lfe

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

get-version:
	@echo "Get LFE py version:"
	@$(LFE) \
	-eval '(lfe_io:format "~p~n" (list (py-util:get-py-version)))' -noshell -s erlang halt

get-versions:
	@echo "Erlang/OTP, LFE, & library versions:"
	@$(LFE) \
	-eval '(progn (py:start) (lfe_io:format "~p~n" (list (py-util:get-versions))))' -noshell -s erlang halt

clean-ebin:
	@echo "Cleaning ebin dir ..."
	@rm -f $(OUT_DIR)/*.beam

proj-compile: clean-ebin
	@echo "Compiling project code and dependencies ..."
	@rebar3 compile

compile-tests: clean-eunit
	@PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) $(LFETOOL) tests build

repl: proj-compile
	@which clear >/dev/null 2>&1 && clear || printf "\033c"
	@echo "Starting an LFE REPL ..."
	@$(LFE) -s $(PROJECT)

clean: clean-ebin
	@echo "Cleaning project build dir ..."
	@rm -rf ebin/* _build/default/lib/py/ebin/*

check:
	@rebar3 as tests eunit

check-travis: check
