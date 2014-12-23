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

PROJECT = py
LIB = $(PROJECT)
DEPS = ./deps
BIN_DIR = ./bin
EXPM = $(BIN_DIR)/expm
SOURCE_DIR = ./src
OUT_DIR = ./ebin
TEST_DIR = ./test
TEST_OUT_DIR = ./.eunit
SCRIPT_PATH=$(DEPS)/lfe/bin:.:./bin:"$(PATH)":/usr/local/bin
ifeq ($(shell which lfetool),)
LFETOOL=$(BIN_DIR)/lfetool
else
LFETOOL=lfetool
endif
ERL_LIBS=..:../$(PROJECT):$(shell $(LFETOOL) info erllibs)

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

get-lfetool: $(BIN_DIR)
	curl -L -o ./lfetool https://raw.github.com/lfe/lfetool/stable/lfetool && \
	chmod 755 ./lfetool && \
	mv ./lfetool $(BIN_DIR)

get-version:
	@PATH=$(SCRIPT_PATH) $(LFETOOL) info version
	@echo "Erlang/OTP, LFE, & library versions:"
	@ERL_LIBS=$(ERL_LIBS) PATH=$(SCRIPT_PATH) erl \
	-eval "lfe_io:format(\"~p~n\",['$(PROJECT)-util':'get-versions'()])." \
	-noshell -s erlang halt

$(EXPM): $(BIN_DIR)
	@[ -f $(EXPM) ] || \
	PATH=$(SCRIPT_PATH) lfetool install expm $(BIN_DIR)

get-deps:
	@echo "Getting dependencies ..."
	@which rebar.cmd >/dev/null 2>&1 && rebar.cmd get-deps || rebar get-deps
	@PATH=$(SCRIPT_PATH) $(LFETOOL) update deps

clean-ebin:
	@echo "Cleaning ebin dir ..."
	@rm -f $(OUT_DIR)/*.beam

clean-eunit:
	-@PATH=$(SCRIPT_PATH) $(LFETOOL) tests clean

proj-compile: get-deps clean-ebin
	@echo "Compiling project code and dependencies ..."
	@cp src/$(PROJECT).app.src $(OUT_DIR)/$(PROJECT).app
	@which rebar.cmd >/dev/null 2>&1 && \
	PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) rebar.cmd compile || \
	PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) rebar compile

proj-compile-no-deps: clean-ebin
	@echo "Compiling only project code ..."
	@which rebar.cmd >/dev/null 2>&1 && \
	PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) \
	rebar.cmd compile skip_deps=true || \
	PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) rebar compile skip_deps=true

compile-tests: clean-eunit
	@PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) $(LFETOOL) tests build

repl: proj-compile
	@which clear >/dev/null 2>&1 && clear || printf "\033c"
	@echo "Starting an LFE REPL ..."
	@PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) $(LFETOOL) repl lfe -s $(PROJECT)

repl-no-deps: proj-compile-no-deps
	@which clear >/dev/null 2>&1 && clear || printf "\033c"
	@echo "Starting an LFE REPL ..."
	@PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) $(LFETOOL) repl lfe -s $(PROJECT)

shell: proj-compile
	@which clear >/dev/null 2>&1 && clear || printf "\033c"
	@echo "Starting an Erlang shell ..."
	@PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) erl -s $(PROJECT)

shell-no-deps: proj-compile-no-deps
	@which clear >/dev/null 2>&1 && clear || printf "\033c"
	@echo "Starting an Erlang shell ..."
	@PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) erl -s $(PROJECT)

clean: clean-ebin clean-eunit
	@which rebar.cmd >/dev/null 2>&1 && rebar.cmd clean || rebar clean

check-unit-only:
	@PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) $(LFETOOL) tests unit

check-integration-only:
	@PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) $(LFETOOL) tests integration

check-system-only:
	@PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) $(LFETOOL) tests system

check-unit-with-deps: get-deps proj-compile proj-compile-tests check-unit-only
check-unit: clean-eunit proj-compile-no-deps check-unit-only
check-integration: clean-eunit proj-compile check-integration-only
check-system: clean-eunit proj-compile check-system-only
check-all-with-deps: clean-eunit proj-compile check-unit-only \
	check-integration-only check-system-only clean-eunit
check-all: get-deps clean-eunit proj-compile-no-deps
	@PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) $(LFETOOL) tests all

check: check-unit-with-deps

check-travis: $(LFETOOL) check
