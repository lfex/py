NODENAME=$(shell echo "lsci"|sed -e 's/-//g')
RUN_DIR=./run
LOG_DIR=./log

compile: get-deps clean-ebin proj-compile
	@cd deps/e2 && make

run-dir:
	@mkdir -p $(RUN_DIR)

log-dir:
	@mkdir -p $(LOG_DIR)

run:
	@@ERL_LIBS=$(ERL_LIBS) PATH=$(SCRIPT_PATH) \
	run_erl -daemon ./run/ ./log/ \
	"erl -pa ebin -config ${config-./dev} -s \'lsci\'"

dev:
	@echo "Running OTP app in the foreground ..."
	@ERL_LIBS=$(ERL_LIBS) PATH=$(SCRIPT_PATH) lfe \
	-eval "application:start('lsci')"

dev-named:
	@echo "Running OTP app in the foreground ..."
	@ERL_LIBS=$(ERL_LIBS) PATH=$(SCRIPT_PATH) lfe \
	-sname repl@${HOST} -setcookie `cat ~/.erlang.cookie` \
	-eval "application:start('lsci')"

run-named: dev-named

prod:
	@echo "Running OTP app in the background ..."
	@ERL_LIBS=$(ERL_LIBS) PATH=$(SCRIPT_PATH) lfe \
	-sname ${NODENAME}@${HOST} -setcookie `cat ~/.erlang.cookie` \
	-eval "application:start('lsci')" \
	-noshell -detached

daemon: prod

stop:
	@echo "Stopping OTP app ..."
	@ERL_LIBS=$(ERL_LIBS) PATH=$(SCRIPT_PATH) lfe \
	-sname controller@${HOST} -setcookie `cat ~/.erlang.cookie` \
	-eval "rpc:call('${NODENAME}@${HOST}', init, stop, [])" \
	-noshell -s erlang halt

list-nodes:
	@echo "Getting list of running OTP nodes ..."
	@echo
	@ERL_LIBS=$(ERL_LIBS) PATH=$(SCRIPT_PATH) lfe \
	-sname controller@${HOST} -setcookie `cat ~/.erlang.cookie` \
	-eval 'io:format("~p~n",[element(2,net_adm:names())]).' \
	-noshell -s erlang halt
