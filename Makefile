all: deps compile

deps: deps/yaws

deps/yaws:
	@rebar get-deps

compile:
	@rebar compile

dialyze: all dial_ety dial_dig dial_dlv dial_dnd

dial_ety:
	dialyzer -Wrace_conditions etdd_yaws/ebin

dial_dig:
	dialyzer -Wrace_conditions etdd_dig/ebin

dial_dlv:
	dialyzer -Wrace_conditions etdd_dlv/ebin

dial_dnd:
	dialyzer -Wrace_conditions dig_and_delve/ebin

gc:
	@echo 'Removing all emacs backup files'
	@rm -f *~
	@rm -f */*~
	@rm -f */*/*~
	@rm -f */*/*/*~

rel: all
	@echo 'Generating dig_and_delve release'
	@(cd rel; rebar generate)

clean: gc
	@rebar clean

relclean:
	@rm -f rel/erl_crash.dump
	@rm -rf rel/dig_and_delve

realclean: clean relclean
	@rebar del-deps
	@rm -rf deps/*


eunit: all
	ERL_LIBS=$(CURDIR):$(CURDIR)/deps rebar skip_deps=true eunit
