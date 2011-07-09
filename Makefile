all: deps compile

deps:
	@rebar get-deps

compile:
	@rebar compile

dialyze: dial_ety

dial_ety:
	dialyzer -Wrace_conditions etdd_yaws/ebin

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
	@rm -rf rel/etdd

realclean: clean relclean
	@rebar del-deps
	@rm -rf deps/*


eunit: all
	ERL_LIBS=$(CURDIR):$(CURDIR)/deps rebar skip_deps=true eunit
