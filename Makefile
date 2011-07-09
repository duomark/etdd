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

rel: all
	@echo 'Generating etdd release'
	@(cd rel; rebar generate)

clean:
	@rebar clean

relclean:
	@rm -rf rel/etdd

realclean: clean relclean
	@rebar del-deps
	@rm -rf deps/*


eunit: all
	ERL_LIBS=$(CURDIR):$(CURDIR)/deps rebar skip_deps=true eunit
