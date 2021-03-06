.PHONY: deps

REBAR=./rebar

all: deps compile test

modules:
	@cd deps/merl && $(MAKE)

before-compile: modules

compile: before-compile
	@$(REBAR) compile

app: before-compile
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean-modules:
	@cd deps/merl && $(MAKE) clean

clean: clean-modules
	@$(REBAR) clean

distclean: clean
	@$(REBAR) delete-deps

test: deps compile
	- rm -f ./test/records.erl
	- erl -noshell -pa $(PWD)/ebin -pa $(PWD)/deps/*/ebin \
		-eval "jsoncodegen:make([\"test/test_records.hrl\"], [], \"./test/\", \"records\")" \
		-s init stop
	@$(REBAR) eunit skip_deps=true

console:
	exec erl -pa $(PWD)/ebin \
	  -pa $(PWD)/deps/*/ebin
