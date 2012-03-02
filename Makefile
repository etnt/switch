ERL ?= erl
APP := switch

.PHONY: deps test

all: deps
	@./rebar compile

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

test:
	@./rebar eunit skip_apps=mochiweb,webmachine

local_clean:
	@rm ./ebin/*

xref: all
	@./rebar xref skip_apps=mochiweb,webmachine

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'
