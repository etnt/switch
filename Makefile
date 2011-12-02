ERL ?= erl
APP := switch

.PHONY: deps

all: deps
	@./rebar compile

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

test: local_clean
	@./rebar eunit skip_app=mochiweb,webmachine

local_clean:
	@rm ./ebin/*

xref: all
	@./rebar xref skip_app=mochiweb,webmachine

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'
