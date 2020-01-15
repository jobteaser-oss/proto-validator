
prefix = /usr/local

all: build

dialyzer:
	rebar3 dialyzer

build:
	rebar3 escriptize

test:
	rebar3 eunit

cover:
	rebar3 eunit --cover
	rebar3 cover

clean:
	$(RM) -r _build

install: build
	install -D -m755 proto-validator $(prefix)/bin/proto-validator

uninstall:
	$(RM) $(prefix)/bin/proto-validator

.PHONY: all dialyzer build test cover clean install uninstall
