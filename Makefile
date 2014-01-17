R = ./rebar

.PHONY: all
all: compile

.PHONY: clean
clean:
	$(R) clean

# Deps
.PHONY: get-deps
get-deps:
	$(R) get-deps

.PHONY: update-deps
update-deps:
	$(R) update-deps

.PHONY: deps
deps: get-deps update-deps

.PHONY: compile
compile:
	$(R) compile

.PHONY: eunit
eunit:
	$(R) eunit -v skip_deps=true

# Dialyzer

DIAAPPS = \
	asn1 \
	compiler \
	crypto \
	erts \
	inets \
	kernel \
	mnesia \
	sasl \
	ssl \
	stdlib \
	syntax_tools \
	tools \
	webtool \

STD_DIA="${HOME}/.dialyzer_plt"


.PHONY: build_plt
build_plt: erl_std.plt

erl_std.plt:
	dialyzer --build_plt  --apps $(DIAAPPS) --output_plt erl_std.plt

.PHONY: dialyzer
dialyzer: compile build_plt
	dialyzer ebin --plts erl_std.plt

.PHONY: edialyzer
edialyzer: eunit build_plt
	dialyzer .eunit --plts erl_std.plt
