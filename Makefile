DIALYZER_PLT ?= $(HOME)/.dialyzer_plt

.PHONY: all
all: get-deps compile

.PHONY: get-deps
get-deps:
	@./rebar get-deps

.PHONY: compile
compile:
	@./rebar compile

.PHONY: docs
docs:
	@./rebar doc skip_deps=true

.PHONY: check
check: xref dialyzer

.PHONY: xref
xref:
	@./rebar xref skip_deps=true

dialyzer.plt: compile
	@test -f dialyzer.plt -a "$(shell find deps -wholename */ebin/*.beam -newer $@ 2>/dev/null)" = "" || \
	   dialyzer --add_to_plt \
                    --no_check_plt \
                    --plt ${DIALYZER_PLT} \
                    --output_plt $@ \
                    --apps erts inets kernel stdlib \
                    -r deps


.PHONY: dialyzer
dialyzer: compile dialyzer.plt
	@dialyzer --no_check_plt --verbose --plt dialyzer.plt -r ebin

.PHONY: test
test: eunit ct

.PHONY: eunit
eunit:
	@./rebar eunit skip_deps=true

.PHONY: ct
ct:
	@./rebar ct -v skip_deps=true

.PHONY: clean
clean:
	@./rebar clean
	$(RM) doc/*

# eof
