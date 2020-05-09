## shallow clone for speed

# REBAR_GIT_CLONE_OPTIONS += --depth 1
# export REBAR_GIT_CLONE_OPTIONS

REBAR = rebar3
all: compile

CUTTLEFISH_SCRIPT = _build/default/lib/cuttlefish/cuttlefish

app.config: $(CUTTLEFISH_SCRIPT)
	$(verbose) $(CUTTLEFISH_SCRIPT) -l info -e etc/ -c etc/emqx_bridge_devnull.conf -i priv/emqx_bridge_devnull.schema -d data

$(CUTTLEFISH_SCRIPT):
	@$(REBAR) get-deps
	@if [ ! -f cuttlefish ]; then make -C _build/default/lib/cuttlefish; fi

clean: distclean

compile: unlock
	$(REBAR) compile

ct: compile
	$(REBAR) as test ct -v

eunit: compile
	$(REBAR) as test eunit

xref:
	$(REBAR) xref

cover:
	$(REBAR) cover

unlock:
	$(REBAR) unlock

distclean::
	@rm -rf _build cover deps logs log data
	@rm -f rebar.lock compile_commands.json cuttlefish
