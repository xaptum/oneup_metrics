BASEDIR = $(shell pwd)
REBAR = rebar3
APPNAME = oneup_metrics
RELPATH = _build/default/rel/$(APPNAME)
DEBUG=1

compile:
	$(REBAR) compile

release:
	$(REBAR) release

recompile:
	find . -name ebin | xargs rm -rf
	$(REBAR) compile

eunit:
	$(REBAR) eunit --module= oneup_metrics_test

common-test:
	ct_run -dir $(BASEDIR)/ct -logdir $(BASEDIR)/ct/logs \
	-pa $(BASEDIR)/_build/default/lib/*/ebin -erl_args \
	-config $(BASEDIR)/templates/sys.config

test: recompile eunit common-test

console: release
	cd $(RELPATH) && ./bin/$(APPNAME) console

clean:
	$(REBAR) clean
	rm -rf _build
