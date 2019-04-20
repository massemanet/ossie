ossie
==

This is a fork of some Erlang code from [osmocom](http://osmocom.org). It's a fairly complete
SS7/sigtran/SCCP/TCAP/MAP stack.

As far as I can tell, it was originally 2 projects; *osmocom_ss7* (primarily by Harald Welte) &
*signerl* (primarily by Vance Shipley). It's pretty nice looking code, but was
seemingly abandoned in 2010 or thereabouts.

This project is an attempt to bring the code up to 2019 standards; use rebar3,
sensible namespacing (hence the rename to _ossie_, from OSmo/SIgnErl), remove
deprecated functions, make dialyzer clean, etc.

I take no credit for any of the code, except a few bug fixes.

* usage

Clone from git. You'll need the _libpcap_ header files.

Running

```shell
rebar3 clean && rebar3 compile && rebar3 xref && rebar3 eunit && rebar3 dialyzer
```
probaby works.

After that, you're on your own.
