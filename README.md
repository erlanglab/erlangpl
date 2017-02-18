## Getting started

The Erlang Performance Lab tool (erlangpl for short) can be started using escript or as a regular Erlang release. 

```
$ git clone git@bitbucket.org:erlanglab/erlangpl.git
$ cd erlangpl
$ make rebar
$ make
$ ./bootstrap
```

## Running erlangpl script

The erlangpl shell script is a self-contained escript, which can be started from a command line as long as you have Erlang/OTP installed.

```
$ ./erlangpl -h

Usage: erlangpl [-n <node>] [-c <cookie>] [-h] [-v <verbose>] [-V]
                [-s <sname>] [-l <name>]

$ ./erlangpl -n testnode@127.0.0.1 -c YOURCOOKIE
```

Once started, try visiting http://localhost:8000/

## Running development release

You can also start the tool as a regular Erlang release and connect to its console to debug the tool itself.

```
$ make
$ rebar -f generate
$ ./rel/erlangpl/bin/erlangpl console node=testnode@127.0.0.1 cookie=YOURCOOKIE
```

Have fun!