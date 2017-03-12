## Getting started

The Erlang Performance Lab tool (erlangpl for short) can be started using escript or as a regular Erlang release.

For building UI you have to have [node](https://nodejs.org/en/), [yarn](https://yarnpkg.com/lang/en/) and [elm-lang](https://guide.elm-lang.org/install.html) installed.
Be aware that building UI can take some time. It takes around 1 minute on stock MacBook 2015 plus dependencies download for the first time. Second time dependencies will be cached. 

```
$ git clone --recursive git@github.com:erlanglab/erlangpl.git
$ cd erlangpl
$ make rebar
$ make build-ui
$ make
$ ./bootstrap
```

## Running erlangpl script

The erlangpl shell script is a self-contained escript, which can be started from a command line as long as you have Erlang/OTP installed.

```
$ ./erlangpl -h

Usage: erlangpl [-n <node>] [-c <cookie>] [-p <plugin>] [-h]
                [-v <verbose>] [-P <port>] [-V] [-s <sname>] [-l <name>]

  -n, --node     Monitored node name
  -c, --cookie   Overwrite ~/.erlang.cookie
  -p, --plugin   Path to plugins
  -h, --help     Show the program options
  -v, --verbose  Verbosity level (-v, -vv, -vvv)
  -P, --port     HTTP and WS port number
  -V, --version  Show version information
  -s, --sname    Start with a shortname
  -l, --name     Start with a longname, default erlangpl@127.0.0.1

$ ./erlangpl -n testnode@127.0.0.1 -c YOURCOOKIE
```

Once started, try visiting http://localhost:8000/

## Examples

### Connecting to an Elixir iex session

```
$ iex --name foo@127.0.0.1 -S mix
```

```
$ erlangpl --node foo@127.0.0.1
```

## Running development release

You can also start the tool as a regular Erlang release and connect to its console to debug the tool itself.

```
$ make
$ rebar -f generate
$ ./rel/erlangpl/bin/erlangpl console node=testnode@127.0.0.1 cookie=YOURCOOKIE
```

Have fun!
