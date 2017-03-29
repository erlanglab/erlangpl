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
$ ./erlangpl --node foo@127.0.0.1
```

### Mnesia cluster
You can generate messages between nodes by querying a distributed database Mnesia.

To setup a Mnesia cluster, start several Erlang nodes with unique names e.g. `a@`, `b@`, `c@`, etc. and start the database on all of them:
```
$ erl -name a@127.0.0.1 -setcookie mnesia_cluster -run mnesia start \
 -detached -noshell -noinput
$ erl -name b@127.0.0.1 -setcookie mnesia_cluster -run mnesia start \
 -detached -noshell -noinput
$ erl -name c@127.0.0.1 -setcookie mnesia_cluster -run mnesia start \
 -detached -noshell -noinput
$ erl -name d@127.0.0.1 -setcookie mnesia_cluster -run mnesia start \
-detached -noshell -noinput
```
Then create a `test_table` and configure it to be replicated on all nodes:
```
erl -name maint@127.0.0.1 -setcookie mnesia_cluster -remsh a@127.0.0.1
(a@127.0.0.1)2> mnesia:change_config(extra_db_nodes, ['b@127.0.0.1']).
(a@127.0.0.1)3> mnesia:change_config(extra_db_nodes, ['c@127.0.0.1']).
(a@127.0.0.1)4> mnesia:change_config(extra_db_nodes, ['d@127.0.0.1']).
(a@127.0.0.1)5> mnesia:create_table(test_table, []).
(a@127.0.0.1)6> [mnesia:add_table_copy(test_table, Node, ram_copies) || Node <- nodes()].
```

Start the Erlang performance lab tool:
```
$ $ ./erlangpl --node a@127.0.0.1 --cookie mnesia_cluster
```

Here's some mnesia activities to add some information the performance lab dashboard:
```
[begin mnesia:transaction(fun() -> mnesia:write({test_table, Key, "value"}) end), timer:sleep(10) end || Key <- lists:seq(1,2000)].
[begin mnesia:sync_dirty(fun() -> mnesia:write({test_table, Key, "value"}) end), timer:sleep(10) end || Key <- lists:seq(1,2000)].
[begin mnesia:dirty_write({test_table, Key, "value"}), timer:sleep(10) end || Key <- lists:seq(1,2000)].
```

Stopping the demo mnesia cluster:
```
ps aux | grep "[a|b|c|d]@127.0.0.1" | awk '{ print $2 }' | xargs kill
```

Videos from those experiments were posted on [YouTube](https://www.youtube.com/channel/UCGkcbu799cC1rtMaQtAajpg)


## Running development release

You can also start the tool as a regular Erlang release and connect to its console to debug the tool itself.

```
$ make
$ rebar -f generate
$ ./rel/erlangpl/bin/erlangpl console node=testnode@127.0.0.1 cookie=YOURCOOKIE
```

Have fun!
