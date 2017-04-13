# Erlang Performance Lab [![Build Status](https://travis-ci.org/erlanglab/erlangpl.svg?branch=master)](https://travis-ci.org/erlanglab/erlangpl)

> Tool for developers working with systems running on the Erlang VM (BEAM). It helps with performance analysis.

## Getting started

The Erlang Performance Lab tool (erlangpl for short) can be started using escript or as a regular Erlang release.

### Download prebuilt script

The easiest way to get started is to download a prebuilt `erlangpl` script
([download link](https://github.com/erlanglab/erlangpl/releases/download/0.6.1/erlangpl.tar.gz)).

### Build it manually

#### Prerequisites

For building UI you need to have following dependencies installed:
* [node](https://nodejs.org/en/)
* [yarn](https://yarnpkg.com/lang/en/)
* [elm-lang](https://guide.elm-lang.org/install.html)

Be aware that building UI can take some time. It takes around 1 minute on stock MacBook 2015 plus dependencies
download for the first time. Second time dependencies will be cached.

```
$ git clone https://github.com/erlanglab/erlangpl.git
$ cd erlangpl
$ make rebar
$ make ui
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

Once started, try visiting http://localhost:37575/

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
erl -name a@127.0.0.1
(a@127.0.0.1)1> mnesia:start().
```
Then create a `test_table` and configure it to be replicated on all nodes:
```
(a@127.0.0.1)2> mnesia:change_config(extra_db_nodes, ['b@127.0.0.1']).
(a@127.0.0.1)3> mnesia:change_config(extra_db_nodes, ['c@127.0.0.1']).
(a@127.0.0.1)4> mnesia:change_config(extra_db_nodes, ['d@127.0.0.1']).
(a@127.0.0.1)5> mnesia:create_table(test_table, []).
(a@127.0.0.1)6> [mnesia:add_table_copy(test_table, Node, ram_copies) || Node <- nodes()].
```

Here are some behaviours you can test:
```
[begin mnesia:transaction(fun() -> mnesia:write({test_table, Key, "value"}) end), timer:sleep(10) end || Key <- lists:seq(1,2000)].
[begin mnesia:sync_dirty(fun() -> mnesia:write({test_table, Key, "value"}) end), timer:sleep(10) end || Key <- lists:seq(1,2000)].
[begin mnesia:dirty_write({test_table, Key, "value"}), timer:sleep(10) end || Key <- lists:seq(1,2000)].
```

Videos from those experiments were posted on [YouTube](https://www.youtube.com/channel/UCGkcbu799cC1rtMaQtAajpg)

## Developing

### Erlang
#### Running development release

You can also start the tool as a regular Erlang release and connect to its console to debug the tool itself.

```
$ make
$ rebar -f generate
$ ./rel/erlangpl/bin/erlangpl console node=testnode@127.0.0.1 cookie=YOURCOOKIE
```

### User Interface

#### Running standalone

`erlangpl-ui` can be started standalone using Node with npm or yarn.
We are recomending [yarn](https://yarnpkg.com/lang/en/) for that.

```shell
yarn && yarn start
```

Now, application can be found at `localhost:3000` and will be listening for messages from `localhost:37575` where you have to have [erlangpl](https://github.com/erlanglab/erlangpl) running.

#### Writing Elm code

Although `erlangpl-ui` is written in React we belive in Elm power. Because of that we support Elm in out build process.
This is possible because of [react-elm-components](https://github.com/evancz/react-elm-components) and [elm-webpack](https://github.com/elm-community/elm-webpack-loader).

You can write any separate component in Elm and then wrap it into React component which can be integrated with whole application. Elm code should be placed in `ui/src/elm` and every component whould have main file in this directory and all files related to this component in directory with the same name. React wrapper file should have the same name as Elm component and `flow` should be disabled for this file.

```elm
-- ui/src/elm/About.elm

module About exposing (..)

import Html exposing (text)

main =
    text "Hello world from Elm component"
```


```javascript
// ui/src/about/components/About.js

import React from 'react';
import Elm from 'react-elm-components';
import { About } from '../../elm/About.elm';

import './About.css';

const AboutWrapper = () => {
  return (
    <Elm src={About} />
  );
};

export default AboutWrapper;
```

Have fun!
