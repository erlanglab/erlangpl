# Plugins 

## What we want?

* icon (left bar) - icon or somethins similar, very easy to implement I guess
* footer component - display minimum custom info, some tooltip overlay etc, should have access to internal state 
* main view (specific route) - route based view, `/:plugin/:anything*`

* own state - every plugin should have internal state which will be preserved on view change
* communication to other plugins - because some plugins can have state that other don't have, or plugins could require some computation, state change (cast, call)
* websocket connecton - heart of every plugin because it allows us to get info about underlaying Erlang application we're inspecting
* ability to run async in background - everything mention above should run async in background so we can have only computional plugin for example

## What options we have?

### Electron

* dynamic loading should be easy (we can learn a lot from Atom plugin system)
* not so multiplatform as Erlang which is available on probably every system right now

### Web

Two main aproaches are:
* our own custom webpack-dev-server based on erlang as much as we can (still we need node underneath)
* probuild everything in our custom build system as fast as possible

#### `webpack-dev-server` like

* Hot Module Replacement, how webpack-dev-server handles that, maybe we can run two separate servers at the same time? but probably we can achive the same thing with erlang too because it's all based on http/ws protocols
* run something like webpack-dev-server, then you can change some file based on client/server communication change that file and force webpack-dev-server to reload
* something like we can run development server whole time and not build for production of optimize dev-server for production as much as we can

#### Prebuild 

* probably we have to build whole application knowing which plugin to include
* import() is good for dynamic loading but it requires build step right now I guess


# Other cool things to have

* connect to node after opening browser/application
* list available nodes
* add nodes to favourite
* some config/settings system (localStorage or send data to Erlang and perform disk save)
* theme system (this is always nice)
* plugins/themes registry with package manager (git based)
