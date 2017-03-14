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
* our own custom webpack-dev-server based on erlang as much as we can (still we need Node runtime underneath)
* prebuild everything in our custom build system as fast as possible
* require modules/libraries from global scope

#### `webpack-dev-server` like (JIT?)

* most advanced solution, require Node runtime dependency
* we have Erlang "controller" which manager all file system level things happening
* when we require new plugin it's added to build list, then webpack run it's bundling (with import() for easier fauil catch)
* when we have new application version, we store state in localStorage, reload page and restore state
* when build fails, we're only showing info about failt (with logs for example) to user via WebSockets
* we have to make build time as fast as possible and as fabult tolerant as possible 

#### Prebuilt 

* builing whole UI with plugins
* then we can require every plugin with import() and filter if plugins work or not

#### Global (window) scope libraries

* this silution does not require Node runtime dependency
* we're building every plugin with webpack lib output and then require it as dynamically as we can

### More efficient runtime

* maybe we can pack everything into Vagrant for example or something like this
* we have to minimalize runtime dependencies for users not running development version (ideally only Erlang VM)

# Other cool things to have

* connect to node after opening browser/application
* list available nodes
* add nodes to favourite
* some config/settings system (localStorage or send data to Erlang and perform disk save)
* theme system (this is always nice)
* plugins/themes registry with package manager (git based)
