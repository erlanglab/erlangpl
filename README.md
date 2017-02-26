# Erlang Performance Lab UI
[![Build Status](https://travis-ci.org/erlanglab/erlangpl-ui.svg?branch=master)](https://travis-ci.org/erlanglab/erlangpl-ui)

The Erlang Performance Lab UI is under heavy development leaning towards replacement for current [erlangpl](https://github.com/erlanglab/erlangpl) frontend.

## Getting started

`erlangpl-ui` can be started standalone using Node with npm or yarn.
We are recomending [yarn](https://yarnpkg.com/lang/en/) for that.

```shell
git clone https://github.com/erlanglab/erlangpl-ui.git
cd erlangpl-ui
yarn
yarn start
```

Now, application can be found at `localhost:3000` and will be listening for messages from `localhost:8000` where you have to have [erlangpl](https://github.com/erlanglab/erlangpl) running. 
