# Erlang Performance Lab UI
[![Build Status](https://travis-ci.org/erlanglab/erlangpl-ui.svg?branch=master)](https://travis-ci.org/erlanglab/erlangpl-ui)
[![Code formatting](https://img.shields.io/badge/code%20formatting-prettier-ff69b4.svg)](https://github.com/prettier/prettier)
[![Code formatting](https://img.shields.io/badge/code%20formatting-elm--format-ff69b4.svg)](https://github.com/avh4/elm-format)

> UI form Erlang Performance Lab

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

## Writing Elm code

Although `erlangpl-ui` is written in React we belive in Elm power. Because of that we support Elm in out build process.
This is possible because of [react-elm-components](https://github.com/evancz/react-elm-components) and [elm-webpack](https://github.com/elm-community/elm-webpack-loader). 

You can write any separate component in Elm and then wrap it into React component which can be integrated with whole application. Elm code should be placed in `src/elm` and every component whould have main file in this directory and all files related to this component in directory with the same name. React wrapper file should have the same name as Elm component and `flow` should be disabled for this file.

```elm
-- src/elm/About.elm

module About exposing (..)

import Html exposing (text)

main =
    text "Hello world from Elm component"
```


```javascript
// src/about/components/About.js

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

## Code formatting
We belive in code consistency. Because of that we care about code formatting.

### Elm
Elm code should be formatted using latest version of [elm-format](https://github.com/avh4/elm-format)

### JavaScript
JavaScript code should be formatted using latest version of [prettier](https://github.com/prettier/prettier) with `singleQuote: true`.
