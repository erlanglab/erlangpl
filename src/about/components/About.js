import React from 'react';
import Elm from 'react-elm-components';
import { About } from '../../elm/About.elm';

import header from '../images/header.png';

import './About.css';

const Wrapper = () => {
  return <Elm src={About} flags={header} />;
};

export default Wrapper;
