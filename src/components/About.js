// @flow
import React from 'react';
import Elm from 'react-elm-components';
import { Main } from '../elm/Main.elm';

import './About.css';

const About = () => {
  return (
    <div className="About">
      About
      <Elm src={Main} />
    </div>
  );
};

export default About;
