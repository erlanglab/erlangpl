import React from 'react';
import Elm from 'react-elm-components';
import { About } from '../elm/About.elm';

import './About.css';

const AboutWrapper = () => {
  return (
    <div className="About">
      <Elm src={About} />
    </div>
  );
};

export default AboutWrapper;
