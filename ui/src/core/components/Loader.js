// @flow
import React from 'react';

import './Loader.css';

type Props = {
  text: string
};

const Loader = ({ text }: Props) => (
  <div className="loader">
    <div className="text-center">
      <div className="spinner">
        <div className="bounce1" />
        <div className="bounce2" />
        <div className="bounce3" />
      </div>
      <span>{text}</span>
    </div>
  </div>
);

export default Loader;
