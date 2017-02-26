// @flow
import React from 'react';

import './Loader.css';

type Props = {
  text: string,
};

const Loader = ({ text }: Props) => {
  return (
    <div className="text-center">
      <div className="spinner">
        <div className="bounce1" />
        <div className="bounce2" />
        <div className="bounce3" />
      </div>
      <span>{text}</span>
    </div>
  );
};

export default Loader;
