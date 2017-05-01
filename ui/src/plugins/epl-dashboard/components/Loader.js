// @flow
import React from 'react';

import './Loader.css';

type Props = {
  text: string,
  style: any
};

const Loader = ({ text, style }: Props) => {
  return (
    <div className="text-center" style={{ ...style, paddingTop: '100px' }}>
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
