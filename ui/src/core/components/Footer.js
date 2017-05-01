// @flow
import React from 'react';
import { connect } from 'react-redux';

import './Footer.css';
import FooterItem from './FooterItem';

type Props = {
  node: string,
  connection: string
};

const Footer = ({ node, connection }: Props) => {
  const color = {
    connected: '#227A50',
    disconnected: '#C13035'
  }[connection];

  return (
    <div className="Footer text-right">
      <FooterItem
        id="footer-node"
        title="Node"
        popover={
          <span>
            Connected to: <strong>{node}</strong>
          </span>
        }
        item={<span>{node}</span>}
      />

      <FooterItem
        id="footer-connectionn"
        title="Connection"
        popover={
          <span>
            Status: <strong style={{ color }}>{connection}</strong>
          </span>
        }
        item={<i className="fa fa-plug" style={{ color }} />}
      />
    </div>
  );
};

export default connect(state => {
  return {
    node: state.core.node,
    connection: state.core.connection
  };
}, {})(Footer);
