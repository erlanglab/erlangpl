// @flow
import React from 'react';
import './Footer.css';
import { Popover, OverlayTrigger } from 'react-bootstrap';
import { connect } from 'react-redux';

type Props = {
  node: string,
  connection: string
};

const Footer = ({ node, connection }: Props) => {
  const color = ({
    connected: 'green',
    disconnected: 'red'
  })[connection];

  return (
    <div className="Footer text-right">
      <div className="Footer-item">
        <OverlayTrigger
          trigger={['hover']}
          placement="top"
          overlay={
            (
              <Popover id="footer-node" title="Node">
                Connected to: <strong>{node}</strong>
              </Popover>
            )
          }
        >
          <span>{node}</span>
        </OverlayTrigger>
      </div>

      <div className="Footer-item">
        <OverlayTrigger
          trigger={['hover']}
          placement="top"
          overlay={
            (
              <Popover id="footer-connection" title="Connection">
                Status: <strong style={{ color }}>{connection}</strong>
              </Popover>
            )
          }
        >
          <i className="fa fa-plug" style={{ color }} />
        </OverlayTrigger>
      </div>
    </div>
  );
};

export default connect(
  state => {
    return {
      node: state.systemInfo.nodeName,
      connection: state.connection
    };
  },
  {}
)(Footer);
