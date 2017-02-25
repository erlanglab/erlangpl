// @flow
import React from 'react';
import './Footer.css';
import { Popover, OverlayTrigger } from 'react-bootstrap';
import { connect } from 'react-redux';

type Props = {
  node: string,
};

const Footer = ({ node }: Props) => {
  return (
    <div className="Footer text-right">
      {/* TODO (@baransu) websocket connection status */}
      <div className="Footer-node">
        <OverlayTrigger
          trigger={['hover']}
          placement="top"
          overlay={
            (
              <Popover id="footer-node-popover" title="Node">
                <strong>Current node: </strong>{node}
              </Popover>
            )
          }
        >
          <span>{node}</span>
        </OverlayTrigger>
      </div>
    </div>
  );
};

export default connect(
  state => {
    return {
      node: state.systemInfo.nodeName,
    };
  },
  {},
)(Footer);
