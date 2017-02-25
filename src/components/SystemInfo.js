// @flow
import React from 'react';
import { connect } from 'react-redux';
import { Row, Col, ListGroup, ListGroupItem } from 'react-bootstrap';

import './SystemInfo.css';

const SystemInfo = ({ info }) => {
  const overview = [
    ['throughput', info.receive ? `${info.receive.count} msg` : undefined],
    ['throughput', info.receive ? `${info.receive.sizes} B` : undefined],
    ['processes', info.processCount],
    ['spawns', info.spawn ? info.spawn.count : undefined],
    ['exits', info.exit ? info.exit.count : undefined],
    ['abnormal exits', info.exit ? info.exit.abnormal : undefined],
    ['memory', info.memoryTotal ? `${info.memoryTotal} B` : undefined],
  ];

  const systemInfo = [
    ['opt release', info.otpRelease],
    ['erts version', info.version],
    ['node pid', info.nodePid],
    ['schedulers', info.schedulers],
    ['build type', info.buildType],
    ['wordsize', info.wordsize],
  ];

  const ertsSettings = [
    ['kernelPoll', info.kernelPoll],
    ['smpSupport', info.smpSupport],
    [
      'fullSweep',
      info.garbageCollection
        ? info.garbageCollection.fullsweepAfter
        : undefined,
    ],
    ['proces limit', info.processLimit],
    ['thread', info.threads],
    ['async threads', info.threadPoolSize],
    ['port limit', info.portLimit],
  ];

  return (
    <Row className="show-grid">
      <Col xs={4}>
        <h5 className="text-center">overview (last 5 sec)</h5>
        <ListGroup className="SystemInfo-list">
          {overview.map(([name, value], i) => (
            <ListGroupItem key={i}>
              <span>{name}</span>
              <span style={{ float: 'right' }}>{value || 'N/A'}</span>
            </ListGroupItem>
          ))}
        </ListGroup>
      </Col>
      <Col xs={4}>
        <h5 className="text-center">system info</h5>
        <ListGroup className="SystemInfo-list">
          {systemInfo.map(([name, value], i) => (
            <ListGroupItem key={i}>
              <span>{name}</span>
              <span style={{ float: 'right' }}>{value || 'N/A'}</span>
            </ListGroupItem>
          ))}
        </ListGroup>
      </Col>
      <Col xs={4}>
        <h5 className="text-center">erts settings</h5>
        <ListGroup className="SystemInfo-list">
          {ertsSettings.map(([name, value], i) => (
            <ListGroupItem key={i}>
              <span>{name}</span>
              <span style={{ float: 'right' }}>{value || 'N/A'}</span>
            </ListGroupItem>
          ))}
        </ListGroup>
      </Col>
    </Row>
  );
};

export default connect(
  state => {
    return { info: state.systemInfo };
  },
  {},
)(SystemInfo);
