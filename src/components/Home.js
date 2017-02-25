// @flow
import React from 'react';
import { connect } from 'react-redux';
import { ListGroup, ListGroupItem, Grid, Row, Col } from 'react-bootstrap';

import header from '../images/erlangpl_header.png';

import './Home.css';

const Home = ({ info }) => {
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
    <Grid className="Home">
      <Row className="show-grid">
        <Col xs={12}>
          <img src={header} style={{ margin: '5px 0' }} />
        </Col>
        <Col xs={4}>
          <h5 className="text-center">overview (last 5 sec)</h5>
          <ListGroup>
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
          <ListGroup>
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
          <ListGroup>
            {ertsSettings.map(([name, value], i) => (
              <ListGroupItem key={i}>
                <span>{name}</span>
                <span style={{ float: 'right' }}>{value || 'N/A'}</span>
              </ListGroupItem>
            ))}
          </ListGroup>
        </Col>
        {/* <Col xs={12}>
            perf graph
            </Col> */}
      </Row>
    </Grid>
  );
};

export default connect(
  state => {
    return { info: state.systemInfo };
  },
  {},
)(Home);
