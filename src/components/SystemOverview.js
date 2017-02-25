// @flow
import React from 'react';
import { connect } from 'react-redux';
import { Row, Col, ListGroup, ListGroupItem } from 'react-bootstrap';

import './SystemInfo.css';
import './SystemOverview.css';

const SystemOverview = ({ info }) => {
  const overview = [
    ['throughput', info.receive ? `${info.receive.count} msg` : undefined],
    ['throughput', info.receive ? `${info.receive.sizes} B` : undefined],
    ['processes', info.processCount],
    ['spawns', info.spawn ? info.spawn.count : undefined],
    ['exits', info.exit ? info.exit.count : undefined],
    ['abnormal exits', info.exit ? info.exit.abnormal : undefined],
    ['memory', info.memoryTotal ? `${info.memoryTotal} B` : undefined],
  ];

  return (
    <Row className="SystemOverview">
      <Col xs={4}>
        <h5 className="SystemInfo-list-header">
          Overview (last 5 sec)
        </h5>
        <ListGroup className="SystemInfo-list">
          {overview.map(([name, value], i) => (
            <ListGroupItem key={i}>
              <span>{name}</span>
              <span className="value">{value || 'N/A'}</span>
            </ListGroupItem>
          ))}
        </ListGroup>
      </Col>
      <Col xs={8}>
        here will be messages throughput chart
      </Col>
    </Row>
  );
};

export default connect(
  state => {
    return { info: state.systemInfo };
  },
  {},
)(SystemOverview);
