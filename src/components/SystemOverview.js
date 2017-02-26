// @flow
import React, { Component } from 'react';
import { connect } from 'react-redux';
import { Row, Col, ListGroup, ListGroupItem } from 'react-bootstrap';
import Measure from 'react-measure';

import Chart from './SystemOverviewChart';

import './SystemOverview.css';

class SystemOverview extends Component {
  state: { width: number };
  constructor(props) {
    super(props);
    this.state = {
      width: 0,
    };
  }

  render() {
    const { info } = this.props;
    console.log(info);

    const receiveLength = info.receive.length ? info.receive.length : 0;

    const overview = [
      [
        'throughput',
        receiveLength
          ? `${info.receive[receiveLength - 1].count} msg`
          : undefined,
      ],
      [
        'throughput',
        receiveLength
          ? `${info.receive[receiveLength - 1].sizes} B`
          : undefined,
      ],
      ['processes', info.processCount],
      ['spawns', info.spawn ? info.spawn.count : undefined],
      ['exits', info.exit ? info.exit.count : undefined],
      ['abnormal exits', info.exit ? info.exit.abnormal : undefined],
      ['memory', info.memoryTotal ? `${info.memoryTotal} B` : undefined],
    ];

    const data = info.receive.map(a => ({
      name: 'Throughput (B)',
      Size: parseInt(a.sizes),
    }));

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
        <Measure
          includeMargin={false}
          onMeasure={({ width }) => this.setState({ width })}
        >
          <Col xs={7}>
            <Chart
              title="Throughput chart"
              height={250}
              width={this.state.width}
              data={data}
              color="#1F79B7"
              dataKey="Size"
              axisDataKey="name"
              loaderText="Gathering throughput data"
            />
          </Col>
        </Measure>
      </Row>
    );
  }
}

export default connect(
  state => {
    return { info: state.systemInfo };
  },
  {},
)(SystemOverview);
