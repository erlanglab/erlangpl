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
      width: 0
    };
  }

  render() {
    const { info, overview } = this.props;

    const systemOverview = [
      ['Throughput', info.receive ? `${info.receive.count} msg` : undefined],
      ['Throughput', info.receive ? `${info.receive.sizes} B` : undefined],
      ['Processes', info.processCount],
      ['Spawns', info.spawn ? info.spawn.count : undefined],
      ['Exits', info.exit ? info.exit.count : undefined],
      ['Abnormal Exits', info.exit ? info.exit.abnormal : undefined],
      ['Memory', info.memoryTotal ? `${info.memoryTotal} B` : undefined]
    ];

    const throughputData = overview.receive.map(a => ({
      name: 'Throughput (B)',
      size: parseInt(a.sizes, 10)
    }));

    const memoryData = overview.memoryTotal.map(a => {
      return {
        name: 'Memory (MB)',
        usage: Number((parseInt(a, 10) / 1000000).toFixed(2))
      };
    });

    return (
      <Row className="SystemOverview">
        <Col xs={4}>
          <h5 className="SystemInfo-list-header">
            Overview (last 5 sec)
          </h5>
          <ListGroup className="SystemInfo-list">
            {systemOverview.map(([name, value], i) => (
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
              title="Throughput"
              height={250}
              width={this.state.width}
              data={throughputData}
              color="#1F79B7"
              dataKey="size"
              loaderText="Gathering throughput data"
            />
            <Chart
              title="Memory usage"
              height={250}
              width={this.state.width}
              data={memoryData}
              color="#8FBF47"
              dataKey="usage"
              domain={['dataMin', 'dataMax']}
              loaderText="Gathering memory usage data"
            />
          </Col>
        </Measure>
      </Row>
    );
  }
}

export default connect(
  state => {
    return {
      info: state.home.systemInfo,
      overview: state.home.systemOverview
    };
  },
  {}
)(SystemOverview);
