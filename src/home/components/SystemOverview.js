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

    const mem = ['KB', 'MB', 'GB'];

    const memory = mem.reduce(
      (acc, a) => {
        if (acc.v > 1000) {
          return { v: acc.v / 1000, t: a };
        } else {
          return acc;
        }
      },
      { v: parseInt(info.memoryTotal, 10), t: 'B' }
    );

    const systemOverview = [
      ['Throughput', info.receive ? `${info.receive.count} msg` : undefined],
      ['Throughput', info.receive ? `${info.receive.sizes} B` : undefined],
      ['Processes', info.processCount],
      ['Spawns', info.spawn ? info.spawn.count : undefined],
      ['Exits', info.exit ? info.exit.count : undefined],
      ['Abnormal Exits', info.exit ? info.exit.abnormal : undefined],
      [
        'Memory',
        info.memoryTotal ? `${memory.v.toFixed(2)} ${memory.t}` : undefined
      ]
    ];

    const throughputData = overview.receive.map(a => ({
      name: 'Throughput (msg)',
      count: parseInt(a.count, 10)
    }));

    const memoryData = overview.memoryTotal.map(a => {
      return {
        name: 'Memory (MB)',
        usage: Number((parseInt(a, 10) / 1000000).toFixed(2))
      };
    });

    const processesData = overview.processCount.map(a => ({
      name: 'Processes',
      count: parseInt(a, 10)
    }));

    const maxProcesses = Math.max(...overview.processCount);

    const dimensions = {
      width: this.state.width - this.state.width / 10,
      height: 210
    };

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
          <Col xs={8} className="charts">
            <Chart
              title="Memory usage"
              height={dimensions.height}
              width={dimensions.width}
              data={memoryData}
              color="#8FBF47"
              dataKey="usage"
              domain={['dataMin', 'dataMax']}
              loaderText="Gathering memory usage data"
            />
            <Chart
              title="Processes"
              height={dimensions.height}
              width={dimensions.width}
              data={processesData}
              color="#227A50"
              dataKey="count"
              domain={[
                `dataMin - ${Math.floor(maxProcesses / 5)}`,
                `dataMax + ${Math.floor(maxProcesses / 5)}`
              ]}
              loaderText="Gathering processes data"
            />
            <Chart
              title="Throughput"
              height={dimensions.height}
              width={dimensions.width}
              data={throughputData}
              color="#1F79B7"
              dataKey="count"
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
    return {
      info: state.home.systemInfo,
      overview: state.home.systemOverview
    };
  },
  {}
)(SystemOverview);
