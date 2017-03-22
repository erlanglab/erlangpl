// @flow
import React, { Component } from 'react';
import { connect } from 'react-redux';
import Vizceral from 'vizceral-react';
import { Motion, spring } from 'react-motion';

import 'vizceral-react/dist/vizceral.css';
import './Traffic.css';

import TrafficTools from './TrafficTools';
import * as actions from '../actions';

/* import sampleData from '../sample_data.json';*/

class Traffic extends Component {
  state: {
    start: number,
    end: number,
    height: number,
    width: number,
    graph: boolean
  };

  vizceral: any;

  constructor(props) {
    super(props);
    this.state = {
      height: 0,
      width: 0,
      start: 0,
      end: 0,
      graph: false
    };
  }

  resize = () => {
    const { clientWidth, clientHeight } = this.vizceral.refs.vizCanvas;
    this.setState({ width: clientWidth, height: clientHeight });
  };

  componentDidMount() {
    this.resize();
    window.addEventListener('resize', this.resize);
  }

  handleViewChange = (data: any) => {
    const sidePanelLevel = 3;
    let anim;
    const { view, graph } = data;
    if (
      view.length > sidePanelLevel - 1 &&
      this.props.view.length === sidePanelLevel - 1
    ) {
      //open
      anim = { end: 1, start: 0 };
    } else if (this.props.view.length > sidePanelLevel - 1) {
      //stay open
      anim = { end: 1, start: 1 };
    } else {
      //close
      anim = { end: 0, start: 1 };
    }

    this.setState({
      end: anim.end,
      start: anim.start,
      graph: graph !== null
    });

    this.props.updateTrafficView(view);
  };

  selectCentralNode = () => {
    this.props.updateTrafficView(['INTERNET']);
  };

  render() {
    const sidePanelWidth = 30;

    return (
      <div className="Traffic">
        <TrafficTools className="Traffic-tools" />
        <Motion
          defaultStyle={{ x: this.state.start, y: 1 }}
          style={{
            x: spring(this.state.end),
            y: this.state.graph ? spring(0) : 1
          }}
          children={({ x, y }) => (
            <div className="Traffic-container">
              <div
                className="Traffic-panel"
                style={{ width: `${100 - sidePanelWidth * x}%`, float: 'left' }}
              >

                {this.props.view.length === 0 &&
                  <div
                    className="fake-click"
                    style={{
                      width: this.state.height / 5,
                      height: this.state.height / 5
                    }}
                    onClick={this.selectCentralNode}
                  />}

                <Vizceral
                  ref={node => this.vizceral = node}
                  traffic={this.props.data}
                  view={this.props.view}
                  viewChanged={this.handleViewChange}
                  showLabels={true}
                  match={this.props.search}
                  allowDraggingOfNodes={true}
                  targetFramerate={25}
                />
                <div
                  className="loader"
                  style={{
                    visibility: y ? 'visible' : 'hidden',
                    opacity: y
                  }}
                >
                  <div className="text-center">
                    <div className="spinner">
                      <div className="bounce1" />
                      <div className="bounce2" />
                      <div className="bounce3" />
                    </div>
                    <span>Gathering cluster data</span>
                  </div>
                </div>
              </div>
              <div
                className="Traffic-panel"
                style={{
                  width: `${sidePanelWidth * x}%`,
                  opacity: x,
                  float: 'right'
                }}
              >
                <pre style={{ height: '100%' }}>
                  <code>
                    {this.props.nodeInfo
                      ? JSON.stringify(this.props.nodeInfo, null, 2)
                      : 'No info available'}
                  </code>
                </pre>
              </div>
            </div>
          )}
        />
      </div>
    );
  }
}

export default connect(
  state => {
    return {
      search: state.eplVizceral.search,
      nodeInfo: state.eplSupTree.nodeInfo,
      data: state.eplVizceral.data,
      view: state.eplVizceral.view
    };
  },
  {
    updateTrafficData: actions.updateTrafficData,
    updateTrafficView: actions.updateTrafficView
  }
)(Traffic);
