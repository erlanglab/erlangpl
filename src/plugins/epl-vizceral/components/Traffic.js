// @flow
import React, { Component } from 'react';
import { connect } from 'react-redux';
import Vizceral from 'vizceral-react';
import { Motion, spring } from 'react-motion';

import loaderLogo from '../images/erlanglab_logo_alpha_250.png';
import loaderText from '../images/erlangpl_test_1.jpg';

import 'vizceral-react/dist/vizceral.css';
import './Traffic.css';

import TrafficTools from './TrafficTools';
import * as actions from '../actions';

import sampleData from '../sample_data.json';

class Traffic extends Component {
  state: {
    start: number,
    end: number,
    graph: boolean
  };

  constructor(props) {
    super(props);
    this.state = {
      start: 0,
      end: 0,
      graph: false
    };
  }

  // TODO (baransu) remove dispatching dummy data once we have server connection
  componentDidMount() {
    setTimeout(
      () => {
        this.props.updateTrafficData(sampleData);
      },
      1500
    );
  }

  handleViewChange(data: any) {
    const sidePanelLevel = 2;
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
  }

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
                <Vizceral
                  traffic={this.props.data}
                  view={this.props.view}
                  viewChanged={event => this.handleViewChange(event)}
                  showLabels={true}
                  allowDraggingOfNodes={true}
                  targetFramerate={30}
                />
                <div
                  className="loader"
                  style={{
                    visibility: y ? 'visible' : 'hidden',
                    opacity: y
                  }}
                >
                  <div className="loader-img">
                    <img
                      src={loaderLogo}
                      style={{ width: '75px', height: '75px' }}
                      alt="Erlang Performance Lab"
                    />
                    <img
                      style={{ width: '540px', height: '75px' }}
                      src={loaderText}
                      alt="Erlang Performance Lab"
                    />
                  </div>
                </div>
              </div>
              <div
                className="Traffic-panel text-center"
                style={{
                  width: `${sidePanelWidth * x}%`,
                  opacity: x,
                  float: 'right'
                }}
              >
                <h3>side panel content</h3>
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
      data: state.eplVizceral.data,
      view: state.eplVizceral.view
    };
  },
  {
    updateTrafficData: actions.updateTrafficData,
    updateTrafficView: actions.updateTrafficView
  }
)(Traffic);
