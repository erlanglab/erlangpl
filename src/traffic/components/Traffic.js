// @flow
import React, { Component } from 'react';
import { connect } from 'react-redux';
import Vizceral from 'vizceral-react';
import { Motion, spring } from 'react-motion';

import 'vizceral-react/dist/vizceral.css';
import './Traffic.css';

import TrafficTools from './TrafficTools';
import * as actions from '../actions';

class Traffic extends Component {
  state: {
    start: number,
    end: number
  };
  constructor(props) {
    super(props);
    this.state = {
      start: 0,
      end: 0
    };
  }

  handleViewChange(view: Array<string>) {
    if (view.length > 0 && this.props.view.length === 0) {
      //open
      this.setState({ end: 1, start: 0 });
    } else if (this.props.view.length > 0) {
      //stay open
      this.setState({ end: 1, start: 1 });
    } else {
      //close
      this.setState({ end: 0, start: 1 });
    }

    this.props.updateTrafficView(view);
  }

  render() {
    const sidePanelWidth = 25;

    return (
      <div className="Traffic">
        <TrafficTools className="Traffic-tools" />
        <Motion
          defaultStyle={{ x: this.state.start }}
          style={{ x: spring(this.state.end) }}
          children={({ x }) => (
            <div className="Traffic-container">
              <div
                className="Traffic-panel"
                style={{ width: `${100 - sidePanelWidth * x}%`, float: 'left' }}
              >
                <Vizceral
                  traffic={this.props.data}
                  view={this.props.view}
                  viewChanged={event => this.handleViewChange(event.view)}
                  showLabels={true}
                  allowDraggingOfNodes={true}
                  targetFramerate={30}
                />
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
      data: state.traffic.data,
      view: state.traffic.view
    };
  },
  {
    updateTrafficView: actions.updateTrafficView
  }
)(Traffic);
