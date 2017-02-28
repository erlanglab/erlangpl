// @flow
import React, { Component } from 'react';
import { connect } from 'react-redux';
import Vizceral from 'vizceral-react';

import 'vizceral-react/dist/vizceral.css';
import './Traffic.css';

import TrafficTools from './TrafficTools';
import * as actions from '../actions';

class Traffic extends Component {
  render() {
    return (
      <div className="Traffic">
        <TrafficTools className="Traffic-tools" />
        <div className="Traffic-container">
          <Vizceral
            traffic={this.props.data}
            view={this.props.view}
            viewChanged={event => this.props.updateTrafficView(event.view)}
            showLabels={true}
            allowDraggingOfNodes={true}
            targetFramerate={30}
          />
        </div>
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
