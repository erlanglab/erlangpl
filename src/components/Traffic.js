// @flow
import React, { Component } from 'react';
import { connect } from 'react-redux';
import Vizceral from 'vizceral-react';

import 'vizceral-react/dist/vizceral.css';
import './Traffic.css';

import TrafficTools from './TrafficTools';

import history from '../history';

class Traffic extends Component {
  handleViewChange(view: Array<string>) {
    history.push(`/traffic/${view.join('/')}`);
    // dispatching view change is handled in store.js as history listener
  }

  render() {
    return (
      <div className="Traffic">
        <TrafficTools className="Traffic-tools" />
        <div className="Traffic-container">
          <Vizceral
            traffic={this.props.data}
            view={this.props.view}
            viewChanged={event => this.handleViewChange(event.view)}
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
  state => ({
    data: state.traffic.data,
    view: state.traffic.view,
  }),
  {},
)(Traffic);
