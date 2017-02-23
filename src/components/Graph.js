// @flow
import React, { Component } from 'react';
import { connect } from 'react-redux';
import Vizceral from 'vizceral-react';

import 'vizceral-react/dist/vizceral.css';
import './Graph.css';

import GraphTools from './GraphTools';

import history from '../history';

class Graph extends Component {
  handleViewChange(view: Array<string>) {
    history.push(`/graph/${view.join('/')}`);
    // dispatching view change is handled in store.js as history listener
  }

  render() {
    return (
      <div className="Graph">
        <GraphTools className="Graph-tools" />
        <div className="Graph-container">
          <Vizceral
            traffic={this.props.traffic}
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

import { updateGraphView } from '../actions/graph';

export default connect(
  state => ({
    traffic: state.graph.data,
    view: state.graph.view,
  }),
  {
    updateGraphView: updateGraphView,
  },
)(Graph);
