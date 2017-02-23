// @flow
import React from 'react';
import { connect } from 'react-redux';
import Vizceral from 'vizceral-react';

import 'vizceral-react/dist/vizceral.css';
import './Graph.css';

import GraphTools from './GraphTools';

const Graph = ({ traffic, view, updateGraphView }) => {
  return (
    <div className="Graph">
      <GraphTools className="Graph-tools" />
      <div className="Graph-container">
        <Vizceral
          traffic={traffic}
          view={view}
          viewChanged={event => updateGraphView(event.view)}
          showLabels={true}
          allowDraggingOfNodes={true}
          targetFramerate={30}
        />
      </div>
    </div>
  );
};

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
