// @flow
import React from 'react';
import './Graph.css';
/* import TrafficFlow from './TrafficFlow.js';*/

import { connect } from 'react-redux';

import Vizceral from 'vizceral-react';
import 'vizceral-react/dist/vizceral.css';

const Graph = ({ trafficData }) => {
  return (
    <div className="Graph">
      <Vizceral
        traffic={trafficData}
        view={['']}
        showLabels={true}
        filters={[]}
        viewChanged={() => {}}
        viewUpdated={() => {}}
        allowDraggingOfNodes={true}
        targetFramerate={30}
      />
    </div>
  );
};

const mapStateToProps = state => ({ trafficData: state.trafficData });

export default connect(mapStateToProps, {})(Graph);
