// @flow
import React, { Component } from 'react';
import { connect } from 'react-redux';
// import difference from 'lodash/difference';
// import { Motion, spring } from 'react-motion';
// import { ListGroup, ListGroupItem } from 'react-bootstrap';

// import { send } from '../../../sockets';
import {
  Sigma,
  EdgeShapes,
  // SigmaEnableWebGL,
  NodeShapes,
  RelativeSize,
  ForceAtlas2,
  RandomizeNodePositions
} from 'react-sigma';

import DiffManager from './DiffManager';
import ForceLink from 'react-sigma/lib/ForceLink';

import './SupTree.css';

class SupTree extends Component {
  div: any;

  componentDidMount() {}

  componentWillReceiveProps(props) {}

  render() {
    return (
      <div className="SupTree">
        <Sigma
          style={{ width: '100%', height: '100%' }}
          settings={{
            mouseZoomDuration: 50,
            doubleClickEnabled: false,
            minEdgeSize: 0.1,
            minNodeSize: 1.5,
            maxNodeSize: 5,
            defaultLabelColor: '#fff',
            animationsTime: 1000,
            clone: false
          }}
        >
          <DiffManager graph={this.props.tree}>
            <EdgeShapes default="tapered" />
            <NodeShapes default="diamond" />
            <RandomizeNodePositions />
            {/* <ForceLink
              background={true}
              iterationsPerRender={1}
              easing="cubicInOut"
            /> */}
            <RelativeSize initialSize={15} />
            <ForceAtlas2 iterationsPerRender={1} />
          </DiffManager>
        </Sigma>
      </div>
    );
  }
}

export default connect(
  state => ({
    tree: state.eplSupTree.tree,
    nodeInfo: state.eplSupTree.nodeInfo
  }),
  {}
)(SupTree);
