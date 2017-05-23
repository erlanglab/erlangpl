// @flow
import React, { Component } from 'react';
import { connect } from 'react-redux';
// import { Motion, spring } from 'react-motion';
// import { ListGroup, ListGroupItem } from 'react-bootstrap';

// import { send } from '../../../sockets';
import { Sigma } from 'react-sigma';
import ForceLink from 'react-sigma/lib/ForceLink';

import DiffManager from './DiffManager';
import './SupTree.css';

class SupTree extends Component {
  div: any;

  componentDidMount() {}

  componentWillReceiveProps(props) {}

  render() {
    return (
      <div className="SupTree">

        {this.props.tree.nodes.length === 0 &&
          <div className="loader">
            <div className="text-center">
              <div className="spinner">
                <div className="bounce1" />
                <div className="bounce2" />
                <div className="bounce3" />
              </div>
              <span>Creating graph</span>
            </div>
          </div>}

        <Sigma
          style={{ width: '100%', height: '100%' }}
          settings={{
            edgesPowRatio: 0.4,
            nodesPowRatio: 0.4,
            doubleClickEnabled: false,
            minEdgeSize: 0.1,
            minNodeSize: 1,
            maxNodeSize: 5,
            labelThreshold: 10,
            defaultLabelColor: '#fff',
            defaultHoverLabelBGColor: '#9da5b4',
            animationsTime: 1000,
            clone: false
          }}
        >
          <DiffManager graph={this.props.tree}>
            <ForceLink
              edgeWeightInfluence={0}
              iterationsPerRender={1}
              randomize="locally"
              alignNodeSiblings
              timeout={this.props.tree.nodes.length * 50}
            />
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
