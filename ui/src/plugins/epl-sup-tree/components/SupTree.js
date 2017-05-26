// @flow
import React, { Component } from 'react';
import { connect } from 'react-redux';
import PluginWrapper from '../../../core/components/PluginWrapper';

import { send } from '../../../sockets';
import {
  Sigma,
  // eslint-disable-next-line no-unused-vars
  SigmaEnableWebGL
} from 'react-sigma';
import ForceLink from 'react-sigma/lib/ForceLink';

// import SidePanel from './SidePanel';
import DiffManager from './DiffManager';
import './SupTree.css';

class SupTree extends Component {
  state = { selected: '' };

  handleNodeClick = ({ data }) => {
    this.setState({ selected: data.node.id });
    send('epl_st_EPL', data.node.id);
  };

  render() {
    return (
      <PluginWrapper
        className="SupTree"
        loading={this.props.tree.nodes.length === 0}
        loaderText="Creating graph"
      >
        <Sigma
          onClickNode={this.handleNodeClick}
          style={{ width: '100%', height: '100%' }}
          settings={{
            edgesPowRatio: 0.4,
            nodesPowRatio: 0.4,
            doubleClickEnabled: false,
            minEdgeSize: 0.1,
            minNodeSize: 1,
            maxNodeSize: 5,
            labelThreshold: 6,
            defaultLabelColor: '#fff',
            defaultHoverLabelBGColor: '#9da5b4',
            animationsTime: 1000,
            clone: false
          }}
        >
          <DiffManager graph={this.props.tree} selected={this.state.selected}>
            <ForceLink
              edgeWeightInfluence={0}
              iterationsPerRender={1}
              randomize="locally"
              alignNodeSiblings
              timeout={this.props.tree.nodes.length * 15}
            />
          </DiffManager>
        </Sigma>
      </PluginWrapper>
    );
  }
}

export default connect(
  state => ({
    tree: state.eplSupTree.tree
  }),
  {}
)(SupTree);
