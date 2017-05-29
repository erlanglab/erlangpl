// @flow
import React, { Component } from 'react';
import { connect } from 'react-redux';
import PluginWrapper from '../../../core/components/PluginWrapper';

import { send } from '../../../sockets';
import {
  Sigma,
  // eslint-disable-next-line no-unused-vars
  SigmaEnableWebGL,
  Filter
} from 'react-sigma';
import ForceLink from 'react-sigma/lib/ForceLink';

import SidePanel from './SidePanel';
import DiffManager from './DiffManager';
import './SupTree.css';

class SupTree extends Component {
  state = {
    filterKey: Math.random()
  };

  handleNodeClick = ({ data }) => {
    send('epl_st_EPL', data.node.id);
  };

  nodeFilter = ({ app }) => {
    const application = this.props.apps.find(a => a.name === app);
    return application.selected;
  };

  componentWillReceiveProps(nextProps: any) {
    if (this.props.apps !== nextProps.apps) {
      this.setState({ filterKey: Math.random() });
    }
  }

  render() {
    return (
      <PluginWrapper
        className="SupTree"
        loading={this.props.tree.nodes.length === 0}
        loaderText="Creating graph"
        sidePanel={<SidePanel />}
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
          <DiffManager graph={this.props.tree} selected={this.props.selected}>
            <ForceLink
              edgeWeightInfluence={0}
              iterationsPerRender={1}
              randomize="locally"
              alignNodeSiblings
              timeout={this.props.tree.nodes.length * 15}
            />
            <Filter key={this.state.filterKey} nodesBy={this.nodeFilter} />
          </DiffManager>
        </Sigma>
      </PluginWrapper>
    );
  }
}

import * as actions from '../actions';

export default connect(
  state => {
    const info = state.eplSupTree.nodeInfo;
    return {
      apps: state.eplSupTree.apps,
      tree: state.eplSupTree.tree,
      selected: info ? info.id : ''
    };
  },
  { center: actions.center }
)(SupTree);
