// @flow
import React, { Component } from 'react';
import differenceBy from 'lodash/differenceBy';
import get from 'lodash/get';

import { SELECTED } from '../constants';

function embedProps(elements: mixed, extraProps: any) {
  return React.Children.map(elements, element =>
    React.cloneElement(element, extraProps)
  );
}

class DiffManager extends Component {
  state: {
    key: number,
    center: { x: number, y: number }
  };

  constructor(props: any) {
    super(props);
    this.state = {
      key: Math.random(),
      center: { x: 0, y: 0 }
    };
  }

  componentDidMount() {
    const { nodes, edges } = this.props.graph;
    const sigma = this.props.sigma.graph;
    nodes.forEach(n => sigma.addNode(n));
    edges.forEach(e => sigma.addEdge(e));
    this.setState({ key: Math.random() });
  }

  componentWillReceiveProps({ selected, ...props }: any) {
    const { nodes, edges } = props.graph;
    const sigmaGraph = props.sigma.graph;

    // REMOVE OLD
    differenceBy(this.props.graph.edges, edges, 'id').forEach(e =>
      sigmaGraph.dropEdge(e.id)
    );
    differenceBy(this.props.graph.nodes, nodes, 'id').forEach(n =>
      sigmaGraph.dropNode(n.id)
    );

    // ADD NEW
    const newNodes = differenceBy(nodes, this.props.graph.nodes, 'id');
    newNodes.forEach(e => sigmaGraph.addNode(e));
    const newEdges = differenceBy(edges, this.props.graph.edges, 'id');
    newEdges.forEach(e => sigmaGraph.addEdge(e));

    if (selected && this.props.selected !== selected) {
      // CLEAR OLD SELECTED NODE
      this.updateNode(
        this.props.selected,
        '',
        this.props.graph.nodes,
        this.props.graph.edges,
        this.props.sigma.graph
      );

      // SHOW NEW SELECTED NODE
      this.updateNode(selected, SELECTED, nodes, edges, sigmaGraph);

      // CENTER ON NODE
      const node = sigmaGraph.nodes().find(node => node.id === selected);
      const { x, y } = props.sigma.camera.graphPosition(node.x, node.y);
      // $FlowFixMe
      sigma.misc.animation.camera(
        props.sigma.camera,
        {
          // NOTE: this is far from being nice but it works
          // I hope it won't break in future
          x: x / 5,
          y: y / 5,
          ratio: 1
        },
        { duration: props.sigma.settings('animationsTime') || 300 }
      );
    }

    if (newEdges.length + newNodes.length > 0)
      this.setState({ key: Math.random() });

    props.sigma.refresh();
  }

  updateNode = (
    id: string,
    // NOTE: empty sring will clear color
    color: string,
    nodes: Array<*>,
    edges: Array<*>,
    sigma: any
  ) => {
    const fn = n => n.id === id;
    const selectedNode = nodes.find(fn);
    const graphSelectedNode = sigma.nodes().find(fn);

    if (selectedNode && graphSelectedNode) {
      const position = {
        x: graphSelectedNode.x,
        y: graphSelectedNode.y
      };

      const connectedEdges = edges.filter(
        e => e.source === id || e.target === id
      );

      connectedEdges.forEach(e => sigma.dropEdge(e.id));

      sigma.dropNode(id).addNode(
        color
          ? {
              ...selectedNode,
              ...position,
              color
            }
          : { ...selectedNode, ...position }
      );

      connectedEdges.forEach(e => sigma.addEdge(e));
    }
  };

  render() {
    // NOTE: We have to randomize key to force ForceLink update
    return (
      <div key={this.state.key}>
        {embedProps(this.props.children, { sigma: this.props.sigma })}
      </div>
    );
  }
}

export default DiffManager;
