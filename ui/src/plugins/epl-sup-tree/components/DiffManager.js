// @flow
import React, { Component } from 'react';
import differenceBy from 'lodash/differenceBy';

import { SELECTED } from '../constants';

function embedProps(elements: mixed, extraProps: any) {
  return React.Children.map(elements, element =>
    React.cloneElement(element, extraProps)
  );
}

class DiffManager extends Component {
  constructor(props: any) {
    super(props);
    this.state = { key: Math.random() };
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
    const sigma = props.sigma.graph;

    // REMOVE OLD
    differenceBy(this.props.graph.edges, edges, 'id').forEach(e =>
      sigma.dropEdge(e.id)
    );
    differenceBy(this.props.graph.nodes, nodes, 'id').forEach(n =>
      sigma.dropNode(n.id)
    );

    // ADD NEW
    const newNodes = differenceBy(nodes, this.props.graph.nodes, 'id');
    newNodes.forEach(e => sigma.addNode(e));
    const newEdges = differenceBy(edges, this.props.graph.edges, 'id');
    newEdges.forEach(e => sigma.addEdge(e));

    if (this.props.selected !== props.selected) {
      // clear old selected node
      this.updateNode(
        this.props.selected,
        '',
        this.props.graph.nodes,
        this.props.graph.edges,
        this.props.sigma.graph
      );
      // show new selected node
      this.updateNode(selected, SELECTED, nodes, edges, sigma);
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
