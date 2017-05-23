// @flow
import React, { Component } from 'react';
import differenceBy from 'lodash/differenceBy';

export function embedProps(elements: mixed, extraProps: any) {
  return React.Children.map(elements, element =>
    React.cloneElement(element, extraProps)
  );
}

class DiffManager extends Component {
  constructor(props: any) {
    super(props);
    this.state = { key: Math.random() };
    const { nodes, edges } = props.graph;
    nodes.forEach(node => props.sigma.graph.addNode(node));
    edges.forEach(edge => props.sigma.graph.addEdge(edge));
    props.sigma.startForceAtlas2();
  }

  componentWillReceiveProps(props: any) {
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
    newNodes.forEach(n => sigma.addNode(n));
    const newEdges = differenceBy(edges, this.props.graph.edges, 'id');
    newEdges.forEach(e => sigma.addEdge(e));

    // NOTE: We have to radnomize key to force ForceLink update
    if (newEdges.length + newNodes.length > 0)
      this.setState({ key: Math.random() });
  }

  render() {
    return (
      <div key={this.state.key}>
        {embedProps(this.props.children, { sigma: this.props.sigma })}
      </div>
    );
  }
}

export default DiffManager;
