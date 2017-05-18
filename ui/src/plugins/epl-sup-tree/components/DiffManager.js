// @flow
import React, { Component } from 'react';

export function embedProps(elements: mixed, extraProps: any) {
  return React.Children.map(elements, element =>
    React.cloneElement(element, extraProps)
  );
}

class DiffManager extends Component {
  constructor(props: any) {
    super(props);
    const { nodes, edges } = props.graph;
    nodes.forEach(node => props.sigma.graph.addNode(node));
    edges.forEach(edge => props.sigma.graph.addEdge(edge));
  }

  componentWillReceiveProps(props: any) {
    const { nodes, edges } = props.graph;
    console.log(props);
    nodes.forEach(node => props.sigma.graph.addNode(node));
    edges.forEach(edge => props.sigma.graph.addEdge(edge));
    // TODO: create list of old and new (nodes, edges)
    // TODO: remove old, add new (nodes, edges)
    console.log(props.sigma);
    // props.sigma.startForceAtlas2();
  }

  render() {
    return (
      <div key={Math.random()}>
        {embedProps(this.props.children, { sigma: this.props.sigma })}
      </div>
    );
  }
}

export default DiffManager;
