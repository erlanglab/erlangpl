// @flow
import React, { Component } from 'react';
import differenceBy from 'lodash/differenceBy';

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
    nodes.forEach(node => this.props.sigma.graph.addNode(node));
    edges.forEach(edge => this.props.sigma.graph.addEdge(edge));
    this.setState({ key: Math.random() });
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

    if (newEdges.length + newNodes.length > 0)
      this.setState({ key: Math.random() });
  }

  render() {
    // NOTE: We have to radnomize key to force ForceLink update
    return (
      <div key={this.state.key}>
        {embedProps(this.props.children, { sigma: this.props.sigma })}
      </div>
    );
  }
}

export default DiffManager;
