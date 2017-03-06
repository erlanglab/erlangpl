// @flow
import React, { Component } from 'react';
import { connect } from 'react-redux';
import Viva from 'vivagraphjs';

import './SupTree.css';

let iter = 0;

class SupTree extends Component {
  div: any;
  renderer: any;
  g: any;

  constructor(props) {
    super(props);
    this.state = {
      all: []
    };
  }

  componentDidMount() {
    this.g = Viva.Graph.graph();
    const graphics = Viva.Graph.View.webglGraphics({
      clearColor: true, // we want to avoid rendering artifacts
      clearColorValue: {
        r: 1,
        g: 1,
        b: 1,
        a: 1
      }
    });

    graphics.node(node => {
      let color = '#000';
      let size = 15;
      if (node.data && node.data.type === 'worker') {
        color = '#f00';
        size = 5;
      } else if (node.data && node.data.type === 'supervisor') {
        color = '#0f0';
        size = 10;
      }
      return Viva.Graph.View.webglSquare(size, color);
    });

    this.layout = Viva.Graph.Layout.forceDirected(this.g, {
      springLength: 1,
      springCoeff: 0.0001,
      dragCoeff: 0.1,
      gravity: -1
    });

    // run layout for as many iterations as we want:
    /* this.layout = Viva.Graph.Layout.forceDirected(this.g, {
     *   springLength: 0.1,
     *   springCoeff: 0.0001,
     *   dragCoeff: 1,
     *   gravity: -0.05
     * });*/

    // check for webgl support and run instead of svg
    // TODO (@baransu) fix svg
    this.renderer = Viva.Graph.View.renderer(this.g, {
      container: this.div,
      graphics,
      layout: this.layout
    });
    this.renderer.run();
  }

  mapChild(child, parent) {
    this.g.addNode(child.id, { ...child });
    this.g.addLink(child.id, parent.id);
    return child.children.map(c => this.mapChild(c, child));
  }

  componentWillReceiveProps(props) {
    if (this.div) {
      console.log(props.tree);
      //render pelna bulwa

      const root = this.g.addNode('whole');
      this.layout.pinNode(root, true);

      const tree = Object.keys(props.tree).map(app => {
        if (Object.keys(props.tree[app]).length) {
          const parent = props.tree[app];
          this.g.addNode(parent.id, { ...parent });

          this.g.addLink('whole', parent.id);
          return parent.children.map(child => this.mapChild(child, parent));
        } else {
          return undefined;
        }
      });

      if (this.tree === undefined) {
        for (var i = 0; i < 1000; ++i) {
          this.layout.step();
        }
      }

      this.tree = tree;
      /* console.log(this.g);
       * // check if node exist if not, add;
       * this.g.addNode('some' + iter);
       * this.g.addNode('other' + iter);
       * this.link = this.g.addLink('some' + iter, 'other' + iter);
       * if (iter) {
       *   this.g.addLink('some' + iter, 'some' + (iter - 1));
       *   if (iter > 1) {
       *     this.g.removeLink(this.link);
       *   }
       * }
       * iter++;*/
    }
  }

  render() {
    return <div className="SupTree" ref={node => this.div = node} />;
  }
}

export default connect(state => ({ tree: state.eplSupTree }), {})(SupTree);
