// @flow
import React, { Component } from 'react';
import { connect } from 'react-redux';
import Viva from 'vivagraphjs';
import difference from 'lodash/difference';

import { send } from '../../../sockets';

import './SupTree.css';

class SupTree extends Component {
  div: any;
  renderer: any;
  g: any;
  layout: any;
  first: boolean;
  apps: Array<any>;
  all: Array<string>;

  componentDidMount() {
    this.all = [];
    this.apps = [];
    this.g = Viva.Graph.graph();
    const graphics = Viva.Graph.View.webglGraphics();

    graphics.node(node => {
      let color = '#000', size = 15;
      if (node.data && node.data.type === 'worker') {
        color = '#f00';
        size = 5;
      } else if (node.data && node.data.type === 'supervisor') {
        color = '#008000';
        size = 10;
      }
      return Viva.Graph.View.webglSquare(size, color);
    });

    this.layout = Viva.Graph.Layout.forceDirected(this.g, {
      springLength: 1,
      springCoeff: 0.0001,
      dragCoeff: 0.1,
      gravity: -0.5
    });

    // TODO (@baransu) check for webgl support and run instead of svg
    this.renderer = Viva.Graph.View.renderer(this.g, {
      container: this.div,
      graphics,
      layout: this.layout,
      prerender: 1200
    });

    const events = Viva.Graph.webglInputEvents(graphics, this.g);

    // on click get info from server about node
    events.click(({ id }) => send('epl_st_EPL', id));
  }

  mapChild(child, parent, list) {
    if (this.all.indexOf(child.id) < 0) {
      this.g.addNode(child.id, { ...child });
      this.g.addLink(child.id, parent.id);
    }
    list.push(child.id);
    child.children.forEach(c => this.mapChild(c, child, list));
  }

  componentWillReceiveProps(props) {
    if (this.div) {
      let list = [];
      Object.keys(props.tree).forEach(app => {
        if (Object.keys(props.tree[app]).length) {
          const parent = props.tree[app];
          if (this.all.indexOf(parent.id) < 0) {
            const app = this.g.addNode(parent.id, { ...parent });
            if (!this.apps.includes(app)) {
              this.apps.push(app);
            }
          }
          list.push(parent.id);
          parent.children.forEach(child => this.mapChild(child, parent, list));
        }
      });

      if (this.first === undefined) {
        this.renderer.run();
        this.apps.forEach(app => this.layout.pinNode(app, true));
      }

      // simple diffing to remove not existing nodes
      difference(this.all, list).forEach(id => this.g.removeNode(id));

      this.first = false;
      this.all = list;
    }
  }

  render() {
    return <div className="SupTree" ref={node => this.div = node} />;
  }
}

export default connect(state => ({ tree: state.eplSupTree }), {})(SupTree);
