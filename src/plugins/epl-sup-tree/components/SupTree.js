// @flow
import React, { Component } from 'react';
import { connect } from 'react-redux';
import Viva from 'vivagraphjs';
import difference from 'lodash/difference';

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

    graphics
      .node(node => {
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
      })
      .placeNode((ui, pos) => {
        if (ui.node.data.type === 'supervisor') {
          // This callback is called by the renderer before it updates
          // node coordinate. We can use it to update corresponding DOM
          // label position;
          // we create a copy of layout position
          var domPos = {
            x: pos.x,
            y: pos.y
          };
          // And ask graphics to transform it to DOM coordinates:
          graphics.transformGraphToClientCoordinates(domPos);
          // then move corresponding dom label to its own position:
          var nodeId = ui.node.id;

          var labelStyle = this.domLabels[nodeId].style;
          labelStyle.left = domPos.x + 'px';
          labelStyle.top = domPos.y + 'px';
        }
      });

    this.layout = Viva.Graph.Layout.forceDirected(this.g, {
      springLength: 1,
      springCoeff: 0.0001,
      dragCoeff: 0.1,
      gravity: -1
    });

    // TODO (@baransu) check for webgl support and run instead of svg
    this.renderer = Viva.Graph.View.renderer(this.g, {
      container: this.div,
      graphics,
      layout: this.layout
    });
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
      const generateDOMLabels = graph => {
        // this will map node id into DOM element
        var labels = Object.create(null);
        graph.forEachNode(node => {
          if (node.data.type === 'supervisor') {
            var label = document.createElement('span');
            label.classList.add('node-label');
            label.innerText = node.id;
            labels[node.id] = label;
            this.div.appendChild(label);
          }
        });
        // NOTE: If your graph changes over time you will need to
        // monitor graph changes and update DOM elements accordingly
        return labels;
      };

      const precompute = (iterations, callback) => {
        // let's run 10 iterations per event loop cycle:
        var i = 0;
        while (iterations > 0 && i < 10) {
          this.layout.step();
          iterations--;
          i++;
        }
        if (iterations > 0) {
          setTimeout(
            () => {
              precompute(iterations, callback);
            },
            0
          ); // keep going in next even cycle
        } else {
          // we are done!
          callback();
        }
      };

      let list = [];
      Object.keys(props.tree).forEach(app => {
        if (Object.keys(props.tree[app]).length) {
          const parent = props.tree[app];
          if (this.all.indexOf(parent.id) < 0) {
            const app = this.g.addNode(parent.id, { ...parent });
            if (this.apps.includes(app)) this.apps.push(app);
          }
          list.push(parent.id);
          parent.children.forEach(child => this.mapChild(child, parent, list));
        }
      });

      this.domLabels = generateDOMLabels(this.g);

      if (this.first === undefined) {
        precompute(1000, () => {
          this.apps.forEach(app => this.layout.pinNode(app, true));
          this.renderer.run();
        });
      }

      // simple diffing to remove not existing nodes
      difference(this.all, list).forEach(
        id => console.log('removing:', id) || this.g.removeNode(id)
      );

      this.first = false;
      this.all = list;
    }
  }

  render() {
    return <div className="SupTree" ref={node => this.div = node} />;
  }
}

export default connect(state => ({ tree: state.eplSupTree }), {})(SupTree);
