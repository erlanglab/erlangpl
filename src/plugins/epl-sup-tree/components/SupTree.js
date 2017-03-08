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
  graphics: any;
  selected: { color: number, id: string };
  layout: any;
  first: boolean;
  apps: Array<any>;
  all: Array<string>;
  state: {
    apps: Array<string>
  };
  constructor(props: any) {
    super(props);
    this.state = {
      apps: []
    };
  }

  componentDidMount() {
    this.all = [];
    this.apps = [];
    this.g = Viva.Graph.graph();
    this.graphics = Viva.Graph.View.webglGraphics();

    this.graphics
      .node(node => {
        let color = '#000', size = 15;
        if (node.data && node.data.type === 'worker') {
          color = '#1F79B7';
          size = 10;
        } else if (node.data && node.data.type === 'supervisor') {
          color = '#227A50';
          size = 15;
        }

        return Viva.Graph.View.webglSquare(size, color);
      })
      .link(link => {
        return Viva.Graph.View.webglLine('#808080');
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
      graphics: this.graphics,
      layout: this.layout,
      prerender: 1200
    });

    const events = Viva.Graph.webglInputEvents(this.graphics, this.g);

    // on click get info from server about node
    events.click(({ id }) => {
      send('epl_st_EPL', id);
      this.selectNode(id);
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

  selectNode(id: string, center: ?boolean) {
    if (this.selected) {
      const node = this.graphics.getNodeUI(this.selected.id);
      node.color = this.selected.color;
    }

    const node = this.graphics.getNodeUI(id);
    this.selected = { id, color: node.color };
    node.color = 0xdf307dff;
    if (center) {
      const pos = this.layout.getNodePosition(id);
      this.renderer.moveTo(pos.x, pos.y);
    }
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
        this.setState({ apps: Object.keys(props.tree) });
        this.renderer.run();
        this.apps.forEach(app => this.layout.pinNode(app, true));
      }

      // simple diffing to remove not existing nodes
      difference(this.all, list).forEach(id => this.g.removeNode(id));

      this.first = false;
      this.all = list;
    }
  }

  handleAppClick(app: string, id: string) {
    this.toggleTree(id, this.state.apps.includes(app));
    this.setState(({ apps }) => {
      if (apps.includes(app)) {
        const index = apps.indexOf(app);
        return { apps: [...apps.slice(0, index), ...apps.slice(index + 1)] };
      } else {
        return { apps: [...apps, app].sort() };
      }
    });
  }

  changeColorNode(color: number, hide: boolean): number {
    // set to 00 - transparent
    // set to ff - solid
    return parseInt(color.toString(16).slice(0, 6) + (hide ? '00' : 'ff'), 16);
  }

  changeColorLink(color: number, hide: boolean): number {
    if (hide) {
      return parseInt(color.toString(16).slice(0, 6) + '00', 16);
    } else {
      return parseInt('808080ff', 16);
    }
  }

  toggleTree(id: string, hide: boolean) {
    const node = this.graphics.getNodeUI(id);
    if (node) {
      node.color = this.changeColorNode(node.color, hide);
      node.node.links.forEach(({ id }) => {
        const link = this.graphics.getLinkUI(id);
        link.color = this.changeColorLink(link.color, hide);
      });
      node.node.data.children.forEach(n => this.toggleTree(n.id, hide));
    }
    return undefined;
  }

  selectAll() {
    if (this.props.tree) {
      const apps = Object.keys(this.props.tree)
        .map(a => this.toggleTree(this.props.tree[a].id, false) || a);

      this.setState({ apps });
    }
  }

  clearAll() {
    if (this.props.tree) {
      this.state.apps.forEach(a =>
        this.toggleTree(this.props.tree[a].id, true));

      this.setState({ apps: [] });
    }
  }

  render() {
    return (
      <div className="SupTree">
        <div className="graph" ref={node => this.div = node} />
        <div className="side-panel">
          <div className="applications">
            <a onClick={() => this.selectAll()}>
              select all
            </a><br />
            <a onClick={() => this.clearAll()}>clear all</a>
            <ul>
              {Object.keys(this.props.tree).map(
                (app, key) => Object.keys(this.props.tree[app]).length
                  ? <li key={key}>
                      <input
                        type="checkbox"
                        checked={this.state.apps.includes(app)}
                        onChange={() =>
                          this.handleAppClick(app, this.props.tree[app].id)}
                      />
                      <a
                        onClick={() =>
                          this.selectNode(this.props.tree[app].id, true)}
                      >
                        {app}
                      </a>
                    </li>
                  : <li key={key}>
                      <span>{app}</span>
                    </li>
              )}
            </ul>
          </div>
          <div className="node-info">
            {this.props.nodeInfo
              ? <pre>
                  <code>
                    {JSON.stringify(this.props.nodeInfo, null, 2)}
                  </code>
                </pre>
              : ''}
          </div>
        </div>
      </div>
    );
  }
}

export default connect(
  state => ({
    tree: state.eplSupTree.tree,
    nodeInfo: console.log(state.eplSupTree.nodeInfo) ||
      state.eplSupTree.nodeInfo
  }),
  {}
)(SupTree);
