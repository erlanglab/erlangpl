// @flow
import React, { Component } from 'react';
import { connect } from 'react-redux';
import { Motion, spring } from 'react-motion';
import { ListGroup, ListGroupItem } from 'react-bootstrap';

import { COLORS } from '../constants';

type Props = {};

class SidePanel extends Component {
  state: {
    collapse: boolean,
    height: Array<number>
  };

  constructor(props: Props) {
    super(props);
    this.state = {
      collapse: false,
      height: [50, 50]
      // motion stuff
      // currently selected node
    };
  }

  toggleCollapse = () => {
    this.setState(({ collapse }) => ({
      collapse: !collapse,
      height: collapse ? [50, 0] : [0, 50]
    }));
  };

  selectAll = () => {
    // TODO: show all apps
  };

  clearAll = () => {
    // TODO: hide all apps
  };

  handleAppClick = () => {
    // TODO: toggle application
  };

  selectNode = () => {
    // TODO: change color of selected application
  };

  render() {
    return (
      <div className="side-panel">

        <div className="head" onClick={this.toggleCollapse}>
          <h4
            className="text-center"
            style={{
              color: COLORS[this.state.selected.type] || 'inherit'
            }}
          >
            {this.state.selected.id}
          </h4>
          <i className={`fa fa-angle-${this.state.collapse ? 'down' : 'up'}`} />
        </div>

        <Motion
          defaultStyle={{ height: this.state.height[0] }}
          style={{ height: spring(this.state.height[1]) }}
          children={({ height }) => (
            <div className="side-content" style={{}}>

              <div
                className="applications"
                style={{ height: `calc(${height}%)` }}
              >

                <ListGroup style={{ margin: '10px 0px' }}>
                  <ListGroupItem className="application-link">
                    <button onClick={this.selectAll}>
                      Select all
                    </button>
                    <button onClick={this.clearAll}>
                      Clear all
                    </button>
                  </ListGroupItem>
                  {Object.keys(this.props.tree).map(
                    (app, key) =>
                      (Object.keys(this.props.tree[app]).length
                        ? <ListGroupItem key={key} className="application-link">
                            <input
                              type="checkbox"
                              checked={this.state.apps.includes(app)}
                              onChange={() =>
                                this.handleAppClick(
                                  app,
                                  this.props.tree[app].id
                                )}
                            />
                            <a
                              style={{ marginLeft: '5px' }}
                              onClick={() =>
                                this.selectNode(this.props.tree[app].id, true)}
                            >
                              {app}
                            </a>
                          </ListGroupItem>
                        : <ListGroupItem key={key} className="application-link">
                            <span style={{ marginLeft: '17px' }}>
                              {app}
                            </span>
                          </ListGroupItem>)
                  )}
                </ListGroup>
              </div>

              <div
                className="node-info"
                style={{ height: `calc(${100 - height}%)` }}
              >
                <pre style={{ height: '100%' }}>
                  <code>
                    {this.props.nodeInfo &&
                      JSON.stringify(this.props.nodeInfo, null, 2)}
                  </code>
                </pre>
              </div>
            </div>
          )}
        />
      </div>
    );
  }
}

export default connect(
  state => ({
    nodeInfo: state.eplSupTree.nodeInfo
  }),
  {}
)(SidePanel);
