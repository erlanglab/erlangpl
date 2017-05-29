// @flow
import React, { Component } from 'react';
import { connect } from 'react-redux';
import { Motion, spring } from 'react-motion';
import { ListGroup, ListGroupItem } from 'react-bootstrap';

import { send } from '../../../sockets';
import { COLORS } from '../constants';

type Props = {
  selected: string,
  apps: Array<*>,
  nodeInfo?: string
};

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
    };
  }

  toggleCollapse = () => {
    this.setState(({ collapse }) => ({
      collapse: !collapse,
      height: collapse ? [50, 0] : [0, 50]
    }));
  };

  selectAll = () => {
    this.props.selectApps(this.props.apps);
  };

  clearAll = () => {
    this.props.clearApps(this.props.apps);
  };

  selectNode = (id: string) => {
    send('epl_st_EPL', id);
  };

  handleAppClick = (id: string) => {
    const app = this.props.apps.find(app => app.id === id);
    if (app.selected) {
      this.props.clearApps([app]);
    } else {
      this.props.selectApps([app]);
    }
  };

  render() {
    return (
      <div className="side-panel">

        <div className="head" onClick={this.toggleCollapse}>
          <h4
            className="text-center"
            style={{
              color: COLORS[this.props.selected] || 'inherit'
            }}
          >
            {this.props.selected}
          </h4>
          <i className={`fa fa-angle-${this.state.collapse ? 'down' : 'up'}`} />
        </div>

        <Motion
          defaultStyle={{ height: this.state.height[0] }}
          style={{ height: spring(this.state.height[1]) }}
          children={({ height }) => (
            <div className="side-content">
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
                  {this.props.apps.map(
                    ({ name, selected, id }) =>
                      (id
                        ? <ListGroupItem
                            key={name}
                            className="application-link"
                          >
                            <input
                              type="checkbox"
                              checked={selected}
                              onChange={() => this.handleAppClick(id)}
                            />
                            <a
                              style={{ marginLeft: '5px' }}
                              onClick={() => this.selectNode(id)}
                            >
                              {name}
                            </a>
                          </ListGroupItem>
                        : <ListGroupItem
                            key={name}
                            className="application-link"
                          >
                            <span style={{ marginLeft: '17px' }}>
                              {name}
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

import * as actions from '../actions';

export default connect(state => {
  const nodeInfo = state.eplSupTree.nodeInfo;
  return {
    nodeInfo,
    selected: nodeInfo ? nodeInfo.id : 'Applications',
    apps: state.eplSupTree.apps
  };
}, actions)(SidePanel);
