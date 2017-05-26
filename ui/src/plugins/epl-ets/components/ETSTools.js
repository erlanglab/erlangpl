// @flow
import React, { Component } from 'react';
import { connect } from 'react-redux';
import { push } from 'react-router-redux';

import './ETSTools.css';

class ETSTools extends Component {
  handleClick(index: number) {
    const view = this.props.view.slice(0, index);
    const path = `/ets${view.length > 0 ? '/' : ''}${view.join('/')}`;
    this.props.push(path);
  }

  render() {
    const view = ['cluster', ...this.props.view];

    return (
      <div className={`${this.props.className} TrafficTools`}>
        {view.map((v, index) => (
          <a
            key={index}
            className="TrafficTools-route"
            onClick={() => this.handleClick(index)}
          >
            {`${v}/`}
          </a>
        ))}
        <input
          value={this.props.search}
          onChange={event => this.props.setSearch(event.target.value)}
        />
      </div>
    );
  }
}

import * as actions from '../actions';

export default connect(
  state => ({ search: state.eplETS.search, view: state.eplETS.view }),
  {
    push,
    setView: actions.updateTrafficView,
    setSearch: actions.updateTrafficSearch
  }
)(ETSTools);
