// @flow
import React, { Component } from 'react';
import { connect } from 'react-redux';

import './TrafficTools.css';

class TrafficTools extends Component {
  handleClick(index: number) {
    const view = this.props.view.slice(0, index);
    this.props.setView(view);
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
  state => ({ search: state.eplVizceral.search, view: state.eplVizceral.view }),
  {
    setView: actions.updateTrafficView,
    setSearch: actions.updateTrafficSearch
  }
)(TrafficTools);
