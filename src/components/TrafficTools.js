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
    const view = ['global', ...this.props.view];

    return (
      <div className={`${this.props.className} TrafficTools`}>
        {view.map((v, index) => (
          <button key={index} onClick={() => this.handleClick(index)}>
            {v}
          </button>
        ))}
      </div>
    );
  }
}

import { updateTrafficView } from '../actions/traffic';

export default connect(state => ({ view: state.traffic.view }), {
  setView: updateTrafficView,
})(TrafficTools);
