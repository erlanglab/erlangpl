// @flow
import React, { Component } from 'react';
import { connect } from 'react-redux';

import './GraphTools.css';

class GraphTools extends Component {
  handleClick(index: number) {
    const view = this.props.view.slice(0, index);
    this.props.setView(view);
  }

  render() {
    const view = ['global', ...this.props.view];

    return (
      <div className={`${this.props.className} GraphTools`}>
        {view.map((v, index) => (
          <button key={index} onClick={() => this.handleClick(index)}>
            {v}
          </button>
        ))}
      </div>
    );
  }
}

import { updateGraphView } from '../actions/graph';

export default connect(state => ({ view: state.graph.view }), {
  setView: updateGraphView,
})(GraphTools);
