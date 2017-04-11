// @flow
import React, { Component } from 'react';

import sample from './sample.json';
import 'd3-flame-graph/src/d3.flameGraph.css';

import 'd3';
import tip from 'd3-tip';
import flameGraph from 'd3-flame-graph/src/d3.flameGraph';
window.d3.flameGraph = flameGraph;
window.d3.tip = tip;
const d3 = window.d3;

class BottomPanel extends Component {
  div: any;
  componentDidMount() {
    console.log(this.props.height);
    var flamegraph = d3
      .flameGraph()
      .width(this.div.clientWidth)
      .height(this.props.height);
    d3.select('#inner').datum(sample).call(flamegraph);
  }

  render() {
    return (
      <div
        ref={node => this.div = node}
        className="Traffic-panel"
        style={{
          ...this.props.style
        }}
      >
        <div id="inner" />
      </div>
    );
  }
}

export default BottomPanel;
