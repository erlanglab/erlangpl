// @flow
import React, { Component } from 'react';
import { connect } from 'react-redux';
import hljs from 'highlightjs';

import { send } from '../../../sockets';
import * as actions from '../actions';
import 'highlightjs/styles/default.css';
import 'highlightjs/styles/atom-one-dark.css';
import './style.css';

class Component_ extends Component {
  code: any;

  constructor(props) {
    super(props);
    this.state = {
      add: '',
      search: ''
    };
  }

  changeByArrows = e => {
    e.preventDefault();
    const current = this.props.timelines.find(t => t.pid === this.props.pid);
    const currentTimeline = current ? current.timeline : [];
    if (e.which === 40 && this.props.msg < currentTimeline.length - 1) {
      //down
      this.props.setCurrentMsg(this.props.msg + 1);
    } else if (e.which === 38 && this.props.msg > 0) {
      //up
      this.props.setCurrentMsg(this.props.msg - 1);
    }
  };

  componentDidMount() {
    window.addEventListener('keyup', this.changeByArrows, false);
  }

  componentWillUnmount() {
    window.removeEventListener('keyup', this.changeByArrows, false);
  }

  // this can cause perf issues
  componentDidUpdate() {
    if (this.code) {
      hljs.highlightBlock(this.code);
    }
  }

  addPid(event: any) {
    if (event.which === 13) {
      send('epl_timeline_EPL', this.state.add);
      this.setState({ add: '' });
    }
  }

  render() {
    const current = this.props.timelines.find(t => t.pid === this.props.pid);
    const currentTimeline = current ? current.timeline : [];
    const currentState = currentTimeline[this.props.msg];
    return (
      <div className="Timeline">
        <div className="pane left-panel">
          <input
            placeholder="PID to track"
            value={this.state.add}
            onKeyDown={this.addPid.bind(this)}
            onChange={event => this.setState({ add: event.target.value })}
          />
          <input
            placeholder="Search"
            value={this.state.search}
            onKeyDown={this.addPid.bind(this)}
            onChange={event => this.setState({ search: event.target.value })}
          />

          {this.props.timelines.map(({ pid, timeline }) => (
            <div
              className={`pid ${pid === this.props.pid ? 'active' : ''}`}
              onClick={() => this.props.setCurrentPid(pid)}
              key={pid}
            >
              {pid}
              {' '}
              |
              {' '}
              {timeline.length < 1000 ? timeline.length : '999+'}
            </div>
          ))}
        </div>
        <div className="pane right-panel">
          <div className="messages">
            {currentTimeline.map((t, index) => (
              <div
                key={index}
                className={`message ${index === this.props.msg ? 'active' : ''}`}
                onClick={() => this.props.setCurrentMsg(index)}
              >
                <span style={{ fontStyle: 'bold' }}>
                  {currentTimeline.length - index}
                </span>
                <br />
                {t.message}
              </div>
            ))}
          </div>
          <div className="state">
            <pre style={{ textAlign: currentState ? 'left' : 'center' }}>
              <code className="erlang" ref={node => this.code = node}>
                {currentState ? currentState.state : 'No changes tracked'}
              </code>
            </pre>
          </div>
        </div>
      </div>
    );
  }
}

export default connect(
  state => ({
    timelines: state.eplTimeline.get('timelines'),
    pid: state.eplTimeline.get('pid'),
    msg: state.eplTimeline.get('msg')
  }),
  {
    setCurrentPid: actions.setCurrentPid,
    setCurrentMsg: actions.setCurrentMsg
  }
)(Component_);
