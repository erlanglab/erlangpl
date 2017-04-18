// @flow
import React, { Component } from 'react';
import { connect } from 'react-redux';
import hljs from 'highlightjs';
import { Link } from 'react-router-dom';

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
      return this.props.setCurrentMsg(this.props.msg + 1);
    } else if (e.which === 38 && this.props.msg > 0) {
      //up
      return this.props.setCurrentMsg(this.props.msg - 1);
    }
    return true;
  };

  componentDidMount() {
    this.props.pid === null &&
      this.props.timelines.length &&
      this.props.setCurrentPid(this.props.timelines[0].pid);

    window.addEventListener('keyup', this.changeByArrows, false);
  }

  componentWillUnmount() {
    window.removeEventListener('keyup', this.changeByArrows, false);
  }

  componentWillReceiveProps(nextProps) {
    nextProps.pid === null &&
      nextProps.timelines.length &&
      nextProps.setCurrentPid(nextProps.timelines[0].pid);
  }

  // this can cause perf issues
  componentDidUpdate() {
    if (this.code) {
      // hljs.highlightBlock(this.code);
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
        <ul className="Dashboard-navigation nav nav-tabs">
          {this.props.timelines.map(({ pid, timeline }) => (
            <li
              key={pid}
              className={`nav-item ${pid === this.props.pid ? 'Dashboard-active' : ''}`}
              onClick={() => this.props.setCurrentPid(pid)}
            >
              <Link to={`/timeline/${pid}`}>
                {pid}
                <span
                  className="badge"
                  style={{ backgroundColor: '#8fbf47', marginLeft: '5px' }}
                >
                  {timeline.length < 1000 ? timeline.length : '999+'}
                </span>
              </Link>
            </li>
          ))}
        </ul>

        {currentState
          ? <div className="pane">
              <div className="messages">
                {currentTimeline.map((t, index) => (
                  <div
                    key={index}
                    className={`message ${index === this.props.msg ? 'active' : ''}`}
                    onClick={() => this.props.setCurrentMsg(index)}
                  >
                    <span style={{ fontStyle: 'bold' }}>
                      {index}
                    </span>
                    <br />
                    {t.message}
                  </div>
                ))}
              </div>
              <div className="state">
                <pre style={{ textAlign: currentState ? 'left' : 'center' }}>
                  <code className="erlang" ref={node => this.code = node}>
                    {currentState.state}
                  </code>
                </pre>
              </div>
            </div>
          : <div
              className="pane"
              style={{ textAlign: 'center', marginTop: '20px' }}
            >
              No changes tracked
            </div>}
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
