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
  scroll: any;

  constructor(props) {
    super(props);
    this.state = {
      add: '',
      search: ''
    };
  }

  changeByArrows(e) {
    const current = this.props.timelines.find(t => t.pid === this.props.pid);
    const currentTimeline = current ? current.timeline : [];
    if (e.which === 40 && this.props.msg < currentTimeline.length - 1) {
      //down
      e.preventDefault();
      this.props.setCurrentMsg(this.props.msg + 1);
    } else if (e.which === 38 && this.props.msg > 0) {
      //up
      e.preventDefault();
      this.props.setCurrentMsg(this.props.msg - 1);
    }
  }

  componentDidMount() {
    document.body.addEventListener(
      'keydown',
      this.changeByArrows.bind(this),
      false
    );

    const pid = this.props.match.params.pid;
    if (pid) return this.props.setCurrentPid(pid);

    if (this.props.pid === null && this.props.timelines.length)
      return this.props.setCurrentPid(this.props.timelines[0].pid);
  }

  componentWillUnmount() {
    document.body.removeEventListener(
      'keydown',
      this.changeByArrows.bind(this),
      false
    );
  }

  // this can cause perf issues
  componentDidUpdate() {
    // this.code && hljs.highlightBlock(this.code);
  }

  addPid(event: any) {
    if (event.which === 13) {
      send('epl_timeline_EPL', this.state.add);
      this.props.pushTimelinePid(this.state.add);
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
          <li className="nav-item">
            <input
              placeholder="PID to add"
              value={this.state.add}
              onKeyDown={this.addPid.bind(this)}
              onChange={event => this.setState({ add: event.target.value })}
            />
          </li>
        </ul>

        {currentState
          ? <div className="pane">
              <div className="messages" ref={node => this.scroll = node}>
                {currentTimeline.map((t, index) => (
                  <div
                    key={index}
                    className={`message ${index === this.props.msg ? 'active' : ''}`}
                    onClick={() => this.props.setCurrentMsg(index)}
                  >
                    <span className="content">{t.message}</span>
                    <span className="index" style={{ fontStyle: 'bold' }}>
                      {index}
                    </span>
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

import { pushTimelinePid } from '../../epl-sup-tree/actions';

export default connect(
  state => ({
    timelines: state.eplTimeline.get('timelines'),
    pid: state.eplTimeline.get('pid'),
    msg: state.eplTimeline.get('msg')
  }),
  {
    pushTimelinePid,
    setCurrentPid: actions.setCurrentPid,
    setCurrentMsg: actions.setCurrentMsg
  }
)(Component_);
