// @flow
import React, { Component } from 'react';
import { connect } from 'react-redux';
// import hljs from 'highlightjs';
import { Link } from 'react-router-dom';
import AutoSizer from 'react-virtualized/dist/commonjs/AutoSizer';
import List from 'react-virtualized/dist/commonjs/List';

import { send } from '../../../sockets';
import * as actions from '../actions';
import 'highlightjs/styles/default.css';
import 'highlightjs/styles/atom-one-dark.css';
import './style.css';

class Component_ extends Component {
  code: any;
  state: {
    add: string
  };

  constructor(props) {
    super(props);
    this.state = {
      add: ''
    };
  }

  changeByArrows(e: any) {
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
    document.body &&
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
    document.body &&
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
      send('epl_timeline_EPL', JSON.stringify(['add', this.state.add.trim()]));
      this.props.pushTimelinePid(this.state.add);
      this.setState({ add: '' });
    }
  }

  handlePidClick(pid: string) {
    this.props.setCurrentPid(pid);
  }

  handlePidRemove(pid: string) {
    send('epl_timeline_EPL', JSON.stringify(['remove', pid]));
    this.props.removePid(pid);
  }

  render() {
    const current = this.props.timelines.find(t => t.pid === this.props.pid);
    const currentTimeline = current ? current.timeline : [];
    const currentState = currentTimeline[this.props.msg];
    return (
      <div className="Timeline">
        <ul className="Dashboard-navigation nav nav-tabs">
          <li className="nav-item">
            <input
              placeholder="Pid to add"
              value={this.state.add}
              onKeyDown={this.addPid.bind(this)}
              onChange={event => this.setState({ add: event.target.value })}
            />
          </li>
          {this.props.timelines.map(({ pid, timeline }) => (
            <li
              key={pid}
              className={`nav-item ${pid === this.props.pid ? 'Dashboard-active' : ''}`}
              onClick={this.handlePidClick.bind(this, pid)}
            >
              <Link to={`/timeline/${pid}`}>
                <span className="badge">
                  {timeline.length < 1000 ? timeline.length : '999+'}
                </span>
                {pid}
                <i
                  className="fa fa-times"
                  onClick={() => this.handlePidRemove(pid)}
                />
              </Link>
            </li>
          ))}
        </ul>

        {currentState
          ? <div className="pane">
              <div className="messages">
                <AutoSizer>
                  {({ height, width }) => (
                    <List
                      scrollToIndex={this.props.msg}
                      width={width}
                      height={height}
                      rowCount={currentTimeline.length}
                      rowHeight={35}
                      rowRenderer={({ index, key, style }) => {
                        const t = currentTimeline[index];

                        return (
                          <div
                            style={style}
                            key={key}
                            className={`message ${index === this.props.msg ? 'active' : ''}`}
                            onClick={() => this.props.setCurrentMsg(index)}
                          >
                            <span className="content">{t.message}</span>
                            <span className="index">
                              {index}
                            </span>
                          </div>
                        );
                      }}
                    />
                  )}
                </AutoSizer>
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
    removePid: actions.removePid,
    pushTimelinePid,
    setCurrentPid: actions.setCurrentPid,
    setCurrentMsg: actions.setCurrentMsg
  }
)(Component_);
