// @flow
import React, { Component } from 'react';
import { Grid } from 'react-bootstrap';
import { Link, Route, Redirect } from 'react-router-dom';

import SystemInfo from './SystemInfo';
import './Index.css';

class Index extends Component {
  state: { tab: number };
  constructor() {
    super();
    this.state = { tab: 0 };
  }

  render() {
    const active = (index: number) => {
      return this.state.tab === index ? 'Index-active' : '';
    };

    const navItems = [{ text: 'System info', to: '/home/system' }];

    return (
      <div className="Index">
        <ul className="Index-navigation nav nav-tabs">
          {navItems.map((link, i) => (
            <li
              key={i}
              className={`nav-item ${active(i)}`}
              onClick={() => this.setState({ tab: i })}
            >
              <Link to={link.to}>
                {link.text}
              </Link>
            </li>
          ))}
        </ul>
        <Grid className="Index-grid" fluid>
          <Route path="/home/system" component={SystemInfo} />
          <Redirect to="/home/system" component={SystemInfo} />
        </Grid>
      </div>
    );
  }
}
export default Index;
