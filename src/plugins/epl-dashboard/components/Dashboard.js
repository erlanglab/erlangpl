// @flow
import React, { Component } from 'react';
import { Grid } from 'react-bootstrap';
import { Link, Route, Redirect } from 'react-router-dom';

import SystemInfo from './SystemInfo';
import SystemOverview from './SystemOverview';

import './Dashboard.css';

class Dashboard extends Component {
  state: { tab: number };
  constructor() {
    super();
    this.state = { tab: 0 };
  }

  render() {
    const active = (index: number) => {
      return this.state.tab === index ? 'Dashboard-active' : '';
    };

    const navItems = [
      { text: 'System overview', to: '/dashboard/overview' },
      { text: 'Basic system info', to: '/dashboard/system' }
    ];

    return (
      <div className="Dashboard">
        <ul className="Dashboard-navigation nav nav-tabs">
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
        <Grid className="Dashboard-grid" fluid>
          <Route path="/dashboard/system" component={SystemInfo} />
          <Route path="/dashboard/overview" component={SystemOverview} />
          <Route
            exact={true}
            path="/dashboard"
            render={() => <Redirect to={navItems[0].to} />}
          />
        </Grid>
      </div>
    );
  }
}

export default Dashboard;
