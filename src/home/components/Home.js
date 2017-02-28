// @flow
import React, { Component } from 'react';
import { Grid } from 'react-bootstrap';
import { Link, Route, Redirect } from 'react-router-dom';

import SystemInfo from './SystemInfo';
import SystemOverview from './SystemOverview';
import './Home.css';

class Home extends Component {
  state: { tab: number };
  constructor() {
    super();
    this.state = { tab: 0 };
  }

  render() {
    const active = (index: number) => {
      return this.state.tab === index ? 'Home-active' : '';
    };

    const navItems = [
      { text: 'System overview', to: '/home/overview' },
      { text: 'Basic system info', to: '/home/system' }
    ];

    return (
      <div className="Home">
        <ul className="Home-navigation nav nav-tabs">
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
        <Grid className="Home-grid" fluid>
          <Route path="/home/system" component={SystemInfo} />
          <Route path="/home/overview" component={SystemOverview} />
          <Route
            exact={true}
            path="/home"
            render={() => <Redirect to={navItems[0].to} />}
          />
        </Grid>
      </div>
    );
  }
}
export default Home;
