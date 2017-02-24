// @flow
import React from 'react';
import { Link, Route } from 'react-router-dom/';

import logo from '../images/erlanglab_logo_alpha_250.png';
import './Navigation.css';

const tabs = [
  { path: '/graph', icon: 'share-alt' },
  { path: '/about', icon: 'info' },
];

const NavigationLink = ({ to, icon }) => (
  <Route
    path={to}
    exact={true}
    children={({ match }) => (
      <div className={`item ${match ? 'active' : ''}`}>
        <Link to={to}>
          {icon}
        </Link>
      </div>
    )}
  />
);

const Navigation = () => {
  return (
    <div className="Navigation">
      <NavigationLink
        to="/"
        icon={<img src={logo} style={{ height: '50px', width: '50px' }} />}
      />
      {tabs.map((tab, i) => (
        <NavigationLink
          key={i}
          to={tab.path}
          icon={<i className={`fa fa-2x fa-${tab.icon}`} />}
        />
      ))}
    </div>
  );
};

export default Navigation;
