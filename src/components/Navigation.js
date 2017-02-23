// @flow
import React from 'react';
import { Link, Route } from 'react-router-dom/';

import './Navigation.css';

const tabs = [
  { path: '/', icon: 'user-circle' },
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
          <i className={`fa fa-2x fa-${icon}`} />
        </Link>
      </div>
    )}
  />
);

const Navigation = () => {
  return (
    <div className="Navigation">
      {tabs.map((tab, i) => (
        <NavigationLink key={i} to={tab.path} icon={tab.icon} />
      ))}
    </div>
  );
};

export default Navigation;
