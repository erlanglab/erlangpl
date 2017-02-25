// @flow
import React from 'react';
import { Link, Route } from 'react-router-dom/';

import './Navigation.css';

const tabs = [
  { path: '/home', icon: 'desktop' },
  { path: '/messages', icon: 'comments' },
  { path: '/traffic', icon: 'share-alt' },
  { path: '/about', icon: 'question' },
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
