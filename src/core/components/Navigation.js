// @flow
import React from 'react';
import { Link, Route } from 'react-router-dom/';

import './Navigation.css';

const NavigationLink = ({ to, icon }) => (
  <Route
    path={to}
    exact={false}
    children={({ match }) => (
      <div className={`item ${match ? 'active' : ''}`}>
        <Link to={to}>
          {icon}
        </Link>
      </div>
    )}
  />
);

type Props = {
  tabs: Array<{ path: string, icon: string }>
};

const Navigation = ({ tabs }: Props) => {
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
