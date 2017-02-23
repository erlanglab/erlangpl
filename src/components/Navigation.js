// @flow
import React from 'react';
import Link from 'react-router-dom/Link';

import logo from '../images/erlangpl_header.png';

const tabs = [
  { path: '/', text: 'Dashboard' },
  { path: '/page', text: 'Page' },
  { path: '/about', text: 'About' },
];

const Navigation = () => {
  return (
    <div style={{ marginBottom: '50px' }}>
      <img src={logo} alt="Erlang Performance Lab" />
      <div className="tabs is-boxed">
        <ul>
          {tabs.map((tab, i) => (
            <li key={i}>
              <Link to={tab.path}>
                {tab.text}
              </Link>
            </li>
          ))}
        </ul>
      </div>
    </div>
  );
};

export default Navigation;
