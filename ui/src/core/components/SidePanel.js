// @flow
import React, { Component } from 'react';

import { SidePanelContainer } from './styled';

class SidePanel extends Component {
  render() {
    return (
      <SidePanelContainer>
        <div>
          bello world
        </div>
        {this.props.children}
      </SidePanelContainer>
    );
  }
}

export default SidePanel;
