// @flow
import React, { Component } from 'react';

import { SidePanelContainer } from './styled';

class SidePanel extends Component {
  render() {
    return (
      <SidePanelContainer>
        {this.props.children}
      </SidePanelContainer>
    );
  }
}

export default SidePanel;
