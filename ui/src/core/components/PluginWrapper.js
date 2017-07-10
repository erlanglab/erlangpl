// @flow
import React, { Component } from 'react';
import { ReflexContainer, ReflexSplitter, ReflexElement } from 'react-reflex';

import 'react-reflex/styles.css';

import Loader from './Loader';
import SidePanel from './SidePanel';
import { Container, Content } from './styled';

type Props = {
  sidePanel?: React$Element<*>,
  className?: string,
  loading?: boolean,
  loaderText?: string,
  children?: React$Element<*>
};

class PluginWrapper extends Component {
  state: { panel: boolean };

  constructor(props: Props) {
    super(props);
    this.state = {
      panel: props.sidePanel ? true : false
    };
  }

  static defaultProps = {
    className: '',
    loaderText: 'Loading...',
    loading: false
  };

  render() {
    if (this.props.loading) {
      return <Loader text={this.props.loaderText} />;
    }

    const content = (
      <Content>
        {this.props.children}
      </Content>
    );

    // NOTE: when no side panel we don't need Relfex
    if (!this.state.panel) {
      return (
        <Container>
          {content}
        </Container>
      );
    }

    return (
      <Container className={this.props.className}>
        <ReflexContainer orientation="vertical">
          <ReflexElement flex={0.75}>
            {content}
          </ReflexElement>

          <ReflexSplitter
            style={{
              borderColor: '#181a1f'
            }}
          />

          <ReflexElement flex={0.25}>
            <SidePanel>
              {this.props.sidePanel}
            </SidePanel>
          </ReflexElement>

        </ReflexContainer>
      </Container>
    );
  }
}

export default PluginWrapper;
