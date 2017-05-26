// @flow
import React, { Component } from 'react';
import { ReflexContainer, ReflexSplitter, ReflexElement } from 'react-reflex';

import 'react-reflex/styles.css';

import Loader from './Loader';
import SidePanel from './SidePanel';
import { Container, Content } from './styled';

type Props = {
  className?: string,
  loading?: boolean,
  loaderText?: string,
  children?: React$Element<*>
};

class PluginWrapper extends Component {
  props: Props;

  state = { panel: false };

  static defaultProps = {
    className: '',
    loaderText: 'Loading...',
    loading: false
  };

  render() {
    if (this.props.loading) return <Loader text={this.props.loaderText} />;

    const { panel } = this.state;

    return (
      <Container>
        <ReflexContainer orientation="vertical">
          <ReflexElement flex={panel ? 0.75 : 1}>
            <Content className={this.props.className}>
              {this.props.children}
            </Content>
          </ReflexElement>

          {panel &&
            <ReflexSplitter
              style={{
                borderColor: '#181a1f'
              }}
            />}

          {panel &&
            <ReflexElement flex={0.25}>
              <SidePanel />
            </ReflexElement>}

        </ReflexContainer>
      </Container>
    );
  }
}

export default PluginWrapper;
