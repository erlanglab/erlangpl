// @flow
import React, { Component } from 'react';

import Loader from './Loader';
import { Container, Content } from './styled';

// const SidePanel = () => null;

type Props = {
  className?: string,
  loading?: boolean,
  loaderText?: string,
  children?: React$Element<*>
};

class PluginWrapper extends Component {
  props: Props;

  static defaultProps = {
    className: '',
    loaderText: 'Loading...',
    loading: false
  };

  render() {
    if (this.props.loading) return <Loader text={this.props.loaderText} />;
    return (
      <Container>
        <Content className={this.props.className}>
          {this.props.children}
        </Content>
      </Container>
    );
  }
}

export default PluginWrapper;
