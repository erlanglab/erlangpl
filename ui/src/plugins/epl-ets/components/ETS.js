// @flow
import React, { Component } from 'react';
import { connect } from 'react-redux';
import Vizceral from 'vizceral-react';
import { push } from 'react-router-redux';
import { send } from '../../../sockets';

import 'vizceral-react/dist/vizceral.css';
import './ETS.css';

import PluginWrapper from '../../../core/components/PluginWrapper';
import ETSTools from './ETSTools';
import TableView from './TableView';
import * as actions from '../actions';

class ETS extends Component {
  state: {
    start: number,
    end: number,
    height: number,
    width: number,
    graph: boolean,
    showTab: boolean,
    clickedNode: any,
    view: any,
    definitions: object,
    vizStyle: object
  };

  vizceral: any;

  constructor(props) {
    super(props);
    this.state = {
      height: 0,
      width: 0,
      start: 0,
      end: 0,
      graph: false,
      showTab: false,
      clickedNode: false,
      view: [],
      definitions: this.clusterViewDefinitions(),
      vizStyle: {}
    };
  }

  resize = () => {
    if (this.vizceral) {
      const { clientWidth, clientHeight } = this.vizceral.refs.vizCanvas;
      this.setState({ width: clientWidth, height: clientHeight });
    }
  };

  componentDidMount() {
    this.resize();
    window.addEventListener('resize', this.resize);
  }

  componentWillUnmount() {
    window.removeEventListener('resize', this.resize);
  }

  componentWillReceiveProps({ view }) {
    this.setState({ view: view });
  }

  handleTabClick = (tab_id: any) => {
    this.setState({ view: [this.state.clickedNode, tab_id] });
  };

  enableNodeTracing = node => {
    if (node) {
      send('epl_ets_EPL', JSON.stringify({ enable: true, node: node }));
    }
  };

  enableTabTracing = (node, tab) => {
    if (node && tab) {
      send(
        'epl_ets_EPL',
        JSON.stringify({
          enable: true,
          node: node,
          table: tab
        })
      );
    }
  };

  disableTracing = node => {
    if (node) {
      send('epl_ets_EPL', JSON.stringify({ enable: false, node: node }));
    }
  };

  clusterViewDefinitions = () => {
    return {
      detailedNode: {
        volume: {
          default: {
            top: {
              header: 'ETS Count',
              data: 'etsMetrics.all',
              format: '0'
            },
            bottom: {
              header: 'ETS memory usage',
              data: 'etsMetrics.memUsage',
              format: '0.00%'
            },
            donut: {
              data: 'etsMetrics.pieChart'
            }
          },
          entry: {
            top: {
              header: 'ETS Count',
              data: 'etsMetrics.all',
              format: '0'
            }
          }
        }
      }
    };
  };

  detailsViewDefinitions = () => {
    return {
      detailedNode: {
        volume: {
          default: {
            top: {
              header: 'RPS',
              data: 'data.volumePercent',
              format: '0.0%'
            },
            bottom: {
              header: 'ERROR RATE',
              data: 'data.classPercents.danger',
              format: '0.00%'
            },
            donut: {
              data: 'data.globalClassPercents',
              indices: [
                { key: 'danger' },
                { key: 'warning' },
                { key: 'normal', class: 'normalDonut' }
              ]
            },
            arc: {}
          },
          focused: {
            top: {
              header: 'RPS',
              data: 'data.volume',
              format: '0,0'
            },
            donut: {
              data: 'data.classPercents'
            }
          },
          entry: {
            top: {
              header: 'TOTAL RPS',
              data: 'data.volume',
              format: '0,0'
            }
          }
        }
      }
    };
  };

  handleViewChange = (data: any) => {
    const sidePanelLevel = 3;
    let anim;
    const { view, graph } = data;
    if (
      view.length > sidePanelLevel - 1 &&
      this.props.view.length === sidePanelLevel - 1
    ) {
      //open
      anim = { end: 1, start: 0 };
    } else if (this.props.view.length > sidePanelLevel - 1) {
      //stay open
      anim = { end: 1, start: 1 };
    } else {
      //close
      anim = { end: 0, start: 1 };
    }
    switch (view.length) {
      case 0:
        this.disableTracing(this.state.clickedNode);
        this.setState({
          clickedNode: false,
          showTab: false,
          definitions: this.clusterViewDefinitions(),
          vizStyle: {}
        });
        break;
      case 1:
        this.disableTracing(view[0]);
        this.enableNodeTracing(view[0]);
        this.setState({
          clickedNode: view[0],
          showTab: true,
          vizStyle: { display: 'none' }
        });
        break;
      case 2:
        this.disableTracing(view[0]);
        this.enableTabTracing(view[0], view[1]);
        this.setState({
          clickedNode: view[0],
          showTab: false,
          definitions: this.detailsViewDefinitions(),
          vizStyle: {}
        });
        break;
      default:
        break;
    }

    this.setState({
      end: anim.end,
      start: anim.start,
      graph: graph !== null
    });

    const path = `/ets${view.length > 0 ? '/' : ''}${view.join('/')}`;
    this.props.push(path);
  };

  render() {
    return (
      <div className="Traffic">
        <ETSTools className="Traffic-tools" />
        <PluginWrapper
          // NOTE: to hide side panel simply pass null/undefined or false
          // instead of component as sidePanel property
          sidePanel={false}
          loading={false}
          className="Traffic-container"
          loaderText="Gathering ETS cluster data"
        >
          {this.state.showTab
            ? <TableView
                table={{
                  tabs: this.props.data.etsNodeTabs,
                  node: this.state.clickedNode
                }}
                tableClicked={tab_id => {
                  this.handleTabClick(tab_id);
                }}
              />
            : null}
          <div
            className="viz-wrapper"
            style={{
              ...{ height: '100%' },
              ...this.state.vizStyle
            }}
          >
            <Vizceral
              ref={node => (this.vizceral = node)}
              traffic={this.props.data}
              view={this.state.view}
              viewChanged={this.handleViewChange}
              showLabels={true}
              match={this.props.search}
              allowDraggingOfNodes={true}
              targetFramerate={25}
              definitions={this.state.definitions}
            />
          </div>
        </PluginWrapper>
      </div>
    );
  }
}

export default connect(
  state => {
    return {
      search: state.eplETS.search,
      nodeInfo: state.eplSupTree.nodeInfo,
      data: state.eplETS.data,
      view: state.eplETS.view
    };
  },
  {
    push,
    updateETSData: actions.updateETSData
  }
)(ETS);
