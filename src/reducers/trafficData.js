import _ from 'lodash';

import { ACTIONS } from '../actions/trafficData';

export const INITIAL_TRAFFIC = {
  nodes: [],
  connections: []
};

const trafficData = (state = INITIAL_TRAFFIC, action) => {
  if(action.type === ACTIONS.UPDATE_TRAFFIC)
    return { ...action.data };
    // const newTraffic = action.data;
    // const updatedTraffic = {
    //   name: newTraffic.name,
    //   renderer: newTraffic.renderer,
    //   nodes: [...state.nodes],
    //   connections: [...state.connections]
    // };

    // /* add if node doesn't exists or update if exists */
    // newTraffic.nodes.forEach(node => {
    //   const existingNodeIndex =
    //           _.findIndex(updatedTraffic.nodes, { name: node.name });

    //   if (existingNodeIndex !== -1) {
    //     if (node.nodes && node.nodes.length > 0) {
    //       node.updated = node.updated || updatedTraffic.nodes[existingNodeIndex].updated;
    //       updatedTraffic.nodes[existingNodeIndex] = node;
    //     }
    //   } else {
    //     updatedTraffic.nodes.push(node);
    //   }
    // });

    // /* add if connection doesn't exists or update if exists */
    // newTraffic.connections.forEach(connection => {
    //   const existingConnectionIndex =
    //           _.findIndex(updatedTraffic.connections, {
    //             source: connection.source,
    //             target: connection.target
    //           });

    //   if (existingConnectionIndex !== -1) {
    //     updatedTraffic.connections[existingConnectionIndex] = connection;
    //   } else {
    //     updatedTraffic.connections.push(connection);
    //   }
    // });

    // // const regionUpdateStatus =
    // //         _.map(_.filter(updatedTraffic.nodes, n => n.name !== 'INTERNET'), (node) => {
    // //           const updated = node.updated;
    // //           return { region: node.name, updated: updated };
    // //         });

    // return updatedTraffic;

  return state;
};

export default trafficData;
