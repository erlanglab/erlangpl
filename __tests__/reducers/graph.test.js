import { UPDATE_GRAPH_DATA, UPDATE_GRAPH_VIEW } from '../../src/actions/graph';
import reducer, { INITIAL_STATE } from '../../src/reducers/graph';

describe('trafficData reducer', () => {
  it('should handle initial state', () => {
    expect(reducer(undefined, {})).toEqual(INITIAL_STATE);
  });

  it('should handle UPDATE_TRAFFIC_DATA', () => {
    const data = {
      name: 'edge',
      renderer: 'global',
      nodes: [
        {
          renderer: 'region',
          name: 'INTERNET',
          class: 'normal',
        },
        {
          renderer: 'region',
          name: 'us-east-1',
          maxVolume: 50000,
          class: 'normal',
          updated: 1466838546805,
          nodes: [
            {
              name: 'INTERNET',
              renderer: 'focused',
              class: 'normal',
            },
            {
              name: 'proxy-prod',
              renderer: 'focused',
              class: 'normal',
            },
          ],
          connections: [
            {
              source: 'INTERNET',
              target: 'proxy-prod',
              metrics: {
                danger: 116.524,
                normal: 15598.906,
              },
              class: 'normal',
            },
          ],
        },
      ],
      connections: [
        {
          source: 'INTERNET',
          target: 'us-east-1',
          metrics: {
            normal: 26037.626,
            danger: 92.37,
          },
          notices: [],
          class: 'normal',
        },
      ],
    };

    expect(
      reducer(INITIAL_STATE, {
        type: UPDATE_GRAPH_DATA,
        data,
      }),
    ).toEqual({
      ...INITIAL_STATE,
      data,
    });
  });

  it('should handle UPDATE_GRAPH_VIEW', () => {});
});
