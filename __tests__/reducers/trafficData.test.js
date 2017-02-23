import { ACTIONS } from '../../src/actions/trafficData';
import reducer from '../../src/reducers/trafficData';

describe('trafficData reducer', () => {
  it('should handle initial state', () => {
    expect(reducer(undefined, {})).toEqual({
      nodes: [],
      connections: [],
    });
  });

  it('should handle UPDATE_TRAFFIC', () => {
    const state = {
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

    const expectedState = {
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
          name: 'us-east-2',
          maxVolume: 10000,
          class: 'normal',
          updated: 1466838546805,
          nodes: [
            {
              name: 'INTERNET',
              renderer: 'focused',
              class: 'normal',
            },
            {
              name: 'proxy-log',
              renderer: 'focused',
              class: 'normal',
            },
          ],
          connections: [
            {
              source: 'INTERNET',
              target: 'proxy-log',
              metrics: {
                danger: 126.524,
                normal: 185.906,
              },
              class: 'normal',
            },
          ],
        },
      ],
      connections: [
        {
          source: 'INTERNET',
          target: 'us-east-2',
          metrics: {
            normal: 12037.626,
            danger: 2.37,
          },
          notices: [],
          class: 'normal',
        },
      ],
    };

    expect(
      reducer({ nodes: [], connections: [] }, {
        type: ACTIONS.UPDATE_TRAFFIC,
        data: state,
      }),
    ).toEqual(state);

    expect(
      reducer(state, { type: ACTIONS.UPDATE_TRAFFIC, data: expectedState }),
    ).toEqual(expectedState);
  });
});
