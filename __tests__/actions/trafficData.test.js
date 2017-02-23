import * as actions from '../../src/actions/trafficData';

describe('should handle TRAFFIC_DATA action creators', () => {
  it('should handle updateTraffic action creator', () => {
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

    expect(actions.updateTraffic(data)).toEqual({
      type: 'UPDATE_TRAFFIC',
      data,
    });

    expect(actions.updateTraffic(data)).toEqual({
      type: actions.ACTIONS.UPDATE_TRAFFIC,
      data,
    });
  });
});
