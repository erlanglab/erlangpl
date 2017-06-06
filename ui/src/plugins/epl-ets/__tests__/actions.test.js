import * as t from '../actionTypes/';
import * as actions from '../actions';

describe('graph action creators', () => {
  it('should handle updateETSData action creator', () => {
    const data = {
      name: 'edge',
      renderer: 'global',
      nodes: [
        {
          renderer: 'region',
          name: 'INTERNET',
          class: 'normal'
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
              class: 'normal'
            },
            {
              name: 'proxy-log',
              renderer: 'focused',
              class: 'normal'
            }
          ],
          connections: [
            {
              source: 'INTERNET',
              target: 'proxy-log',
              metrics: {
                danger: 126.524,
                normal: 185.906
              },
              class: 'normal'
            }
          ]
        }
      ],
      connections: [
        {
          source: 'INTERNET',
          target: 'us-east-2',
          metrics: {
            normal: 12037.626,
            danger: 2.37
          },
          notices: [],
          class: 'normal'
        }
      ]
    };

    expect(actions.updateETSData(data)).toEqual({
      type: t.UPDATE_ETS_DATA,
      data
    });
  });

  it('should handle updateETSView action creator', () => {
    expect(actions.updateETSView(['us-west-1'])).toEqual({
      type: t.UPDATE_ETS_VIEW,
      view: ['us-west-1']
    });
  });
});
