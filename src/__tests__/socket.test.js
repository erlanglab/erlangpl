import { combineSockets, on } from '../sockets';

describe('socket helpers', () => {
  it('combineSockets should combine', () => {
    const socket1 = [
      {
        builtIn: {
          onclose: [() => {}],
          onopen: [() => {}]
        },
        route: 'epl_dashboard_EPL',
        topic: 'system-init',
        handler: () => {}
      }
    ];

    const socket2 = [
      {
        builtIn: {
          onopen: undefined,
          onclose: undefined
        },
        route: 'epl_dashboard_EPL',
        topic: 'system-init',
        handler: () => {}
      }
    ];
    const sockets = combineSockets([socket1, socket2]);
    expect(sockets.epl_dashboard_EPL.__builtIn.onclose).toHaveLength(1);
    expect(sockets.epl_dashboard_EPL.__builtIn.onopen).toHaveLength(1);
    expect(sockets.epl_dashboard_EPL['system-init']).toHaveLength(2);
  });

  it('should create socket from on function', () => {
    const aaa = data => {};
    const socket = on('epl_dashboard_EPL', {
      'system-init': aaa
    });

    expect(socket).toEqual([
      {
        builtIn: {
          onopen: undefined,
          onclose: undefined
        },
        route: 'epl_dashboard_EPL',
        topic: 'system-init',
        handler: aaa
      }
    ]);
  });

  it('should work together (on, combinedSockets)', () => {
    const handler = () => {};
    const socket1 = on('epl_dashboard_EPL', {
      'system-init': handler
    });
    const socket2 = on('epl_dashboard_EPL', {
      'system-info': handler
    });

    const combined = combineSockets([socket1, socket2]);
    expect(combined).toEqual({
      epl_dashboard_EPL: {
        __builtIn: {
          onclose: [],
          onopen: []
        },
        'system-init': [handler],
        'system-info': [handler]
      }
    });
  });
});
