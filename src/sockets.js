// @flow

type Socket = {
  builtIn: {
    onclose?: Array<() => void>,
    onopen?: Array<() => void>
  },
  route: string,
  topic: string,
  handler: () => void
};

export const on = (
  route: string,
  topics: { [key: string]: () => void },
  onopen: any = undefined,
  onclose: any = undefined
): Array<Socket> => {
  return Object.keys(topics).map(topic => {
    return {
      route,
      topic,
      handler: topics[topic],
      builtIn: {
        onclose,
        onopen
      }
    };
  });
};

export const combineSockets = (sockets: Array<Array<Socket>>) => {
  const flatten = [].concat.apply([], sockets);
  return flatten.reduce(
    (acc, { route, topic, handler, builtIn }) => {
      const concatTopic = (acc, route, topic, handler) => {
        const functions = acc[route][topic];
        return functions ? functions.concat(handler) : [handler];
      };

      return !acc.hasOwnProperty(route)
        ? {
            ...acc,
            [route]: {
              __builtIn: {
                onopen: [].concat(builtIn.onopen || []),
                onclose: [].concat(builtIn.onclose || [])
              },
              [topic]: [].concat(handler)
            }
          }
        : {
            ...acc,
            [route]: {
              ...acc[route],
              __builtIn: {
                onopen: acc[route].__builtIn.onopen.concat(
                  builtIn.onopen || []
                ),
                onclose: acc[route].__builtIn.onclose.concat(
                  builtIn.onclose || []
                )
              },
              [topic]: concatTopic(acc, route, topic, handler)
            }
          };
    },
    {}
  );
};

export const createSockets = (sockets: any) => {
  return Object.keys(sockets).map(route => {
    const { hostname } = window.location;
    let ws = new WebSocket(`ws://${hostname}:8000/${route}`);

    const handlers = Object.keys(sockets[route]).reduce((acc, topic) => {
      if (Array.isArray(sockets[route][topic])) {
        return {
          ...acc,
          [topic]: sockets[route][topic]
        };
      }
      return acc;
    }, {});

    ws.onopen = () => sockets[route].__builtIn.onopen.forEach(c => {
      if (typeof c === 'function') c();
    });

    ws.onclose = () => sockets[route].__builtIn.onclose.forEach(c => {
      if (typeof c === 'function') c();
    });

    ws.onmessage = (msg: any) => {
      const { topic, data } = JSON.parse(msg.data);
      handlers[topic].forEach(handler => {
        if (typeof handler === 'function') {
          handler(data);
        } else {
          console.warn('Could not find handler for', topic);
        }
      });
    };
    return ws;
  });
};
