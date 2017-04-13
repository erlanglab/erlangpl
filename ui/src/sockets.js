// @flow

type Socket = {
  builtIn: {
    onclose?: Array<() => void>,
    onopen?: Array<() => void>
  },
  route: string,
  topics: {
    [key: string]: () => void
  }
};

export const on = (
  route: string,
  topics: { [key: string]: () => void },
  onopen: any = undefined,
  onclose: any = undefined
): Socket => {
  return {
    route,
    topics,
    builtIn: {
      onopen,
      onclose
    }
  };
};

export const onWithStore = (handler: any): ((store: any) => Socket) => {
  return (store: any) => handler(store, on);
};

export const combineSockets = (
  sockets: Array<Socket | ((store: any) => Socket)>,
  store: any
) => {
  // apply
  return sockets
    .map(socket => {
      // if socket is a function ( created with onWithStore ), we have to provide store to it
      if (typeof socket === 'function') {
        return socket(store);
      }
      return socket;
    })
    .reduce((acc, { route, topics, builtIn }) => {
      const concatenatedTopics = Object.keys(topics).reduce((acc: {
        [key: string]: Array<() => void>
      }, topic: string) => {
        return {
          ...acc,
          [topic]: (acc[topic] || []).concat(topics[topic])
        };
      }, acc[route] ? acc[route].topics : {});

      return !acc.hasOwnProperty(route)
        ? {
            ...acc,
            [route]: {
              topics: concatenatedTopics,
              __builtIn: {
                onopen: [].concat(builtIn.onopen || []),
                onclose: [].concat(builtIn.onclose || [])
              }
            }
          }
        : {
            ...acc,
            [route]: {
              ...acc[route],
              topics: concatenatedTopics,
              __builtIn: {
                onopen: acc[route].__builtIn.onopen.concat(
                  builtIn.onopen || []
                ),
                onclose: acc[route].__builtIn.onclose.concat(
                  builtIn.onclose || []
                )
              }
            }
          };
    }, {});
};

let socketsArray = [];

export const createSockets = (sockets: any) => {
  socketsArray = Object.keys(sockets).map(route => {
    let { hostname, port } = window.location;

    if (process.env.NODE_ENV !== 'production') {
      port = 37575;
    }

    let ws = new WebSocket(`ws://${hostname}:${port}/${route}`);

    const handlers = Object.keys(sockets[route].topics).reduce((acc, topic) => {
      if (Array.isArray(sockets[route].topics[topic])) {
        return {
          ...acc,
          [topic]: sockets[route].topics[topic]
        };
      }
      return acc;
    }, {});

    ws.onopen = () => {
      console.log(`${route} - connection opened`);
      sockets[route].__builtIn.onopen.forEach(c => {
        if (typeof c === 'function') c();
      });
    };

    ws.onclose = () => {
      console.log(`${route} - connection closed`);
      sockets[route].__builtIn.onclose.forEach(c => {
        if (typeof c === 'function') c();
      });
    };

    ws.onmessage = (msg: any) => {
      const { topic, data } = JSON.parse(msg.data);
      if (handlers[topic] !== undefined) {
        handlers[topic].forEach(handler => {
          if (typeof handler === 'function') {
            handler(data);
          } else {
            console.warn('Could not find handler for', topic);
          }
        });
      }
    };
    return { route, ws };
  });
};

export const send = (route: string, message: string) => {
  let socket = socketsArray.find(s => s.route === route);
  if (socket) {
    socket.ws.send(message);
  } else {
    console.warn(`Could not find socket on ${route} route`);
  }
};
