if (process.env.NODE_ENV !== 'production') {
  // we're importing script and styles from given plugin
  // this method should be only used in development
  // for production specify your plugin in package.json
  require('epl-plugin/dev');
  require('epl-plugin/dev/style.css');

  // add plugins in development mode here
  // require('epl-plugin/dev');
  // require('epl-plugin/dev/style.css');
}

// plugin handlers
const plugins = Object.keys(window)
  .filter(key => {
    return key.match(/^__EPL_/);
  })
  .map(key => console.log('Loaded:', key.replace('__EPL_', '')) || window[key]);
console.log(plugins);

export default plugins;
