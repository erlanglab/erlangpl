if (process.env.NODE_ENV !== 'production') {
  // here you can specify your plugins
  // this only aplies to development
  // in production plugins are included automatically based on epl-* pattern in your package.json dependencies
  require('epl-plugin/dev');
  require('epl-plugin/dev/style.css');
}
