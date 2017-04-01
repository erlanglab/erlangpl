var packageJSON = require('../package.json');

function getPlugins() {
  return Object.keys(packageJSON.dependencies)
    .concat(Object.keys(packageJSON.devDependencies))
    .reduce(
      (acc, p) => {
        if (p !== 'epl-scripts' && p.match(/^epl-/)) return acc.concat(p);
        return acc;
      },
      []
    );
}

module.exports = {
  getPlugins: getPlugins
};
