var fs = require('fs');
var path = require('path');
var chalk = require('chalk');
var paths = require('./paths');
var packageJSON = require(paths.appPackageJson);

function getPlugins(mode) {
  return packageJSON.plugins.map(plugin => {
    const pluginDirectory = `${paths.appNodeModules}/${plugin}/${mode}`;
    let p = { media: null, name: plugin, styles: [], scripts: [] };
    const media = `${pluginDirectory}/${plugin}`;

    try {
      const mediaFolder = fs.readdirSync(media);
      if (mediaFolder.length) p.media = media;
    } catch (e) {
      // noop
    }

    try {
      fs.readdirSync(pluginDirectory).forEach(file => {
        const ext = path.extname(file);
        if (ext === '.js')
          p.scripts.push({ name: file, dir: `${pluginDirectory}/${file}` });
        if (ext === '.css')
          p.styles.push({ name: file, dir: `${pluginDirectory}/${file}` });
      });
    } catch (e) {
      console.warn(
        chalk.red(
          `Could not find source for ${chalk.red.bold(plugin)} in node_modules`
        )
      );
    }
    return p;
  });
}

module.exports = {
  pluginsDev: () => getPlugins('dev'),
  pluginsProd: () => getPlugins('build')
};
