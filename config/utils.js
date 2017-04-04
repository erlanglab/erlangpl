var fs = require('fs');
var path = require('path');
var packageJSON = require('../package.json');

function getPlugins(mode) {
  const a = packageJSON.plugins.map(plugin => {
    const pluginDirectory = `./node_modules/${plugin}/${mode}`;
    let p = { media: null, name: plugin, styles: [], scripts: [] };
    const media = `${pluginDirectory}/${plugin}`;

    try {
      const mediaFolder = fs.readdirSync(media);
      if (mediaFolder.length) p.media = media;
    } catch (e) {
      // noop
    }

    fs.readdirSync(pluginDirectory).forEach(file => {
      const ext = path.extname(file);
      if (ext === '.js')
        p.scripts.push({ name: file, dir: `${pluginDirectory}/${file}` });
      if (ext === '.css')
        p.styles.push({ name: file, dir: `${pluginDirectory}/${file}` });
    });
    return p;
  });
  return a;
}

const pluginsDev = getPlugins('dev');
const pluginsProd = getPlugins('build');

module.exports = {
  pluginsDev: pluginsDev,
  pluginsProd: pluginsProd
};
