require('./main.scss');

const { Elm } = require('./Main.elm');

const app = Elm.Main.init({
    node: document.getElementById('main')
});
