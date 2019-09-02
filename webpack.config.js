const HtmlWebpackPlugin = require('html-webpack-plugin')

module.exports = (env, argv) => {
    return {
        entry: `${__dirname}/src/index.js`,
        output: {
            path: `${__dirname}/dist`,
            filename: 'bundle.js',
            libraryTarget: 'window',
        },
        module: {
            rules: [
                {
                    test: /\.(css|scss)$/,
                    loader: ['style-loader', 'css-loader', 'sass-loader'],
                },
                {
                    test:    /\.elm$/,
                    loader: 'elm-webpack-loader',
                    options: {
                        debug: (argv.mode !== 'production')
                    }
                }
            ],
        },
        devServer: {
            port: '8080',
            compress: true,
            watchContentBase: true,
        }
    };
};