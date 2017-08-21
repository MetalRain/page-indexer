/* eslint-env node */
const path = require('path')
const CopyWebpackPlugin = require('copy-webpack-plugin')

const distPath = path.resolve(__dirname, 'dist')

module.exports = {
    entry: {
        index: './src/index.js',
        pageTags: './src/pageTags.js'
    },
    output: {
        filename: '[name].js',
        path: distPath
    },
    module: {
        rules: [
            {
                test: /\.js$/,
                exclude: /(node_modules|bower_components)/,
                use: {
                    loader: 'babel-loader',
                    options: {
                        presets: ['env']
                    }
                }
            }
        ]
    },
    plugins: [
        new CopyWebpackPlugin([
            { from: 'src/popup.html' },
            { from: 'manifest.json' },
            { from: 'media/icon-*(full-){16,32,48,64,128}.png', to: '[name].png' },
            { from: 'node_modules/purecss/build/pure-min.css' }
        ])
    ]
}