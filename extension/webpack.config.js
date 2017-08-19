/* eslint-env node */
const path = require('path')
const CopyWebpackPlugin = require('copy-webpack-plugin')

module.exports = {
    entry: {
        index: './src/index.js',
        pageTags: './src/pageTags.js'
    },
    output: {
        filename: '[name].js',
        path: path.resolve(__dirname, 'dist')
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
            { from: 'manifest.json'}
        ])
    ]
}