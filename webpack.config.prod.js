import path from 'path';
import webpack from 'webpack';
import WebpackMd5Hash from 'webpack-md5-hash'
import HtmlWebpackPlugin from 'html-webpack-plugin';
import UglifyJsPlugin from 'uglifyjs-webpack-plugin';

export default {
    cache: true,
    devtool: 'source-map',
    entry: path.resolve(__dirname, 'src/index.js'),
    target: 'web',
    output: {
        path: path.resolve(__dirname, "dist"),
        publicPath: '/',
        filename: 'dashboard.js'
    },
    resolve: {
        extensions: ['*', '.js', '.elm']
    },
    plugins: [
        new WebpackMd5Hash(),
        new webpack.DefinePlugin({
            'process.env.NODE_ENV': JSON.stringify('production'),
            __DEV__: false
        }),
        new webpack.NoEmitOnErrorsPlugin(),
        new HtmlWebpackPlugin({
            template: 'src/index.ejs',
            favicon: 'src/favicon.ico',
            minify: {
                removeComments: true,
                collapseWhitespace: true,
                removeRedundantAttributes: true,
                useShortDoctype: true,
                removeEmptyAttributes: true,
                removeStyleLinkTypeAttributes: true,
                keepClosingSlash: true,
                minifyJS: true,
                minifyURLs: true
            },
            inject: true
        }),

        new webpack.optimize.ModuleConcatenationPlugin(),
        new webpack.optimize.OccurrenceOrderPlugin(),
        new UglifyJsPlugin({ sourceMap: true })
    ],
    module: {
        rules: [
            {
                loader: 'babel-loader',
                test: /\.js&/,
                exclude: [/elm-stuff/, /node_modules/],
            },
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                loader: 'elm-webpack-loader?verbose=true&warn=true&debug=false&pathToMake=' + __dirname + '\\node_modules\\.bin\\elm-make&cwd=' + __dirname + '&forceWatch=false'
            }
        ]
    }
};