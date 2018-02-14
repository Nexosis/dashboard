import path from 'path';
import webpack from 'webpack';
import WebpackMd5Hash from 'webpack-md5-hash'
import HtmlWebpackPlugin from 'html-webpack-plugin';
import UglifyJsPlugin from 'uglifyjs-webpack-plugin';
import ExtractTextPlugin from 'extract-text-webpack-plugin';

const GLOBALS = {
    'process.env.NODE_ENV': JSON.stringify('production'),
    __DEV__: false
};

export default {
    cache: true,
    devtool: 'source-map',
    entry: path.resolve(__dirname, 'src/index.js'),
    target: 'web',
    output: {
        path: path.resolve(__dirname, "dist"),
        publicPath: '/dashboard/',
        filename: '[name].[chunkhash].js'
    },
    resolve: {
        extensions: ['*', '.js', '.elm']
    },
    resolveLoader: {
        modules: [path.resolve(__dirname, 'tools'), 'node_modules']
    },
    plugins: [
        new WebpackMd5Hash(),
        new webpack.DefinePlugin(GLOBALS),

        new ExtractTextPlugin('[name].[contenthash].css'),

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
                minifyCSS: true,
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
            },
            {
                test: /(\.css|\.scss|\.sass)$/,
                use: ExtractTextPlugin.extract({
                    use: [
                        {
                            loader: 'css-loader',
                            options: {
                                minimize: true,
                                sourceMap: true
                            }
                        },
                        {
                            loader: 'postcss-loader',
                            options: {
                                plugins: () => [
                                    require('autoprefixer')
                                ],
                                sourceMap: true
                            }
                        },
                        {
                            loader: 'sass-loader',
                            options: {
                                sourceMap: true
                            }
                        }
                    ]
                })
            },
            {
                test: /config\.json$/,
                use: [
                    {
                        loader: 'file-loader',
                        options: { name: '[name].[ext]' }
                    },
                    'config-loader',
                ]
            }
        ]
    }
};