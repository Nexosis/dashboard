import path from 'path';
import webpack from 'webpack';
import combineLoaders from 'webpack-combine-loaders';
import HtmlWebpackPlugin from 'html-webpack-plugin';
import ScriptExtHtmlWebpackPlugin from 'script-ext-html-webpack-plugin';

export default {
    mode: 'development',
    devtool: 'eval-source-map',
    cache: true,
    entry: [
        './src/webpack-public-path',
        'webpack-hot-middleware/client?reload=true',
        path.resolve(__dirname, './src/index.js')
    ],
    target: 'web',
    output: {
        path: path.join(__dirname, "dist"),
        filename: 'dashboard.js',
        sourceMapFilename: 'dashboard.map.js'
    },
    resolve: {
        extensions: ['*', '.js', '.elm']
    },
    resolveLoader: {
        modules: [path.resolve(__dirname, 'tools'), 'node_modules']
    },
    plugins: [
        new webpack.DefinePlugin({
            'process.env.NODE_ENV': JSON.stringify('development'),
            __DEV__: true
        }),
        new webpack.HotModuleReplacementPlugin(),
        new HtmlWebpackPlugin({
            template: 'src/index.ejs',
            minify: {
                removeComments: false,
                collapseWhitespace: true
            },
            inject: 'head'
        }),
        new ScriptExtHtmlWebpackPlugin({
            defaultAttribute: 'defer'
        })
    ],
    optimization: {
        noEmitOnErrors: true,
        splitChunks: {
            cacheGroups: {
                default: false,
                commons: {
                    // Do not include nexosis-styles as a vendor css file.
                    test: /[\\/]node_modules[\\/](?!nexosis)/,
                    name: 'vendor',
                    chunks: 'all'
                }
            }
        }
    },
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
                loader: combineLoaders([
                    {
                        loader: 'elm-hot-loader'
                    },
                    {
                        loader: 'elm-webpack-loader?verbose=true&warn=true&debug=true&pathToMake=' + __dirname + '/node_modules/.bin/elm-make&cwd=' + __dirname + '&forceWatch=true'
                    }
                ])
            },
            {
                test: /(\.css|\.scss|\.sass)$/,
                use: [
                    'style-loader',
                    {
                        loader: 'css-loader',
                        options: {
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
            },
            {
                type: 'javascript/auto',
                test: /(config)\.json$/,
                use: [
                    {
                        loader: 'file-loader',
                        options: { name: '[name].[ext]' }
                    }
                    , 'config-loader'
                ]
            },
            {
                test: /(\.md)$/,
                use: [
                    {
                        loader: 'file-loader',
                        options: { name: 'docs/[name].[ext]' }
                    }
                ]
            }
        ]
    }
};