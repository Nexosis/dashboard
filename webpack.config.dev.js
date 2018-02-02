import path from 'path';
import webpack from 'webpack';
import combineLoaders from 'webpack-combine-loaders';

export default {
    devtool: 'eval-source-map',
    cache: true,
    entry: entry,
    target: 'web',
    output: {
        path: path.join(__dirname, "dist"),
        filename: 'dashboard.js',
        sourceMapFilename: 'dashboard.map.js'
    },
    resolve: {
        extensions: ['*', '.js', '.elm']
    },
    plugins: [
        new webpack.DefinePlugin({
            'process.env.NODE_ENV': JSON.stringify('development'),
            __DEV__: true
        }),
        new webpack.HotModuleReplacementPlugin(),
        new webpack.NoEmitOnErrorsPlugin(),
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
                loader: combineLoaders([
                    {
                        loader: 'elm-hot-loader'
                    },
                    {
                        loader: 'elm-webpack-loader?verbose=true&warn=true&debug=true&pathToMake=' + __dirname + '\\node_modules\\.bin\\elm-make&cwd=' + __dirname + '&forceWatch=true'
                    }
                ])
            }
        ]
    }
};