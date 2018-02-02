import path from 'path';
import webpack from 'webpack';

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
            'process.env.NODE_ENV': JSON.stringify('production'),
            __DEV__: false
        }),
        new webpack.NoEmitOnErrorsPlugin(),

        new webpack.optimize.ModuleConcatenationPlugin(),
        new webpack.optimize.OccurrenceOrderPlugin(),
        new webpack.optimize.UglifyJsPlugin({ sourceMap: true, comments: false, compress: { warnings: false } }),
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