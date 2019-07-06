'use strict';

module.exports = {
  devtool: 'inline-source-map',
  entry: './src/index.ts',
  module: {
     rules: [
       {
         test: /\.tsx?$/,
         loader: 'ts-loader',
       },
       {
         test: /\.pl$/,
         loader: 'raw-loader',
       },
     ],
  },
  resolve: {
    extensions: [ '.ts', '.tsx', '.js' ],
  },
};
