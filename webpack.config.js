'use strict';

module.exports = {
  target: 'node',
  devtool: 'inline-source-map',
  entry: './src/index.ts',
  module: {
     rules: [
       {
         test: /\.tsx?$/,
         loader: 'ts-loader',
       },
       {
         test: /\.pro$/,
         loader: 'raw-loader',
       },
     ],
  },
  resolve: {
    extensions: [ '.ts', '.tsx', '.js' ],
  },
};
