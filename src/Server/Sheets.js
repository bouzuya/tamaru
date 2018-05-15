"use strict";

var google = require('googleapis').google;

exports.createClientImpl = function (key) {
  return function () {
    return google.sheets({ auth: key, version: 'v4' });
  };
};

exports.getValuesImpl = function (client) {
  return function (spreadsheetId) {
    return function (range) {
      return function () {
        return new Promise(function (resolve, reject) {
          return client.spreadsheets.values.get(
            { range: range, spreadsheetId: spreadsheetId },
            function (error, response) {
              if (error) {
                reject(error);
              } else {
                resolve(response.data.values);
              }
            });
        });
      };
    };
  };
};
