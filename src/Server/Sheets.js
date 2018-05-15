"use strict";

var google = require('googleapis').google;

exports.createClientImpl = function (key) {
  return function () {
    return google.sheets({ auth: key, version: 'v4' });
  };
};

exports.getRowsImpl = function (client) {
  return function (spreadsheetId) {
    return function (range) {
      return function () {
        return new Promise(function (resolve, reject) {
          return client.spreadsheets.values.get(
            {
              dateTimeRenderOption: "SERIAL_NUMBER",
              majorDimension: "ROWS",
              range: range,
              spreadsheetId: spreadsheetId,
              valueRenderOption: "UNFORMATTED_VALUE"
            },
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

exports.getSheetTitlesImpl = function (client) {
  return function (spreadsheetId) {
    return function () {
      return new Promise(function (resolve, reject) {
        return client.spreadsheets.get(
          {
            includeGridData: false,
            spreadsheetId: spreadsheetId
          },
          function (error, response) {
            if (error) {
              reject(error);
            } else {
              resolve(
                response.data.sheets.map(function (i) {
                  return i.properties.title;
                })
              );
            }
          });
      });
    };
  };
};
