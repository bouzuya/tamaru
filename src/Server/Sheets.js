"use strict";

var google = require('googleapis').google;

exports.getRowsImpl = function (key) {
  return function (spreadsheetId) {
    return function (range) {
      return function () {
        return new Promise(function (resolve, reject) {
          var client = google.sheets({ auth: key, version: 'v4' });
          return client.spreadsheets.values.get(
            {
              dateTimeRenderOption: "SERIAL_NUMBER",
              majorDimension: "ROWS",
              range: range,
              spreadsheetId: spreadsheetId,
              valueRenderOption: "FORMATTED_VALUE"
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

exports.getSheetTitlesImpl = function (key) {
  return function (spreadsheetId) {
    return function () {
      return new Promise(function (resolve, reject) {
        var client = google.sheets({ auth: key, version: 'v4' });
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
