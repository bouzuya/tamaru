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

exports.setRowsImpl = function (email) {
  return function (key) {
    return function (spreadsheetId) {
      return function (range) {
        return function (rows) {
          return function () {
            return google.auth.getClient({
              credentials: {
                client_email: email,
                private_key: key
              },
              scopes: [
                'https://www.googleapis.com/auth/drive',
                'https://www.googleapis.com/auth/drive.file',
                'https://www.googleapis.com/auth/spreadsheets'
              ]
            })
              .then(function (auth) {
                return new Promise(function (resolve, reject) {
                  var client = google.sheets({ auth: auth, version: 'v4' });
                  return client.spreadsheets.values.update(
                    {
                      includeValuesInResponse: false,
                      range: range,
                      requestBody: {
                        majorDimension: 'ROWS',
                        range: range,
                        values: rows
                      },
                      responseValueRenderOption: 'FORMATTED_VALUE',
                      spreadsheetId: spreadsheetId,
                      valueInputOption: 'RAW'
                    },
                    function (error, response) {
                      if (error) {
                        reject(error);
                      } else {
                        resolve(response.data.values);
                      }
                    });
                });
              });
          };
        };
      };
    };
  };
};
