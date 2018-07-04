"use strict";

exports.fetchImpl = function (options) {
  return function () {
    return window.fetch(options.url, {
      body: options.body,
      headers: options.headers,
      method: options.method
    });
  };
};

exports.jsonImpl = function (response) {
  return function () {
    return response.json();
  };
};

exports.statusImpl = function (response) {
  return response.status;
};
