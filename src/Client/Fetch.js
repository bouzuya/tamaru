"use strict";

exports.fetchImpl = function (options) {
  return window.fetch(options.url, {
    body: options.body,
    headers: options.headers,
    method: options.method
  });
};
