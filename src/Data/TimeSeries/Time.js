"use strict";

exports.parseISOTimeImpl = function (str) {
  var dt = new Date(str);
  return dt.getTime();
};

exports.formatTime = function (t) {
  var d = new Date(t);
  return d.toISOString();
};