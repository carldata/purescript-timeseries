"use strict";

exports.parseISOTimeImpl = function (str) {
  var dt = new Date(str);
  return dt.getTime();
};

