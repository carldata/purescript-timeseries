"use strict";

exports.parseIsoNative = function (str) {
  var dt = new Date(str);
  return {
          year: dt.getUTCFullYear(),
          month: dt.getUTCMonth() + 1,
          day: dt.getUTCDate(),
          hour: dt.getUTCHours(),
          minute: dt.getUTCMinutes(),
          second: dt.getUTCSeconds(),
          millisecond: dt.getUTCMilliseconds()
  };
};

