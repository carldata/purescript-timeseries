"use strict";

exports.parseISOTimeImpl = function (str) {
  // Force UTC zone 
  if(!str.endsWith("Z")){
    str += "Z";
  }
  var dt = new Date(str);
  return dt.getTime();
};

