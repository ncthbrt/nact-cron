// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("bs-platform/lib/js/caml_js_exceptions.js");

var substitutions = [
  [
    "@annually",
    "0 0 1 1 *"
  ],
  [
    "@monthly",
    "0 0 1 * *"
  ],
  [
    "@weekly",
    "0 0 * * 0"
  ],
  [
    "@daily",
    "0 0 * * *"
  ],
  [
    "@midnight",
    "0 0 * * *"
  ],
  [
    "@hourly",
    "0 * * * *"
  ]
];

function substitute(substitutions, str) {
  return Belt_Array.reduce(substitutions, str.toUpperCase(), (function (prevStr, param) {
                if (param[0] === str) {
                  return param[1];
                } else {
                  return prevStr;
                }
              }));
}

var __x = Belt_Array.range(1, 12);

var monthNames = Belt_Array.zip([
      "JAN",
      "FEB",
      "MAR",
      "APR",
      "MAY",
      "JUN",
      "JUL",
      "AUG",
      "SEP",
      "OCT",
      "NOV",
      "DEC"
    ], Belt_Array.map(__x, (function (prim) {
            return String(prim);
          })));

var __x$1 = Belt_Array.range(0, 6);

var __x$2 = Belt_Array.zip([
      "SUN",
      "MON",
      "TUE",
      "WED",
      "THU",
      "FRI",
      "SAT"
    ], Belt_Array.map(__x$1, (function (prim) {
            return String(prim);
          })));

var dayNames = Belt_Array.concat(__x$2, [
      [
        "7",
        "0"
      ],
      [
        "L",
        "7"
      ]
    ]);

var MalformedCronExpression = /* @__PURE__ */Caml_exceptions.create("NactCron_Expression.MalformedCronExpression");

function isNumber(str) {
  try {
    Caml_format.caml_int_of_string(str);
    return true;
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Failure") {
      return false;
    }
    throw exn;
  }
}

function parseToken(field, substitutions) {
  var substitute$1 = function (param) {
    return substitute(substitutions, param);
  };
  var match = field.split("-");
  var len = match.length;
  if (len >= 3) {
    throw {
          RE_EXN_ID: MalformedCronExpression,
          Error: new Error()
        };
  }
  switch (len) {
    case 0 :
        throw {
              RE_EXN_ID: MalformedCronExpression,
              Error: new Error()
            };
    case 1 :
        var n = match[0];
        switch (n) {
          case "*" :
          case "?" :
              return /* WildcardToken */0;
          default:
            var __x = n.split(",");
            return {
                    TAG: /* CommaSeparatedArrayToken */1,
                    _0: Belt_Array.map(__x, (function (i) {
                            try {
                              return Caml_format.caml_int_of_string(Curry._1(substitute$1, i));
                            }
                            catch (raw_exn){
                              var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
                              if (exn.RE_EXN_ID === "Failure") {
                                throw {
                                      RE_EXN_ID: MalformedCronExpression,
                                      Error: new Error()
                                    };
                              }
                              throw exn;
                            }
                          }))
                  };
        }
    case 2 :
        var start = match[0];
        var exit = 0;
        var match$1 = match[1];
        if (match$1 === "") {
          if (isNumber(substitute(substitutions, start))) {
            return {
                    TAG: /* IntervalToken */0,
                    _0: Caml_format.caml_int_of_string(substitute(substitutions, start)),
                    _1: undefined
                  };
          }
          exit = 2;
        } else {
          exit = 2;
        }
        if (exit === 2 && start === "") {
          var end_ = match[1];
          if (isNumber(substitute(substitutions, end_))) {
            return {
                    TAG: /* IntervalToken */0,
                    _0: undefined,
                    _1: Caml_format.caml_int_of_string(substitute(substitutions, end_))
                  };
          }
          
        }
        var end_$1 = match[1];
        if (isNumber(substitute(substitutions, start)) && isNumber(substitute(substitutions, end_$1))) {
          return {
                  TAG: /* IntervalToken */0,
                  _0: Caml_format.caml_int_of_string(substitute(substitutions, start)),
                  _1: Caml_format.caml_int_of_string(substitute(substitutions, end_$1))
                };
        }
        throw {
              RE_EXN_ID: MalformedCronExpression,
              Error: new Error()
            };
        break;
    
  }
}

function getIntervalToken(step) {
  try {
    return Caml_format.caml_int_of_string(step);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Failure") {
      throw {
            RE_EXN_ID: MalformedCronExpression,
            Error: new Error()
          };
    }
    throw exn;
  }
}

function inRange(start, end_, n) {
  if (Caml_obj.caml_lessequal(start, n)) {
    return Caml_obj.caml_lessequal(n, end_);
  } else {
    return false;
  }
}

function parseSubExpr(subExpr, start, end_, substitutions) {
  var match = subExpr.split("/");
  var len = match.length;
  if (len >= 3) {
    throw {
          RE_EXN_ID: MalformedCronExpression,
          Error: new Error()
        };
  }
  var match$1;
  switch (len) {
    case 0 :
        throw {
              RE_EXN_ID: MalformedCronExpression,
              Error: new Error()
            };
    case 1 :
        var field = match[0];
        match$1 = [
          parseToken(field, substitutions),
          undefined
        ];
        break;
    case 2 :
        var field$1 = match[0];
        var step = match[1];
        match$1 = [
          parseToken(field$1, substitutions),
          getIntervalToken(step)
        ];
        break;
    
  }
  var stepToken = match$1[1];
  var fieldToken = match$1[0];
  if (typeof fieldToken === "number") {
    if (stepToken !== undefined) {
      return {
              NAME: "Interval",
              VAL: [
                start,
                end_,
                stepToken
              ]
            };
    } else {
      return "Wildcard";
    }
  }
  if (fieldToken.TAG === /* IntervalToken */0) {
    var b = fieldToken._1;
    var a = fieldToken._0;
    if (stepToken !== undefined) {
      return {
              NAME: "Interval",
              VAL: [
                Belt_Option.getWithDefault(a, start),
                Belt_Option.getWithDefault(b, end_),
                stepToken
              ]
            };
    } else {
      return {
              NAME: "Interval",
              VAL: [
                Belt_Option.getWithDefault(a, start),
                Belt_Option.getWithDefault(b, end_),
                1
              ]
            };
    }
  }
  var lst = fieldToken._0;
  if (stepToken !== undefined) {
    if (lst.length !== 1) {
      throw {
            RE_EXN_ID: MalformedCronExpression,
            Error: new Error()
          };
    }
    var n = lst[0];
    if (inRange(start, end_, n)) {
      return {
              NAME: "Interval",
              VAL: [
                n,
                end_,
                stepToken
              ]
            };
    }
    throw {
          RE_EXN_ID: MalformedCronExpression,
          Error: new Error()
        };
  }
  if (Belt_Array.every(lst, (function (param) {
            return inRange(start, end_, param);
          }))) {
    return {
            NAME: "Values",
            VAL: lst.sort()
          };
  }
  throw {
        RE_EXN_ID: MalformedCronExpression,
        Error: new Error()
      };
}

var nearestWeekdayRegex = new RegExp("^\\d+W$");

var lastDayOfWeekRegex = new RegExp("^[0-7]L$");

var daysBeforeEndOfMonthRegex = new RegExp("^L\\-\\d+$");

var nthDayOfWeekInMonthRegex = new RegExp("^([0-7]|[A-Za-z]{3})#[1-5]$");

function parseDaysOfMonthSubExpr(subExpr) {
  switch (subExpr) {
    case "L" :
        return {
                NAME: "DaysBeforeEndOfMonth",
                VAL: 0
              };
    case "LW" :
        return "LastWeekdayOfMonth";
    default:
      if (daysBeforeEndOfMonthRegex.test(subExpr)) {
        var n = Caml_format.caml_int_of_string(subExpr.replace("L-", ""));
        if (inRange(1, 31, n)) {
          return {
                  NAME: "DaysBeforeEndOfMonth",
                  VAL: n - 1 | 0
                };
        }
        throw {
              RE_EXN_ID: MalformedCronExpression,
              Error: new Error()
            };
      }
      if (!nearestWeekdayRegex.test(subExpr)) {
        return parseSubExpr(subExpr, 1, 31, []);
      }
      var n$1 = Caml_format.caml_int_of_string(subExpr.replace("W", ""));
      if (inRange(1, 32, n$1)) {
        return {
                NAME: "NearestWeekday",
                VAL: n$1
              };
      }
      throw {
            RE_EXN_ID: MalformedCronExpression,
            Error: new Error()
          };
  }
}

function parseDaysOfWeekSubExpr(subExpr) {
  if (lastDayOfWeekRegex.test(subExpr)) {
    var n = Caml_format.caml_int_of_string(subExpr.replace("L", ""));
    return {
            NAME: "LastDayOfWeekInMonth",
            VAL: n === 7 ? 0 : n
          };
  }
  if (!nthDayOfWeekInMonthRegex.test(subExpr)) {
    return parseSubExpr(subExpr, 0, 6, dayNames);
  }
  var arr = subExpr.split("#");
  var match;
  try {
    match = [
      Caml_format.caml_int_of_string(substitute(dayNames, Caml_array.get(arr, 0))),
      Caml_format.caml_int_of_string(Caml_array.get(arr, 1))
    ];
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Failure") {
      throw {
            RE_EXN_ID: MalformedCronExpression,
            Error: new Error()
          };
    }
    throw exn;
  }
  return {
          NAME: "NthDayOfWeekInMonth",
          VAL: [
            match[0],
            match[1]
          ]
        };
}

function parseYearsSubExpr(subExpr) {
  var match = subExpr.split("/");
  var len = match.length;
  if (len >= 3) {
    throw {
          RE_EXN_ID: MalformedCronExpression,
          Error: new Error()
        };
  }
  var match$1;
  switch (len) {
    case 0 :
        throw {
              RE_EXN_ID: MalformedCronExpression,
              Error: new Error()
            };
    case 1 :
        var field = match[0];
        match$1 = [
          parseToken(field, []),
          undefined
        ];
        break;
    case 2 :
        var field$1 = match[0];
        var step = match[1];
        match$1 = [
          parseToken(field$1, []),
          getIntervalToken(step)
        ];
        break;
    
  }
  var stepToken = match$1[1];
  var fieldToken = match$1[0];
  if (typeof fieldToken === "number") {
    if (stepToken !== undefined) {
      return {
              NAME: "Interval",
              VAL: [
                undefined,
                undefined,
                stepToken
              ]
            };
    } else {
      return "Wildcard";
    }
  }
  if (fieldToken.TAG === /* IntervalToken */0) {
    return {
            NAME: "Interval",
            VAL: [
              fieldToken._0,
              fieldToken._1,
              Belt_Option.getWithDefault(stepToken, 1)
            ]
          };
  }
  var lst = fieldToken._0;
  if (stepToken === undefined) {
    return {
            NAME: "Values",
            VAL: lst.sort()
          };
  }
  if (lst.length !== 1) {
    throw {
          RE_EXN_ID: MalformedCronExpression,
          Error: new Error()
        };
  }
  var n = lst[0];
  return {
          NAME: "Interval",
          VAL: [
            n,
            undefined,
            stepToken
          ]
        };
}

var spacesRegex = new RegExp("\\s+");

function parse(str) {
  var __x = substitute(substitutions, str.trim()).split(spacesRegex);
  var match = Belt_List.fromArray(Belt_Array.keepMap(__x, (function (x) {
              return x;
            })));
  if (match) {
    var match$1 = match.tl;
    if (match$1) {
      var match$2 = match$1.tl;
      if (match$2) {
        var match$3 = match$2.tl;
        if (match$3) {
          var match$4 = match$3.tl;
          if (match$4) {
            var rest = match$4.tl;
            if (Belt_List.length(rest) <= 1) {
              var years = Belt_List.head(rest);
              return {
                      minutes: parseSubExpr(match.hd, 0, 59, []),
                      hours: parseSubExpr(match$1.hd, 0, 23, []),
                      daysOfMonth: parseDaysOfMonthSubExpr(match$2.hd),
                      months: parseSubExpr(match$3.hd, 1, 12, monthNames),
                      daysOfWeek: parseDaysOfWeekSubExpr(match$4.hd),
                      years: years !== undefined ? parseYearsSubExpr(years) : "Wildcard",
                      expression: str
                    };
            }
            throw {
                  RE_EXN_ID: MalformedCronExpression,
                  Error: new Error()
                };
          }
          throw {
                RE_EXN_ID: MalformedCronExpression,
                Error: new Error()
              };
        }
        throw {
              RE_EXN_ID: MalformedCronExpression,
              Error: new Error()
            };
      }
      throw {
            RE_EXN_ID: MalformedCronExpression,
            Error: new Error()
          };
    }
    throw {
          RE_EXN_ID: MalformedCronExpression,
          Error: new Error()
        };
  }
  throw {
        RE_EXN_ID: MalformedCronExpression,
        Error: new Error()
      };
}

function tryParse(str) {
  try {
    return parse(str);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === MalformedCronExpression) {
      return ;
    }
    throw exn;
  }
}

exports.MalformedCronExpression = MalformedCronExpression;
exports.parse = parse;
exports.tryParse = tryParse;
/* monthNames Not a pure module */
