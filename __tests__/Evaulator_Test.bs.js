// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.bs.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Random = require("bs-platform/lib/js/random.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Belt_Range = require("bs-platform/lib/js/belt_Range.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var NactCron_Evaluators = require("../src/NactCron_Evaluators.bs.js");

var daysInMonths = {
  hd: [
    1,
    31
  ],
  tl: {
    hd: [
      2,
      28
    ],
    tl: {
      hd: [
        2,
        29
      ],
      tl: {
        hd: [
          3,
          31
        ],
        tl: {
          hd: [
            4,
            30
          ],
          tl: {
            hd: [
              5,
              31
            ],
            tl: {
              hd: [
                6,
                30
              ],
              tl: {
                hd: [
                  7,
                  31
                ],
                tl: {
                  hd: [
                    8,
                    31
                  ],
                  tl: {
                    hd: [
                      9,
                      30
                    ],
                    tl: {
                      hd: [
                        10,
                        31
                      ],
                      tl: {
                        hd: [
                          11,
                          30
                        ],
                        tl: {
                          hd: [
                            12,
                            31
                          ],
                          tl: /* [] */0
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
};

function intToDay(param) {
  switch (param) {
    case 0 :
        return "Sunday";
    case 1 :
        return "Monday";
    case 2 :
        return "Tuesday";
    case 3 :
        return "Wednesday";
    case 4 :
        return "Thursday";
    case 5 :
        return "Friday";
    case 6 :
        return "Saturday";
    default:
      return "???";
  }
}

function randomTime(start, end_, step) {
  return Math.imul(Random.$$int(Caml_int32.div(end_ - start | 0, step) + 1 | 0), step) + start | 0;
}

function testRandomValues(size, lowerBound, upperBound, f) {
  var values = Belt_Array.map(Belt_Array.range(0, size), (function (param) {
            return randomTime(lowerBound, upperBound, 1);
          })).sort();
  var interval = Belt_List.fromArray(Belt_Array.range(lowerBound, upperBound));
  var match = Belt_List.partition(interval, (function (i) {
          return Belt_Array.some(values, (function (j) {
                        return i === j;
                      }));
        }));
  var inValuesTest = function (i) {
    return Jest.test("Time " + (String(i) + " which is in the set of supplied values should evaluate to true"), (function (param) {
                  return Jest.ExpectJs.toEqual(true, Jest.ExpectJs.expect(Curry._2(f, i, {
                                      NAME: "Values",
                                      VAL: values
                                    })));
                }));
  };
  var outOfValuesTest = function (i) {
    return Jest.test("Time " + (String(i) + " which is not in the set of supplied values should evaluate to false"), (function (param) {
                  return Jest.ExpectJs.toEqual(false, Jest.ExpectJs.expect(Curry._2(f, i, {
                                      NAME: "Values",
                                      VAL: values
                                    })));
                }));
  };
  Belt_List.forEach(match[0], inValuesTest);
  return Belt_List.forEach(match[1], outOfValuesTest);
}

function testIsInExpr(name, f, lowerBoundOpt, upperBound) {
  var lowerBound = lowerBoundOpt !== undefined ? lowerBoundOpt : 0;
  var randomTime$1 = function (startOpt, end_Opt, stepOpt, param) {
    var start = startOpt !== undefined ? startOpt : lowerBound;
    var end_ = end_Opt !== undefined ? end_Opt : upperBound;
    var step = stepOpt !== undefined ? stepOpt : 1;
    return randomTime(start, end_, step);
  };
  var testRandomInterval = function (param) {
    var timeA = randomTime$1(undefined, undefined, undefined, undefined);
    var timeB = randomTime$1(undefined, undefined, undefined, undefined);
    var start = timeA < timeB ? timeA : timeB;
    var end_ = timeA > timeB ? timeA : timeB;
    var step = Random.$$int(10) + 1 | 0;
    var interval = Belt_Array.rangeBy(start, end_, step);
    var inIntervalTest = function (i) {
      return Jest.test("Time " + (String(i) + (" which is inside the interval [" + (String(start) + (", " + (String(end_) + ("] with a step size of " + (String(step) + " should evaluate to true"))))))), (function (param) {
                    return Jest.ExpectJs.toEqual(true, Jest.ExpectJs.expect(Curry._2(f, i, {
                                        NAME: "Interval",
                                        VAL: [
                                          start,
                                          end_,
                                          step
                                        ]
                                      })));
                  }));
    };
    var outOfIntervalTest = function (i) {
      return Jest.test("Time " + (String(i) + (" which is outside of the interval [" + (String(start) + (", " + (String(end_) + ("] with a step size of " + (String(step) + " should evaluate to false"))))))), (function (param) {
                    return Jest.ExpectJs.toEqual(false, Jest.ExpectJs.expect(Curry._2(f, i, {
                                        NAME: "Interval",
                                        VAL: [
                                          start,
                                          end_,
                                          step
                                        ]
                                      })));
                  }));
    };
    Belt_Array.forEach(interval, inIntervalTest);
    return Belt_Array.forEach(Belt_Array.keep(Belt_Array.range(start, end_), (function (i) {
                      return !Belt_Array.some(interval, (function (j) {
                                    return i === j;
                                  }));
                    })), outOfIntervalTest);
  };
  return Jest.describe(name, (function (param) {
                Jest.describe("`Wildcard", (function (param) {
                        return Jest.test("`Wildcard always evaluate to true", (function (param) {
                                      return Jest.ExpectJs.toEqual(true, Jest.ExpectJs.expect(Curry._2(f, randomTime$1(undefined, undefined, undefined, undefined), "Wildcard")));
                                    }));
                      }));
                Jest.describe("`Interval", (function (param) {
                        return Belt_Range.forEach(0, 3, testRandomInterval);
                      }));
                return Jest.describe("`Values", (function (param) {
                              return Belt_Range.forEach(0, 4, (function (__x) {
                                            return testRandomValues(__x, lowerBound, upperBound, f);
                                          }));
                            }));
              }));
}

testIsInExpr("isInHour", NactCron_Evaluators.isInHour, 0, 23);

testIsInExpr("isInMonth", NactCron_Evaluators.isInMonth, 0, 23);

Jest.describe("isInYear", (function (param) {
        var testRandomInterval = function (openStartOpt, openEndOpt, param) {
          var openStart = openStartOpt !== undefined ? openStartOpt : false;
          var openEnd = openEndOpt !== undefined ? openEndOpt : false;
          var timeA = randomTime(0, 3000, 1);
          var timeB = randomTime(0, 3000, 1);
          var intervalStart = openStart ? undefined : (
              timeA < timeB ? timeA : timeB
            );
          var intervalEnd = openEnd ? undefined : (
              timeA > timeB ? timeA : timeB
            );
          var step = Random.$$int(10) + 1 | 0;
          var interval = Belt_Array.rangeBy(Belt_Option.getWithDefault(intervalStart, 0), Belt_Option.getWithDefault(intervalEnd, 3000), step);
          var inIntervalTest = function (i) {
            return Jest.test("Time " + (String(i) + (" which is inside the interval [" + (Belt_Option.getWithDefault(JSON.stringify(intervalStart), "") + (", " + (Belt_Option.getWithDefault(JSON.stringify(intervalEnd), "") + ("] with a step size of " + (String(step) + " should evaluate to true"))))))), (function (param) {
                          return Jest.ExpectJs.toEqual(true, Jest.ExpectJs.expect(NactCron_Evaluators.isInYear(i, {
                                              NAME: "Interval",
                                              VAL: [
                                                intervalStart,
                                                intervalEnd,
                                                step
                                              ]
                                            })));
                        }));
          };
          var outOfIntervalTest = function (i) {
            return Jest.test("Time " + (String(i) + (" which is outside of the interval [" + (Belt_Option.getWithDefault(JSON.stringify(intervalStart), "") + (", " + (Belt_Option.getWithDefault(JSON.stringify(intervalEnd), "") + ("] with a step size of " + (String(step) + " should evaluate to false"))))))), (function (param) {
                          return Jest.ExpectJs.toEqual(false, Jest.ExpectJs.expect(NactCron_Evaluators.isInYear(i, {
                                              NAME: "Interval",
                                              VAL: [
                                                intervalStart,
                                                intervalEnd,
                                                step
                                              ]
                                            })));
                        }));
          };
          Belt_Array.forEach(interval, inIntervalTest);
          return Belt_Array.forEach(Belt_Array.keep(interval, (function (i) {
                            return !Belt_Array.some(interval, (function (j) {
                                          return i === j;
                                        }));
                          })), outOfIntervalTest);
        };
        Jest.describe("`Wildcard", (function (param) {
                return Jest.test("`Wildcard always evaluate to true", (function (param) {
                              return Jest.ExpectJs.toEqual(true, Jest.ExpectJs.expect(NactCron_Evaluators.isInYear(Random.$$int(3000), "Wildcard")));
                            }));
              }));
        Jest.describe("`Values", (function (param) {
                return Belt_Range.forEach(0, 4, (function (__x) {
                              return testRandomValues(__x, 2000, 3000, NactCron_Evaluators.isInYear);
                            }));
              }));
        return Jest.describe("`Interval", (function (param) {
                      var partial_arg = true;
                      Belt_Range.forEach(0, 2, (function (eta) {
                              var param;
                              return testRandomInterval(partial_arg, param, eta);
                            }));
                      Belt_Range.forEach(0, 2, (function (eta) {
                              var partial_arg = true;
                              return testRandomInterval(undefined, partial_arg, eta);
                            }));
                      var partial_arg$1 = true;
                      var partial_arg$2 = true;
                      Belt_Range.forEach(0, 2, (function (param) {
                              return testRandomInterval(partial_arg$2, partial_arg$1, param);
                            }));
                      return Belt_Range.forEach(0, 2, (function (eta) {
                                    return testRandomInterval(undefined, undefined, eta);
                                  }));
                    }));
      }));

function testWildcard(f) {
  return Jest.describe("`Wildcard", (function (param) {
                return Jest.test("`Wildcard always evaluate to true", (function (param) {
                              var daysInMonth = 28 + Random.$$int(4) | 0;
                              return Jest.ExpectJs.toEqual(true, Jest.ExpectJs.expect(Curry._4(f, daysInMonth, Random.$$int(daysInMonth + 1 | 0), Random.$$int(7), "Wildcard")));
                            }));
              }));
}

Jest.describe("isInDayOfWeek", (function (param) {
        var testRandomValues = function (size) {
          var values = Belt_Array.map(Belt_Array.range(0, size), (function (param) {
                    return Random.$$int(7);
                  })).sort();
          var interval = Belt_List.fromArray(Belt_Array.range(0, 6));
          var match = Belt_List.partition(interval, (function (i) {
                  return Belt_Array.some(values, (function (j) {
                                return i === j;
                              }));
                }));
          var inValuesTest = function (i) {
            var daysInMonth = 28 + Random.$$int(4) | 0;
            return Jest.test("Time " + (String(i) + " which is in the set of supplied values should evaluate to true"), (function (param) {
                          return Jest.ExpectJs.toEqual(true, Jest.ExpectJs.expect(NactCron_Evaluators.isInDayOfWeek(Random.$$int(daysInMonth + 1 | 0), i, daysInMonth, {
                                              NAME: "Values",
                                              VAL: values
                                            })));
                        }));
          };
          var outOfValuesTest = function (i) {
            var daysInMonth = 28 + Random.$$int(4) | 0;
            return Jest.test("Time " + (String(i) + " which is in the set of supplied values should evaluate to true"), (function (param) {
                          return Jest.ExpectJs.toEqual(false, Jest.ExpectJs.expect(NactCron_Evaluators.isInDayOfWeek(Random.$$int(daysInMonth + 1 | 0), i, daysInMonth, {
                                              NAME: "Values",
                                              VAL: values
                                            })));
                        }));
          };
          Belt_List.forEach(match[0], inValuesTest);
          return Belt_List.forEach(match[1], outOfValuesTest);
        };
        var testRandomInterval = function (param) {
          var timeA = randomTime(0, 6, 1);
          var timeB = randomTime(0, 6, 1);
          var start = timeA < timeB ? timeA : timeB;
          var end_ = timeA > timeB ? timeA : timeB;
          var step = Random.$$int(4) + 1 | 0;
          var interval = Belt_Array.rangeBy(start, end_, step);
          var inIntervalTest = function (i) {
            return Jest.test("Time " + (String(i) + (" which is inside the interval [" + (String(start) + (", " + (String(end_) + ("] with a step size of " + (String(step) + " should evaluate to true"))))))), (function (param) {
                          var daysInMonth = 28 + Random.$$int(4) | 0;
                          return Jest.ExpectJs.toEqual(true, Jest.ExpectJs.expect(NactCron_Evaluators.isInDayOfWeek(Random.$$int(daysInMonth + 1 | 0), i, daysInMonth, {
                                              NAME: "Interval",
                                              VAL: [
                                                start,
                                                end_,
                                                step
                                              ]
                                            })));
                        }));
          };
          var outOfIntervalTest = function (i) {
            return Jest.test("Time " + (String(i) + (" which is outside of the interval [" + (String(start) + (", " + (String(end_) + ("] with a step size of " + (String(step) + " should evaluate to false"))))))), (function (param) {
                          var daysInMonth = 28 + Random.$$int(4) | 0;
                          return Jest.ExpectJs.toEqual(false, Jest.ExpectJs.expect(NactCron_Evaluators.isInDayOfWeek(Random.$$int(daysInMonth + 1 | 0), i, daysInMonth, {
                                              NAME: "Interval",
                                              VAL: [
                                                start,
                                                end_,
                                                step
                                              ]
                                            })));
                        }));
          };
          Belt_Array.forEach(interval, inIntervalTest);
          return Belt_Array.forEach(Belt_Array.keep(Belt_Array.range(start, end_), (function (i) {
                            return !Belt_Array.some(interval, (function (j) {
                                          return i === j;
                                        }));
                          })), outOfIntervalTest);
        };
        var testNthDayOfWeekInMonth = function (dayOfWeek) {
          var offset = Random.$$int(7);
          Jest.test("2nd " + (intToDay(dayOfWeek) + (" of the month should evaluate to false as expecting the 3rd " + intToDay(dayOfWeek))), (function (param) {
                  var __x = Jest.ExpectJs.expect(NactCron_Evaluators.isInDayOfWeek(offset + 7 | 0, dayOfWeek, 31, {
                            NAME: "NthDayOfWeekInMonth",
                            VAL: [
                              dayOfWeek,
                              3
                            ]
                          }));
                  return Jest.ExpectJs.toEqual(false, __x);
                }));
          Jest.test("3rd " + (intToDay(dayOfWeek) + (" of the month should evaluate to true as expecting the 3rd " + intToDay(dayOfWeek))), (function (param) {
                  var __x = Jest.ExpectJs.expect(NactCron_Evaluators.isInDayOfWeek(offset + 14 | 0, dayOfWeek, 31, {
                            NAME: "NthDayOfWeekInMonth",
                            VAL: [
                              dayOfWeek,
                              3
                            ]
                          }));
                  return Jest.ExpectJs.toEqual(true, __x);
                }));
          return Jest.test("3rd " + (intToDay((dayOfWeek + 1 | 0) % 7) + (" of the month should evaluate to false as expecting the 3rd " + intToDay(dayOfWeek))), (function (param) {
                        var __x = Jest.ExpectJs.expect(NactCron_Evaluators.isInDayOfWeek((offset + 14 | 0) + 1 | 0, (dayOfWeek + 1 | 0) % 7, 31, {
                                  NAME: "NthDayOfWeekInMonth",
                                  VAL: [
                                    dayOfWeek,
                                    3
                                  ]
                                }));
                        return Jest.ExpectJs.toEqual(false, __x);
                      }));
        };
        var testLastDayOfWeekInMonth = function (dayOfWeek) {
          var offset = Random.$$int(7);
          var lengthOfMonth = 28 + Random.$$int(4) | 0;
          var lastDayOfWeekInMonth = lengthOfMonth - offset | 0;
          var secondLastDayOfWeekInMonth = (lengthOfMonth - offset | 0) - 7 | 0;
          Jest.test("The last " + (intToDay(dayOfWeek) + " of the month should evaluate to true"), (function (param) {
                  var __x = Jest.ExpectJs.expect(NactCron_Evaluators.isInDayOfWeek(lastDayOfWeekInMonth, dayOfWeek, lengthOfMonth, {
                            NAME: "LastDayOfWeekInMonth",
                            VAL: dayOfWeek
                          }));
                  return Jest.ExpectJs.toEqual(true, __x);
                }));
          Jest.test("The last " + (intToDay((dayOfWeek + 1 | 0) % 7) + (" of the month should evaluate to false as expecting the last " + intToDay(dayOfWeek))), (function (param) {
                  var __x = Jest.ExpectJs.expect(NactCron_Evaluators.isInDayOfWeek(lastDayOfWeekInMonth, (dayOfWeek + 1 | 0) % 7, lengthOfMonth, {
                            NAME: "LastDayOfWeekInMonth",
                            VAL: dayOfWeek
                          }));
                  return Jest.ExpectJs.toEqual(false, __x);
                }));
          Jest.test("The second last " + (intToDay(dayOfWeek) + " of the month should evaluate to false"), (function (param) {
                  var __x = Jest.ExpectJs.expect(NactCron_Evaluators.isInDayOfWeek(secondLastDayOfWeekInMonth, dayOfWeek, lengthOfMonth, {
                            NAME: "LastDayOfWeekInMonth",
                            VAL: dayOfWeek
                          }));
                  return Jest.ExpectJs.toEqual(false, __x);
                }));
          Jest.test("The last " + (intToDay((dayOfWeek + 1 | 0) % 7) + (" of the month should evaluate to false as expecting the last " + intToDay(dayOfWeek))), (function (param) {
                  var __x = Jest.ExpectJs.expect(NactCron_Evaluators.isInDayOfWeek((secondLastDayOfWeekInMonth + 1 | 0) % 7, dayOfWeek, lengthOfMonth, {
                            NAME: "LastDayOfWeekInMonth",
                            VAL: dayOfWeek
                          }));
                  return Jest.ExpectJs.toEqual(false, __x);
                }));
          return Jest.test("The second last " + (intToDay(dayOfWeek) + " of the month should evaluate to false"), (function (param) {
                        var __x = Jest.ExpectJs.expect(NactCron_Evaluators.isInDayOfWeek(secondLastDayOfWeekInMonth, dayOfWeek, lengthOfMonth, {
                                  NAME: "LastDayOfWeekInMonth",
                                  VAL: dayOfWeek
                                }));
                        return Jest.ExpectJs.toEqual(false, __x);
                      }));
        };
        testWildcard(function (daysInMonth, dayOfMonth, dayOfWeek, daysOfWeek) {
              return NactCron_Evaluators.isInDayOfWeek(dayOfMonth, dayOfWeek, daysInMonth, daysOfWeek);
            });
        Jest.describe("`LastDayOfWeekInMonth", (function (param) {
                return Belt_Range.forEach(0, 6, testLastDayOfWeekInMonth);
              }));
        Jest.describe("`NthDayOfWeekInMonth", (function (param) {
                return Belt_Range.forEach(0, 6, testNthDayOfWeekInMonth);
              }));
        Jest.describe("`Values", (function (param) {
                return Belt_Range.forEach(0, 4, testRandomValues);
              }));
        return Jest.describe("`Interval", (function (param) {
                      return Belt_Range.forEach(0, 4, testRandomInterval);
                    }));
      }));

Jest.describe("isInDayOfMonth", (function (param) {
        var testRandomInterval = function (param) {
          var timeA = randomTime(1, 31, 1);
          var timeB = randomTime(1, 31, 1);
          var start = timeA < timeB ? timeA : timeB;
          var end_ = timeA > timeB ? timeA : timeB;
          var step = Random.$$int(10) + 1 | 0;
          var interval = Belt_Array.rangeBy(start, end_, step);
          var inIntervalTest = function (i) {
            return Jest.test("Time " + (String(i) + (" which is inside the interval [" + (String(start) + (", " + (String(end_) + ("] with a step size of " + (String(step) + " should evaluate to true"))))))), (function (param) {
                          var daysInMonth = 28 + Random.$$int(4) | 0;
                          var daysInMonth$1 = i > daysInMonth ? i : daysInMonth;
                          return Jest.ExpectJs.toEqual(true, Jest.ExpectJs.expect(NactCron_Evaluators.isInDayOfMonth(i, Random.$$int(7), daysInMonth$1, {
                                              NAME: "Interval",
                                              VAL: [
                                                start,
                                                end_,
                                                step
                                              ]
                                            })));
                        }));
          };
          var outOfIntervalTest = function (i) {
            return Jest.test("Time " + (String(i) + (" which is outside of the interval [" + (String(start) + (", " + (String(end_) + ("] with a step size of " + (String(step) + " should evaluate to false"))))))), (function (param) {
                          var daysInMonth = 28 + Random.$$int(4) | 0;
                          var daysInMonth$1 = i > daysInMonth ? i : daysInMonth;
                          return Jest.ExpectJs.toEqual(false, Jest.ExpectJs.expect(NactCron_Evaluators.isInDayOfMonth(i, Random.$$int(7), daysInMonth$1, {
                                              NAME: "Interval",
                                              VAL: [
                                                start,
                                                end_,
                                                step
                                              ]
                                            })));
                        }));
          };
          Belt_Array.forEach(interval, inIntervalTest);
          return Belt_Array.forEach(Belt_Array.keep(Belt_Array.range(start, end_), (function (i) {
                            return !Belt_Array.some(interval, (function (j) {
                                          return i === j;
                                        }));
                          })), outOfIntervalTest);
        };
        var testRandomValues = function (size) {
          var values = Belt_Array.map(Belt_Array.range(0, size), (function (param) {
                    return Random.$$int(31);
                  })).sort();
          var interval = Belt_List.fromArray(Belt_Array.range(1, 31));
          var match = Belt_List.partition(interval, (function (i) {
                  return Belt_Array.some(values, (function (j) {
                                return i === j;
                              }));
                }));
          var inValuesTest = function (i) {
            var daysInMonth = 28 + Random.$$int(4) | 0;
            var daysInMonth$1 = i > daysInMonth ? i : daysInMonth;
            return Jest.test("Time " + (String(i) + " which is in the set of supplied values should evaluate to true"), (function (param) {
                          return Jest.ExpectJs.toEqual(true, Jest.ExpectJs.expect(NactCron_Evaluators.isInDayOfMonth(i, Random.$$int(7), daysInMonth$1, {
                                              NAME: "Values",
                                              VAL: values
                                            })));
                        }));
          };
          var outOfValuesTest = function (i) {
            var daysInMonth = 28 + Random.$$int(4) | 0;
            var daysInMonth$1 = i > daysInMonth ? i : daysInMonth;
            return Jest.test("Time " + (String(i) + " which is in the set of supplied values should evaluate to true"), (function (param) {
                          return Jest.ExpectJs.toEqual(false, Jest.ExpectJs.expect(NactCron_Evaluators.isInDayOfMonth(i, Random.$$int(7), daysInMonth$1, {
                                              NAME: "Values",
                                              VAL: values
                                            })));
                        }));
          };
          Belt_List.forEach(match[0], inValuesTest);
          return Belt_List.forEach(match[1], outOfValuesTest);
        };
        var testNthLastDayOfMonth = function (n) {
          Belt_List.forEach(daysInMonths, (function (param) {
                  var daysInMonth = param[1];
                  return Jest.test("The " + (String(n) + ("th last day before end of month " + (String(param[0]) + " should evaluate to true"))), (function (param) {
                                var dayOfMonth = daysInMonth - n | 0;
                                return Jest.ExpectJs.toEqual(true, Jest.ExpectJs.expect(NactCron_Evaluators.isInDayOfMonth(dayOfMonth, Random.$$int(7), daysInMonth, {
                                                    NAME: "DaysBeforeEndOfMonth",
                                                    VAL: n
                                                  })));
                              }));
                }));
          return Belt_List.forEach(daysInMonths, (function (param) {
                        var daysInMonth = param[1];
                        return Jest.test("Days other than the " + (String(n) + ("th last day before end of month " + (String(param[0]) + " should evaluate to false"))), (function (param) {
                                      var dayOfMonth = Caml_int32.mod_(Random.$$int(daysInMonth) + (daysInMonth - n | 0) | 0, daysInMonth + 1 | 0) + 1 | 0;
                                      return Jest.ExpectJs.toEqual(false, Jest.ExpectJs.expect(NactCron_Evaluators.isInDayOfMonth(dayOfMonth, Random.$$int(7), daysInMonth, {
                                                          NAME: "DaysBeforeEndOfMonth",
                                                          VAL: n
                                                        })));
                                    }));
                      }));
        };
        Jest.describe("`NearestWeekdayToDay", (function (param) {
                var testRandomWeekday = function (param) {
                  var dayOfWeek = Random.$$int(5) + 1 | 0;
                  var daysInMonth = 28 + Random.$$int(4) | 0;
                  var dayOfMonth = Random.$$int(daysInMonth + 1 | 0);
                  return Jest.test(intToDay(dayOfWeek) + (", " + (String(dayOfMonth) + (" should evaluate to true as it is the nearest  weekday to day " + (String(dayOfMonth) + " of the month")))), (function (param) {
                                return Jest.ExpectJs.toEqual(true, Jest.ExpectJs.expect(NactCron_Evaluators.isInDayOfMonth(dayOfMonth, dayOfWeek, daysInMonth, {
                                                    NAME: "NearestWeekday",
                                                    VAL: dayOfMonth
                                                  })));
                              }));
                };
                var testRandomWeekend = function (param) {
                  var daysInMonth = Random.$$int(4) + 28 | 0;
                  var dayOfMonth = Random.$$int(daysInMonth + 1 | 0);
                  return Jest.test("Weekend on day " + (String(dayOfMonth) + " of the month should evaluate to false as while it's the correct day of month, it's not a weekday."), (function (param) {
                                return Jest.ExpectJs.toEqual(false, Jest.ExpectJs.expect(NactCron_Evaluators.isInDayOfMonth(dayOfMonth, Random.$$int(2) === 1 ? 6 : 0, daysInMonth, {
                                                    NAME: "NearestWeekday",
                                                    VAL: dayOfMonth
                                                  })));
                              }));
                };
                Jest.test("A Friday two days before the end of the month when targeting the nearest weekday to the last day of the a specific month should evaluate to true", (function (param) {
                        var daysInMonth = Random.$$int(4) + 28 | 0;
                        return Jest.ExpectJs.toEqual(true, Jest.ExpectJs.expect(NactCron_Evaluators.isInDayOfMonth(daysInMonth - 2 | 0, 5, daysInMonth, {
                                            NAME: "NearestWeekday",
                                            VAL: daysInMonth
                                          })));
                      }));
                Jest.test("A Friday three days before the end of the month when targeting the nearest weekday to the last day of the a specific month should evaluate to false", (function (param) {
                        var daysInMonth = Random.$$int(4) + 28 | 0;
                        return Jest.ExpectJs.toEqual(false, Jest.ExpectJs.expect(NactCron_Evaluators.isInDayOfMonth(daysInMonth - 3 | 0, 5, daysInMonth, {
                                            NAME: "NearestWeekday",
                                            VAL: daysInMonth
                                          })));
                      }));
                Jest.test("A Monday the day after the scheduled day in the same month should evaluate to true as it's the nearest weekday", (function (param) {
                        var daysInMonth = Random.$$int(4) + 28 | 0;
                        var dayOfMonth = Random.$$int(daysInMonth);
                        return Jest.ExpectJs.toEqual(true, Jest.ExpectJs.expect(NactCron_Evaluators.isInDayOfMonth(dayOfMonth, 1, daysInMonth, {
                                            NAME: "NearestWeekday",
                                            VAL: dayOfMonth - 1 | 0
                                          })));
                      }));
                Jest.test("A Friday the day before the scheduled day in the same month should evaluate to true as it's the nearest weekday", (function (param) {
                        var daysInMonth = Random.$$int(4) + 28 | 0;
                        var scheduledDayOfMonth = Random.$$int(daysInMonth);
                        return Jest.ExpectJs.toEqual(true, Jest.ExpectJs.expect(NactCron_Evaluators.isInDayOfMonth(scheduledDayOfMonth - 1 | 0, 5, daysInMonth, {
                                            NAME: "NearestWeekday",
                                            VAL: scheduledDayOfMonth
                                          })));
                      }));
                Belt_Array.forEach(Belt_Array.range(0, 3), testRandomWeekday);
                return Belt_Array.forEach(Belt_Array.range(0, 3), testRandomWeekend);
              }));
        testWildcard(function (daysInMonth, dayOfMonth, dayOfWeek, daysOfWeek) {
              return NactCron_Evaluators.isInDayOfMonth(dayOfMonth, dayOfWeek, daysInMonth, daysOfWeek);
            });
        Jest.describe("`LastWeekdayOfMonth", (function (param) {
                Jest.test("A weekday at the end of the month should always evaluate to true", (function (param) {
                        var daysInMonth = Random.$$int(4) + 28 | 0;
                        return Jest.ExpectJs.toEqual(true, Jest.ExpectJs.expect(NactCron_Evaluators.isInDayOfMonth(daysInMonth, Random.$$int(5) + 1 | 0, daysInMonth, "LastWeekdayOfMonth")));
                      }));
                Jest.test("A weekend at the end of the month should always evaluate to false", (function (param) {
                        var daysInMonth = Random.$$int(4) + 28 | 0;
                        return Jest.ExpectJs.toEqual(false, Jest.ExpectJs.expect(NactCron_Evaluators.isInDayOfMonth(daysInMonth, (Random.$$int(2) + 6 | 0) % 7, daysInMonth, "LastWeekdayOfMonth")));
                      }));
                Jest.test("A Friday the day before the end of the month should always evaluate to true", (function (param) {
                        var daysInMonth = Random.$$int(4) + 28 | 0;
                        return Jest.ExpectJs.toEqual(true, Jest.ExpectJs.expect(NactCron_Evaluators.isInDayOfMonth(daysInMonth - 1 | 0, 5, daysInMonth, "LastWeekdayOfMonth")));
                      }));
                return Jest.test("A Friday two days before the end of the month should always evaluate to true", (function (param) {
                              var daysInMonth = Random.$$int(4) + 28 | 0;
                              return Jest.ExpectJs.toEqual(true, Jest.ExpectJs.expect(NactCron_Evaluators.isInDayOfMonth(daysInMonth - 2 | 0, 5, daysInMonth, "LastWeekdayOfMonth")));
                            }));
              }));
        Jest.describe("`Values", (function (param) {
                return Belt_Range.forEach(0, 4, testRandomValues);
              }));
        Jest.describe("`DaysBeforeEndOfMonth", (function (param) {
                return Belt_Range.forEach(0, 4, testNthLastDayOfMonth);
              }));
        return Jest.describe("`Interval", (function (param) {
                      return Belt_Range.forEach(0, 4, testRandomInterval);
                    }));
      }));

var Evaluators;

exports.Evaluators = Evaluators;
exports.daysInMonths = daysInMonths;
exports.intToDay = intToDay;
exports.randomTime = randomTime;
exports.testRandomValues = testRandomValues;
exports.testIsInExpr = testIsInExpr;
exports.testWildcard = testWildcard;
/*  Not a pure module */
