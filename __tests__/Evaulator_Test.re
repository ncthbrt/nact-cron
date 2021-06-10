open Jest;

open ExpectJs;

open NactCron_Evaluators;

module Evaluators = NactCron_Evaluators;

let daysInMonths: list((int, int)) = [
  (1, 31), /* JAN */
  (2, 28), /* FEB */
  (2, 29), /* FEB */
  (3, 31), /* MAR */
  (4, 30), /* APR */
  (5, 31), /* MAY */
  (6, 30), /* JUN */
  (7, 31), /* JUL */
  (8, 31), /* AUG*/
  (9, 30), /* SEP */
  (10, 31), /* OCT */
  (11, 30), /* NOV */
  (12, 31) /* DEC */
];

let intToDay =
  fun
  | 0 => "Sunday"
  | 1 => "Monday"
  | 2 => "Tuesday"
  | 3 => "Wednesday"
  | 4 => "Thursday"
  | 5 => "Friday"
  | 6 => "Saturday"
  | _ => "???";

let randomTime = (~start, ~end_, ~step) =>
  Random.int((end_ - start) / step + 1) * step + start;

let testRandomValues = (size, lowerBound, upperBound, f) => {
  let values =
    Belt.Array.range(0, size)
    |. Belt.Array.map((_) =>
         randomTime(~start=lowerBound, ~end_=upperBound, ~step=1)
       )
    |. Js.Array.sortInPlace;
  let interval =
    Belt.Array.range(lowerBound, upperBound) |. Belt.List.fromArray;
  let (inValues, outOfValues) =
    Belt.List.partition(interval, i => Belt.Array.some(values, j => i == j));
  let inValuesTest = i =>
    test(
      "Time "
      ++ string_of_int(i)
      ++ " which is in the set of supplied values should evaluate to true",
      () =>
      expect(f(i, `Values(values))) |> toEqual(true)
    );
  let outOfValuesTest = i =>
    test(
      "Time "
      ++ string_of_int(i)
      ++ " which is not in the set of supplied values should evaluate to false",
      () =>
      expect(f(i, `Values(values))) |> toEqual(false)
    );
  inValues |. Belt.List.forEach(inValuesTest);
  outOfValues |. Belt.List.forEach(outOfValuesTest);
};

let testIsInExpr = (name, f, ~lowerBound=0, ~upperBound) => {
  let randomTime = (~start=lowerBound, ~end_=upperBound, ~step=1, ()) =>
    randomTime(~start, ~end_, ~step);
  let testRandomInterval = (_) => {
    let timeA = randomTime();
    let timeB = randomTime();
    let start = min(timeA, timeB);
    let end_ = max(timeA, timeB);
    let step = Random.int(10) + 1;
    let interval = Belt.Array.rangeBy(start, end_, ~step);
    let inIntervalTest = i =>
      test(
        "Time "
        ++ string_of_int(i)
        ++ " which is inside the interval ["
        ++ string_of_int(start)
        ++ ", "
        ++ string_of_int(end_)
        ++ "] with a step size of "
        ++ string_of_int(step)
        ++ " should evaluate to true",
        () =>
        expect(f(i, `Interval((start, end_, step)))) |> toEqual(true)
      );
    let outOfIntervalTest = i =>
      test(
        "Time "
        ++ string_of_int(i)
        ++ " which is outside of the interval ["
        ++ string_of_int(start)
        ++ ", "
        ++ string_of_int(end_)
        ++ "] with a step size of "
        ++ string_of_int(step)
        ++ " should evaluate to false",
        () =>
        expect(f(i, `Interval((start, end_, step)))) |> toEqual(false)
      );
    interval |. Belt.Array.forEach(inIntervalTest);
    Belt.Array.range(start, end_)
    |. Belt.Array.keep(i => ! Belt.Array.some(interval, j => i == j))
    |. Belt.Array.forEach(outOfIntervalTest);
  };
  describe(
    name,
    () => {
      describe("`Wildcard", () =>
        test("`Wildcard always evaluate to true", () =>
          expect(f(randomTime(), `Wildcard)) |> toEqual(true)
        )
      );
      describe("`Interval", () =>
        Belt.Range.forEach(0, 3, testRandomInterval)
      );
      describe("`Values", () =>
        Belt.Range.forEach(
          0,
          4,
          testRandomValues(_, lowerBound, upperBound, f),
        )
      );
    },
  );
};

let _ = testIsInExpr("isInMinute", isInMinute, ~upperBound=59);

let _ = testIsInExpr("isInHour", isInHour, ~lowerBound=0, ~upperBound=23);

let _ = testIsInExpr("isInMonth", isInMonth, ~lowerBound=0, ~upperBound=23);

describe("isInYear", () => {
  let randomTime = () => randomTime(~start=0, ~end_=3000, ~step=1);
  let testRandomInterval = (~openStart=false, ~openEnd=false, _) => {
    let timeA = randomTime();
    let timeB = randomTime();
    let intervalStart = openStart ? None : Some(min(timeA, timeB));
    let intervalEnd = openEnd ? None : Some(max(timeA, timeB));
    let step = Random.int(10) + 1;
    let interval =
      Belt.Array.rangeBy(
        Belt.Option.getWithDefault(intervalStart, 0),
        Belt.Option.getWithDefault(intervalEnd, 3000),
        ~step,
      );
    let inIntervalTest = i =>
      test(
        "Time "
        ++ string_of_int(i)
        ++ " which is inside the interval ["
        ++ (
          Js.Json.stringifyAny(intervalStart)
          |. Belt.Option.getWithDefault("")
        )
        ++ ", "
        ++ (
          Js.Json.stringifyAny(intervalEnd) |. Belt.Option.getWithDefault("")
        )
        ++ "] with a step size of "
        ++ string_of_int(step)
        ++ " should evaluate to true",
        () =>
        expect(isInYear(i, `Interval((intervalStart, intervalEnd, step))))
        |> toEqual(true)
      );
    let outOfIntervalTest = i =>
      test(
        "Time "
        ++ string_of_int(i)
        ++ " which is outside of the interval ["
        ++ (
          Js.Json.stringifyAny(intervalStart)
          |. Belt.Option.getWithDefault("")
        )
        ++ ", "
        ++ (
          Js.Json.stringifyAny(intervalEnd) |. Belt.Option.getWithDefault("")
        )
        ++ "] with a step size of "
        ++ string_of_int(step)
        ++ " should evaluate to false",
        () =>
        expect(isInYear(i, `Interval((intervalStart, intervalEnd, step))))
        |> toEqual(false)
      );
    interval |. Belt.Array.forEach(inIntervalTest);
    interval
    |. Belt.Array.keep(i => ! Belt.Array.some(interval, j => i == j))
    |. Belt.Array.forEach(outOfIntervalTest);
  };
  describe("`Wildcard", () =>
    test("`Wildcard always evaluate to true", () =>
      expect(isInYear(Random.int(3000), `Wildcard)) |> toEqual(true)
    )
  );
  describe("`Values", () =>
    Belt.Range.forEach(0, 4, testRandomValues(_, 2000, 3000, isInYear))
  );
  describe("`Interval", () => {
    Belt.Range.forEach(0, 2, testRandomInterval(~openStart=true));
    Belt.Range.forEach(0, 2, testRandomInterval(~openEnd=true));
    Belt.Range.forEach(
      0,
      2,
      testRandomInterval(~openEnd=true, ~openStart=true),
    );
    Belt.Range.forEach(0, 2, testRandomInterval);
  });
});

let testWildcard = f =>
  describe("`Wildcard", () =>
    test("`Wildcard always evaluate to true", () => {
      let daysInMonth = 28 + Random.int(4);
      expect(
        f(
          daysInMonth,
          Random.int(daysInMonth + 1),
          Random.int(7),
          `Wildcard,
        ),
      )
      |> toEqual(true);
    })
  );

describe("isInDayOfWeek", () => {
  let randomTime = () => randomTime(~start=0, ~end_=6, ~step=1);
  let testRandomValues = size => {
    let values =
      Belt.Array.range(0, size)
      |. Belt.Array.map((_) => Random.int(7))
      |. Js.Array.sortInPlace;
    let interval = Belt.Array.range(0, 6) |. Belt.List.fromArray;
    let (inValues, outOfValues) =
      Belt.List.partition(interval, i => Belt.Array.some(values, j => i == j));
    let inValuesTest = i => {
      let daysInMonth = 28 + Random.int(4);
      test(
        "Time "
        ++ string_of_int(i)
        ++ " which is in the set of supplied values should evaluate to true",
        () =>
        expect(
          isInDayOfWeek(
            ~daysInMonth,
            ~dayOfMonth=Random.int(daysInMonth + 1),
            ~dayOfWeek=i,
            `Values(values),
          ),
        )
        |> toEqual(true)
      );
    };
    let outOfValuesTest = i => {
      let daysInMonth = 28 + Random.int(4);
      test(
        "Time "
        ++ string_of_int(i)
        ++ " which is in the set of supplied values should evaluate to true",
        () =>
        expect(
          isInDayOfWeek(
            ~daysInMonth,
            ~dayOfMonth=Random.int(daysInMonth + 1),
            ~dayOfWeek=i,
            `Values(values),
          ),
        )
        |> toEqual(false)
      );
    };
    inValues |. Belt.List.forEach(inValuesTest);
    outOfValues |. Belt.List.forEach(outOfValuesTest);
  };
  let testRandomInterval = (_) => {
    let timeA = randomTime();
    let timeB = randomTime();
    let start = min(timeA, timeB);
    let end_ = max(timeA, timeB);
    let step = Random.int(4) + 1;
    let interval = Belt.Array.rangeBy(start, end_, ~step);
    let inIntervalTest = i =>
      test(
        "Time "
        ++ string_of_int(i)
        ++ " which is inside the interval ["
        ++ string_of_int(start)
        ++ ", "
        ++ string_of_int(end_)
        ++ "] with a step size of "
        ++ string_of_int(step)
        ++ " should evaluate to true",
        () => {
          let daysInMonth = 28 + Random.int(4);
          expect(
            isInDayOfWeek(
              ~dayOfWeek=i,
              ~dayOfMonth=Random.int(daysInMonth + 1),
              ~daysInMonth,
              `Interval((start, end_, step)),
            ),
          )
          |> toEqual(true);
        },
      );
    let outOfIntervalTest = i =>
      test(
        "Time "
        ++ string_of_int(i)
        ++ " which is outside of the interval ["
        ++ string_of_int(start)
        ++ ", "
        ++ string_of_int(end_)
        ++ "] with a step size of "
        ++ string_of_int(step)
        ++ " should evaluate to false",
        () => {
          let daysInMonth = 28 + Random.int(4);
          expect(
            isInDayOfWeek(
              ~dayOfWeek=i,
              ~dayOfMonth=Random.int(daysInMonth + 1),
              ~daysInMonth,
              `Interval((start, end_, step)),
            ),
          )
          |> toEqual(false);
        },
      );
    interval |. Belt.Array.forEach(inIntervalTest);
    Belt.Array.range(start, end_)
    |. Belt.Array.keep(i => ! Belt.Array.some(interval, j => i == j))
    |. Belt.Array.forEach(outOfIntervalTest);
  };
  let testNthDayOfWeekInMonth = dayOfWeek => {
    let offset = Random.int(7);
    test(
      "2nd "
      ++ intToDay(dayOfWeek)
      ++ " of the month should evaluate to false as expecting the 3rd "
      ++ intToDay(dayOfWeek),
      () =>
      expect(
        isInDayOfWeek(
          ~daysInMonth=31,
          ~dayOfMonth=offset + 7,
          ~dayOfWeek,
          `NthDayOfWeekInMonth((dayOfWeek, 3)),
        ),
      )
      |. toEqual(false, _)
    );
    test(
      "3rd "
      ++ intToDay(dayOfWeek)
      ++ " of the month should evaluate to true as expecting the 3rd "
      ++ intToDay(dayOfWeek),
      () =>
      expect(
        isInDayOfWeek(
          ~daysInMonth=31,
          ~dayOfMonth=offset + 14,
          ~dayOfWeek,
          `NthDayOfWeekInMonth((dayOfWeek, 3)),
        ),
      )
      |. toEqual(true, _)
    );
    test(
      "3rd "
      ++ intToDay((dayOfWeek + 1) mod 7)
      ++ " of the month should evaluate to false as expecting the 3rd "
      ++ intToDay(dayOfWeek),
      () =>
      expect(
        isInDayOfWeek(
          ~daysInMonth=31,
          ~dayOfMonth=offset + 14 + 1,
          ~dayOfWeek=(dayOfWeek + 1) mod 7,
          `NthDayOfWeekInMonth((dayOfWeek, 3)),
        ),
      )
      |. toEqual(false, _)
    );
  };
  let testLastDayOfWeekInMonth = dayOfWeek => {
    let offset = Random.int(7);
    let lengthOfMonth = 28 + Random.int(4);
    let lastDayOfWeekInMonth = lengthOfMonth - offset;
    let secondLastDayOfWeekInMonth = lengthOfMonth - offset - 7;
    test(
      "The last "
      ++ intToDay(dayOfWeek)
      ++ " of the month should evaluate to true",
      () =>
      expect(
        isInDayOfWeek(
          ~daysInMonth=lengthOfMonth,
          ~dayOfMonth=lastDayOfWeekInMonth,
          ~dayOfWeek,
          `LastDayOfWeekInMonth(dayOfWeek),
        ),
      )
      |. toEqual(true, _)
    );
    test(
      "The last "
      ++ intToDay((dayOfWeek + 1) mod 7)
      ++ " of the month should evaluate to false as expecting the last "
      ++ intToDay(dayOfWeek),
      () =>
      expect(
        isInDayOfWeek(
          ~daysInMonth=lengthOfMonth,
          ~dayOfMonth=lastDayOfWeekInMonth,
          ~dayOfWeek=(dayOfWeek + 1) mod 7,
          `LastDayOfWeekInMonth(dayOfWeek),
        ),
      )
      |. toEqual(false, _)
    );
    test(
      "The second last "
      ++ intToDay(dayOfWeek)
      ++ " of the month should evaluate to false",
      () =>
      expect(
        isInDayOfWeek(
          ~daysInMonth=lengthOfMonth,
          ~dayOfMonth=secondLastDayOfWeekInMonth,
          ~dayOfWeek,
          `LastDayOfWeekInMonth(dayOfWeek),
        ),
      )
      |. toEqual(false, _)
    );
    test(
      "The last "
      ++ intToDay((dayOfWeek + 1) mod 7)
      ++ " of the month should evaluate to false as expecting the last "
      ++ intToDay(dayOfWeek),
      () =>
      expect(
        isInDayOfWeek(
          ~daysInMonth=lengthOfMonth,
          ~dayOfMonth=(secondLastDayOfWeekInMonth + 1) mod 7,
          ~dayOfWeek,
          `LastDayOfWeekInMonth(dayOfWeek),
        ),
      )
      |. toEqual(false, _)
    );
    test(
      "The second last "
      ++ intToDay(dayOfWeek)
      ++ " of the month should evaluate to false",
      () =>
      expect(
        isInDayOfWeek(
          ~daysInMonth=lengthOfMonth,
          ~dayOfMonth=secondLastDayOfWeekInMonth,
          ~dayOfWeek,
          `LastDayOfWeekInMonth(dayOfWeek),
        ),
      )
      |. toEqual(false, _)
    );
  };
  testWildcard((daysInMonth, dayOfMonth, dayOfWeek, daysOfWeek) =>
    isInDayOfWeek(~daysInMonth, ~dayOfMonth, ~dayOfWeek, daysOfWeek)
  );
  describe("`LastDayOfWeekInMonth", () =>
    Belt.Range.forEach(0, 6, testLastDayOfWeekInMonth)
  );
  describe("`NthDayOfWeekInMonth", () =>
    Belt.Range.forEach(0, 6, testNthDayOfWeekInMonth)
  );
  describe("`Values", () =>
    Belt.Range.forEach(0, 4, testRandomValues)
  );
  describe("`Interval", () =>
    Belt.Range.forEach(0, 4, testRandomInterval)
  );
});

describe("isInDayOfMonth", () => {
  let randomTime = () => randomTime(~start=1, ~end_=31, ~step=1);
  let testRandomInterval = (_) => {
    let timeA = randomTime();
    let timeB = randomTime();
    let start = min(timeA, timeB);
    let end_ = max(timeA, timeB);
    let step = Random.int(10) + 1;
    let interval = Belt.Array.rangeBy(start, end_, ~step);
    let inIntervalTest = i =>
      test(
        "Time "
        ++ string_of_int(i)
        ++ " which is inside the interval ["
        ++ string_of_int(start)
        ++ ", "
        ++ string_of_int(end_)
        ++ "] with a step size of "
        ++ string_of_int(step)
        ++ " should evaluate to true",
        () => {
          let daysInMonth = 28 + Random.int(4);
          let daysInMonth = i > daysInMonth ? i : daysInMonth;
          expect(
            isInDayOfMonth(
              ~dayOfWeek=Random.int(7),
              ~dayOfMonth=i,
              ~daysInMonth,
              `Interval((start, end_, step)),
            ),
          )
          |> toEqual(true);
        },
      );
    let outOfIntervalTest = i =>
      test(
        "Time "
        ++ string_of_int(i)
        ++ " which is outside of the interval ["
        ++ string_of_int(start)
        ++ ", "
        ++ string_of_int(end_)
        ++ "] with a step size of "
        ++ string_of_int(step)
        ++ " should evaluate to false",
        () => {
          let daysInMonth = 28 + Random.int(4);
          let daysInMonth = i > daysInMonth ? i : daysInMonth;
          expect(
            isInDayOfMonth(
              ~dayOfWeek=Random.int(7),
              ~dayOfMonth=i,
              ~daysInMonth,
              `Interval((start, end_, step)),
            ),
          )
          |> toEqual(false);
        },
      );
    interval |. Belt.Array.forEach(inIntervalTest);
    Belt.Array.range(start, end_)
    |. Belt.Array.keep(i => ! Belt.Array.some(interval, j => i == j))
    |. Belt.Array.forEach(outOfIntervalTest);
  };
  let testRandomValues = size => {
    let values =
      Belt.Array.range(0, size)
      |. Belt.Array.map((_) => Random.int(31))
      |. Js.Array.sortInPlace;
    let interval = Belt.Array.range(1, 31) |. Belt.List.fromArray;
    let (inValues, outOfValues) =
      Belt.List.partition(interval, i => Belt.Array.some(values, j => i == j));
    let inValuesTest = i => {
      let daysInMonth = 28 + Random.int(4);
      let daysInMonth = i > daysInMonth ? i : daysInMonth;
      test(
        "Time "
        ++ string_of_int(i)
        ++ " which is in the set of supplied values should evaluate to true",
        () =>
        expect(
          isInDayOfMonth(
            ~daysInMonth,
            ~dayOfWeek=Random.int(7),
            ~dayOfMonth=i,
            `Values(values),
          ),
        )
        |> toEqual(true)
      );
    };
    let outOfValuesTest = i => {
      let daysInMonth = 28 + Random.int(4);
      let daysInMonth = i > daysInMonth ? i : daysInMonth;
      test(
        "Time "
        ++ string_of_int(i)
        ++ " which is in the set of supplied values should evaluate to true",
        () =>
        expect(
          isInDayOfMonth(
            ~daysInMonth,
            ~dayOfWeek=Random.int(7),
            ~dayOfMonth=i,
            `Values(values),
          ),
        )
        |> toEqual(false)
      );
    };
    inValues |. Belt.List.forEach(inValuesTest);
    outOfValues |. Belt.List.forEach(outOfValuesTest);
  };
  let testNthLastDayOfMonth = n => {
    daysInMonths
    |. Belt.List.forEach(((currentMonth, daysInMonth)) =>
         test(
           "The "
           ++ string_of_int(n)
           ++ "th last day before end of month "
           ++ string_of_int(currentMonth)
           ++ " should evaluate to true",
           () => {
             let dayOfMonth = daysInMonth - n;
             expect(
               isInDayOfMonth(
                 ~daysInMonth,
                 ~dayOfMonth,
                 ~dayOfWeek=Random.int(7),
                 `DaysBeforeEndOfMonth(n),
               ),
             )
             |> toEqual(true);
           },
         )
       );
    daysInMonths
    |. Belt.List.forEach(((currentMonth, daysInMonth)) =>
         test(
           "Days other than the "
           ++ string_of_int(n)
           ++ "th last day before end of month "
           ++ string_of_int(currentMonth)
           ++ " should evaluate to false",
           () => {
             let dayOfMonth =
               (Random.int(daysInMonth) + (daysInMonth - n))
               mod (daysInMonth + 1)
               + 1;
             expect(
               isInDayOfMonth(
                 ~daysInMonth,
                 ~dayOfMonth,
                 ~dayOfWeek=Random.int(7),
                 `DaysBeforeEndOfMonth(n),
               ),
             )
             |> toEqual(false);
           },
         )
       );
  };
  describe("`NearestWeekdayToDay", () => {
    let testRandomWeekday = (_) => {
      let dayOfWeek = Random.int(5) + 1;
      let daysInMonth = 28 + Random.int(4);
      let dayOfMonth = Random.int(daysInMonth + 1);
      test(
        intToDay(dayOfWeek)
        ++ ", "
        ++ string_of_int(dayOfMonth)
        ++ " should evaluate to true as it is the nearest  weekday to day "
        ++ string_of_int(dayOfMonth)
        ++ " of the month",
        () =>
        expect(
          isInDayOfMonth(
            ~daysInMonth,
            ~dayOfWeek,
            ~dayOfMonth,
            `NearestWeekday(dayOfMonth),
          ),
        )
        |> toEqual(true)
      );
    };
    let testRandomWeekend = (_) => {
      let daysInMonth = Random.int(4) + 28;
      let dayOfMonth = Random.int(daysInMonth + 1);
      test(
        "Weekend on day "
        ++ string_of_int(dayOfMonth)
        ++ " of the month should evaluate to false as while it's the correct day of month, it's not a weekday.",
        () =>
        expect(
          isInDayOfMonth(
            ~daysInMonth,
            ~dayOfWeek=Random.int(2) == 1 ? 6 : 0,
            ~dayOfMonth,
            `NearestWeekday(dayOfMonth),
          ),
        )
        |> toEqual(false)
      );
    };
    test(
      "A Friday two days before the end of the month when targeting the nearest weekday to the last day of the a specific month should evaluate to true",
      () => {
        let daysInMonth = Random.int(4) + 28;
        expect(
          isInDayOfMonth(
            ~daysInMonth,
            ~dayOfWeek=5,
            ~dayOfMonth=daysInMonth - 2,
            `NearestWeekday(daysInMonth),
          ),
        )
        |> toEqual(true);
      },
    );
    test(
      "A Friday three days before the end of the month when targeting the nearest weekday to the last day of the a specific month should evaluate to false",
      () => {
        let daysInMonth = Random.int(4) + 28;
        expect(
          isInDayOfMonth(
            ~daysInMonth,
            ~dayOfWeek=5,
            ~dayOfMonth=daysInMonth - 3,
            `NearestWeekday(daysInMonth),
          ),
        )
        |> toEqual(false);
      },
    );
    test(
      "A Monday the day after the scheduled day in the same month should evaluate to true as it's the nearest weekday",
      () => {
        let daysInMonth = Random.int(4) + 28;
        let dayOfMonth = Random.int(daysInMonth);
        expect(
          isInDayOfMonth(
            ~daysInMonth,
            ~dayOfWeek=1,
            ~dayOfMonth,
            `NearestWeekday(dayOfMonth - 1),
          ),
        )
        |> toEqual(true);
      },
    );
    test(
      "A Friday the day before the scheduled day in the same month should evaluate to true as it's the nearest weekday",
      () => {
        let daysInMonth = Random.int(4) + 28;
        let scheduledDayOfMonth = Random.int(daysInMonth);
        expect(
          isInDayOfMonth(
            ~daysInMonth,
            ~dayOfWeek=5,
            ~dayOfMonth=scheduledDayOfMonth - 1,
            `NearestWeekday(scheduledDayOfMonth),
          ),
        )
        |> toEqual(true);
      },
    );
    Belt.Array.range(0, 3) |. Belt.Array.forEach(testRandomWeekday);
    Belt.Array.range(0, 3) |. Belt.Array.forEach(testRandomWeekend);
  });
  testWildcard((daysInMonth, dayOfMonth, dayOfWeek, daysOfWeek) =>
    isInDayOfMonth(~daysInMonth, ~dayOfMonth, ~dayOfWeek, daysOfWeek)
  );
  describe("`LastWeekdayOfMonth", () => {
    test(
      "A weekday at the end of the month should always evaluate to true", () => {
      let daysInMonth = Random.int(4) + 28;
      expect(
        isInDayOfMonth(
          ~daysInMonth,
          ~dayOfWeek=Random.int(5) + 1,
          ~dayOfMonth=daysInMonth,
          `LastWeekdayOfMonth,
        ),
      )
      |> toEqual(true);
    });
    test(
      "A weekend at the end of the month should always evaluate to false", () => {
      let daysInMonth = Random.int(4) + 28;
      expect(
        isInDayOfMonth(
          ~daysInMonth,
          ~dayOfWeek=(Random.int(2) + 6) mod 7,
          ~dayOfMonth=daysInMonth,
          `LastWeekdayOfMonth,
        ),
      )
      |> toEqual(false);
    });
    test(
      "A Friday the day before the end of the month should always evaluate to true",
      () => {
      let daysInMonth = Random.int(4) + 28;
      expect(
        isInDayOfMonth(
          ~daysInMonth,
          ~dayOfWeek=5,
          ~dayOfMonth=daysInMonth - 1,
          `LastWeekdayOfMonth,
        ),
      )
      |> toEqual(true);
    });
    test(
      "A Friday two days before the end of the month should always evaluate to true",
      () => {
      let daysInMonth = Random.int(4) + 28;
      expect(
        isInDayOfMonth(
          ~daysInMonth,
          ~dayOfWeek=5,
          ~dayOfMonth=daysInMonth - 2,
          `LastWeekdayOfMonth,
        ),
      )
      |> toEqual(true);
    });
  });
  describe("`Values", () =>
    Belt.Range.forEach(0, 4, testRandomValues)
  );
  describe("`DaysBeforeEndOfMonth", () =>
    Belt.Range.forEach(0, 4, testNthLastDayOfMonth)
  );
  describe("`Interval", () =>
    Belt.Range.forEach(0, 4, testRandomInterval)
  );
});
