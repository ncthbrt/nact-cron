open Jest;

open ExpectJs;

open CronExpression;

let testExpression =
    (
      ~minutes=`Wildcard,
      ~hours=`Wildcard,
      ~daysOfMonth=`Wildcard,
      ~months=`Wildcard,
      ~daysOfWeek=`Wildcard,
      ~years=`Wildcard,
      expression,
    ) =>
  test(
    "The cron expression " ++ expression ++ " should be correctly parsed", () =>
    expect(CronExpression.parse(expression))
    |> toEqual({
         minutes,
         hours,
         daysOfMonth,
         months,
         daysOfWeek,
         years,
         expression,
       })
  );

let testMalformedExpression = expression =>
  test("The cron expression " ++ expression ++ " should fail to parse", () =>
    expectFn(() => CronExpression.parse(expression), ())
    |> toThrowException(CronExpression.MalformedCronExpression)
  );

testExpression("* * * * *");

testExpression(~minutes=`Values([|0|]), "0 * * * *");

testExpression(~minutes=`Values(Belt.Array.range(0, 10)), "0-10 * * * *");

testExpression(~daysOfWeek=`Values([|0, 2, 4, 6|]), "* * * * */2");

testExpression(~months=`Values([|9, 11|]), "* * * 9-12/2 *");

testExpression("* * * ? *");

testExpression("* * * * * ?");

testExpression(
  ~daysOfWeek=`Values([|0, 1, 2, 4|]),
  "* * * * mon,tue,thu,sun",
);

/* Day 7 is Sunday but is officially Day 0 */
testExpression(~daysOfWeek=`Values([|0, 2|]), "* * * * 7,2");

testExpression(
  ~minutes=`Values([|0|]),
  ~hours=`Values([|0|]),
  ~daysOfMonth=`Values([|1|]),
  ~months=`Values([|1|]),
  "@annually",
);

testExpression(
  ~minutes=`Values([|0|]),
  ~hours=`Values([|0|]),
  ~daysOfMonth=`Values([|1|]),
  "@monthly",
);

testExpression(
  ~minutes=`Values([|0|]),
  ~hours=`Values([|0|]),
  ~daysOfWeek=`Values([|0|]),
  "@weekly",
);

testExpression(~minutes=`Values([|0|]), ~hours=`Values([|0|]), "@daily");

testExpression(~minutes=`Values([|0|]), ~hours=`Values([|0|]), "@midnight");

testExpression(~minutes=`Values([|0|]), "@hourly");

testExpression(
  ~minutes=`Values([|0|]),
  ~hours=`Values([|18|]),
  ~daysOfWeek=`LastDayOfWeekInMonth(6),
  ~years=`Values([|2015, 2016, 2017|]),
  "0 18 ? * 6L 2015-2017",
);

testExpression(~months=`Values([|1, 12|]), "* * * DEC,JAN *");

testExpression(~months=`Values([|1, 2, 3|]), "* * * JAN-MAR *");

testExpression(~months=`Values([|1, 3, 5|]), "* * * JAN-MAY/2 *");

testExpression(~daysOfWeek=`Values([|1, 2, 3, 4, 5|]), "* * * * MON-FRI");

testExpression(
  ~daysOfWeek=`Values(Belt.Array.rangeBy(~step=2, 2, 6)),
  "* * * * TUE-/2",
);

testExpression(~daysOfMonth=`NearestWeekday(15), "* * 15W * *");

testExpression(~daysOfMonth=`DaysBeforeEndOfMonth(0), "* * L * *");

testExpression(~daysOfMonth=`DaysBeforeEndOfMonth(1), "* * L-2 * *");

/* Out of range days before end of month */
testMalformedExpression("* * L-32 * *");

testMalformedExpression("* * L-0 * *");

testExpression(~daysOfWeek=`LastDayOfWeekInMonth(5), "* * * * 5L");

testExpression(~daysOfWeek=`NthDayOfWeekInMonth((3, 3)), "* * * * WED#3");

testExpression(~daysOfWeek=`NthDayOfWeekInMonth((3, 3)), "* * * * wed#3");

testExpression(~daysOfWeek=`NthDayOfWeekInMonth((3, 3)), "* * * * 3#3");

testExpression(~daysOfMonth=`LastWeekdayOfMonth, "* * LW * *");

testExpression(
  ~months=`Values([|1, 12|]),
  ~years=`Values(Belt.Array.rangeBy(~step=3, 2000, 2020)),
  "* * * DEC,JAN * 2000-2020/3",
);

testExpression(
  ~months=`Values([|1, 12|]),
  ~years=`UnboundedInterval((Some(2000), None, 3)),
  "* * * DEC,JAN * 2000-/3",
);

testExpression(
  ~months=`Values([|1, 12|]),
  ~years=`UnboundedInterval((Some(2000), None, 3)),
  "* * * DEC,JAN * 2000/3",
);

testExpression(
  ~months=`Values([|1, 12|]),
  ~years=`UnboundedInterval((Some(2000), None, 1)),
  "* * * DEC,JAN * 2000-",
);

testExpression(
  ~months=`Values([|1, 12|]),
  ~years=`UnboundedInterval((None, Some(2020), 1)),
  "* * * DEC,JAN * -2020",
);

testExpression(
  ~months=`Values([|1, 12|]),
  ~years=`UnboundedInterval((None, Some(2020), 2)),
  "* * * DEC,JAN * -2020/2",
);

testExpression(
  ~months=`Values([|1, 12|]),
  ~years=`Values([|2018, 2019, 2020|]),
  "* * * DEC,JAN * 2018,2019,2020",
);

testExpression(
  ~months=`Values([|1, 12|]),
  ~years=`UnboundedInterval((None, None, 2)),
  "* * * DEC,JAN * */2",
);

testExpression(
  ~months=`Values([|1, 12|]),
  ~years=`UnboundedInterval((None, None, 2)),
  "* * * DEC,JAN * */2",
);

testExpression(
  ~months=`Values([|1, 12|]),
  ~years=`UnboundedInterval((None, Some(2020), 2)),
  "* * * DEC,JAN * -2020/2",
);

/* Too many fields */
testMalformedExpression("* * * DEC,JAN * * *");

/* Too few fields */
testMalformedExpression("* * * *");

testMalformedExpression("* * *");

testMalformedExpression("* *");

testMalformedExpression("*");

/* Too many steps */
testMalformedExpression("* * * DEC,JAN * 2020-/2/2");

testMalformedExpression("* * * DEC-/2/3 * *");

/* Adding a step to comma separated list, year */
testMalformedExpression("* * * DEC,JAN * 2020,2021/2");

testMalformedExpression("* * * DEC,JAN/2 * *");

testMalformedExpression("* * * 1,2/2 * *");

testMalformedExpression("* * * 10-20/ * *");

/* 59 is max */
testMalformedExpression("60 * * DEC,JAN * *");

/* 23 is max */
testMalformedExpression("* 24 * DEC,JAN * *");

/* 31 is max */
testMalformedExpression("* * 32 DEC,JAN * *");

/* 1 is min */
testMalformedExpression("* * 0 DEC,JAN * *");

/* 12 is max */
testMalformedExpression("* * * 13 *");

/* 1 is min */
testMalformedExpression("* * * 0 *");

/* 7 is max */
testMalformedExpression("* * * * 8");

testMalformedExpression("* * * JANFEB *");

testMalformedExpression("* * * - *");

testMalformedExpression("* * * * * -");