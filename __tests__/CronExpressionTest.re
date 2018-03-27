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
      expression,
    ) =>
  test(
    "The cron expression " ++ expression ++ " should be correctly parsed", () =>
    expect(CronExpression.parse(expression))
    |> toEqual({minutes, hours, daysOfMonth, months, daysOfWeek, expression})
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
  "@monthly",
);

testExpression(~months=`Values([|1, 12|]), "* * * DEC,JAN *");

testExpression(~months=`Values([|1, 2, 3|]), "* * * JAN-MAR *");

testExpression(~daysOfWeek=`Values([|1, 2, 3, 4, 5|]), "* * * * MON-FRI");

/* Extra field */
testMalformedExpression("* * * DEC,JAN * *");

/* 59 is max */
testMalformedExpression("60 * * DEC,JAN * *");

/* 23 is max */
testMalformedExpression("* 24 * DEC,JAN * *");

/* 31 is max */
testMalformedExpression("* * 32 DEC,JAN * *");

/* 12 is max */
testMalformedExpression("* * * 13 *");

/* 7 is max */
testMalformedExpression("* * * * 8");

testMalformedExpression("* * * JANFEB *");