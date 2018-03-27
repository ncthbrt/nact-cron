let substitutions = [|
  ("@annually", "0 0 1 1 *"),
  ("@monthly", "0 0 1 * *"),
  ("@weekly", "0 0 * * 0"),
  ("@daily", "0 0 * * *"),
  ("@midnight", "0 0 * * *"),
  ("@hourly", "0 * * * *"),
|];

let substitute = (substitutions, str) =>
  Belt.Array.reduce(
    substitutions, Js.String.toUpperCase(str), (prevStr, (macro, repl)) =>
    if (macro == str) {
      repl;
    } else {
      prevStr;
    }
  );

let substituteMacros = substitute(substitutions);

let monthNames =
  Belt.Array.zip(
    [|
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
      "DEC",
    |],
    Belt.Array.range(1, 12) |> Belt.Array.map(_, string_of_int),
  );

let dayNames =
  Belt.Array.zip(
    [|"SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT"|],
    Belt.Array.range(0, 6) |> Belt.Array.map(_, string_of_int),
  )
  |> Belt.Array.concat(_, [|("7", "0"), ("L", "7")|]);

type parsedCronExpression = [ | `Values(array(int)) | `Wildcard];

type day = int;

type startYear = int;

type endYear = int;

type step = int;

type t = {
  minutes: parsedCronExpression,
  hours: parsedCronExpression,
  daysOfMonth: [
    parsedCronExpression
    | `DaysBeforeEndOfMonth(int)
    | `NearestWeekday(int)
    | `LastWeekdayOfMonth
  ],
  months: parsedCronExpression,
  daysOfWeek: [
    parsedCronExpression
    | `LastDayOfWeekInMonth(int)
    | `NthDayOfWeekInMonth(day, int)
  ],
  years: [
    parsedCronExpression
    | `UnboundedInterval(option(startYear), option(endYear), step)
  ],
  expression: string,
};

exception MalformedCronExpression;

type fieldToken =
  | WildcardToken
  | RangeToken(option(int), option(int))
  | CommaSeparatedListToken(array(int))
  | NumberToken(int);

let trueF = (_) => true;

let isNumber = str =>
  try (int_of_string(str) |> trueF) {
  | Failure(_) => false
  };

let toCommaSeparatedList = (str, substitute) =>
  Js.String.split(",", str)
  |> Belt.Array.map(_, i =>
       try (int_of_string(substitute(i))) {
       | Failure(_) => raise(MalformedCronExpression)
       }
     );

let parseToken = (field, substitutions) => {
  let substitute = substitute(substitutions);
  switch (Js.String.split("-", field)) {
  | [|"*"|]
  | [|"?"|] => WildcardToken
  | [|start, ""|] when isNumber(substitute(start)) =>
    RangeToken(Some(int_of_string(substitute(start))), None)
  | [|"", end_|] when isNumber(substitute(end_)) =>
    RangeToken(None, Some(int_of_string(substitute(end_))))
  | [|start, end_|]
      when isNumber(substitute(start)) && isNumber(substitute(end_)) =>
    RangeToken(
      Some(int_of_string(substitute(start))),
      Some(int_of_string(substitute(end_))),
    )
  | [|n|] when isNumber(substitute(n)) =>
    NumberToken(int_of_string(substitute(n)))
  | [|n|] => CommaSeparatedListToken(toCommaSeparatedList(n, substitute))
  | _ => raise(MalformedCronExpression)
  };
};

let getIntervalToken = step =>
  try (int_of_string(step)) {
  | Failure(_) => raise(MalformedCronExpression)
  };

let inRange = (start, end_, n) => start <= n && n <= end_;

let parseSubExpr = (subExpr, start, end_, substitutions) => {
  open Belt.Array;
  let (fieldToken, stepToken) =
    switch (subExpr |> Js.String.split("/")) {
    | [|field|] => (parseToken(field, substitutions), None)
    | [|field, step|] => (
        parseToken(field, substitutions),
        Some(getIntervalToken(step)),
      )
    | _ => raise(MalformedCronExpression)
    };
  switch (fieldToken, stepToken) {
  | (WildcardToken, None) => `Wildcard
  | (WildcardToken, Some(step)) => `Values(rangeBy(start, end_, ~step))
  | (RangeToken(a, b), None) =>
    `Values(
      range(
        Belt.Option.getWithDefault(a, start),
        Belt.Option.getWithDefault(b, end_),
      ),
    )
  | (RangeToken(a, b), Some(step)) =>
    `Values(
      rangeBy(
        Belt.Option.getWithDefault(a, start),
        Belt.Option.getWithDefault(b, end_),
        ~step,
      ),
    )
  | (NumberToken(n), None) when n |> inRange(start, end_) => `Values([|n|])
  | (NumberToken(n), Some(step)) when n <= end_ =>
    `Values(rangeBy(n, end_, ~step))
  | (NumberToken(_), _) => raise(MalformedCronExpression)
  | (CommaSeparatedListToken(lst), None)
      when every(lst, inRange(start, end_)) =>
    `Values(lst |> Js.Array.sortInPlace)
  | (CommaSeparatedListToken(_), _) => raise(MalformedCronExpression)
  };
};

let nearestWeekdayRegex = Js.Re.fromString("^\\d+W$");

let lastDayOfWeekRegex = Js.Re.fromString("^[0-7]L$");

let daysBeforeEndOfMonthRegex = Js.Re.fromString("^L\\-\\d+$");

let nthDayOfWeekInMonthRegex =
  Js.Re.fromString("^([0-7]|[A-Za-z]{3})#[1-5]$");

let parseDaysOfMonthSubExpr =
  fun
  | "L" => `DaysBeforeEndOfMonth(0)
  | "LW" => `LastWeekdayOfMonth
  | subExpr when Js.Re.test(subExpr, daysBeforeEndOfMonthRegex) => {
      let n = int_of_string(Js.String.replace("L-", "", subExpr));
      if (inRange(1, 31, n)) {
        `DaysBeforeEndOfMonth(n - 1);
      } else {
        raise(MalformedCronExpression);
      };
    }
  | subExpr when Js.Re.test(subExpr, nearestWeekdayRegex) => {
      let n = int_of_string(Js.String.replace("W", "", subExpr));
      if (inRange(1, 32, n)) {
        `NearestWeekday(n);
      } else {
        raise(MalformedCronExpression);
      };
    }
  | subExpr => parseSubExpr(subExpr, 1, 31, [||]);

let parseDaysOfWeekSubExpr =
  fun
  | subExpr when Js.Re.test(subExpr, lastDayOfWeekRegex) => {
      let n = subExpr |> Js.String.replace("L", "", _) |> int_of_string(_);
      `LastDayOfWeekInMonth(n == 7 ? 0 : n);
    }
  | subExpr when Js.Re.test(subExpr, nthDayOfWeekInMonthRegex) => {
      let arr = Js.String.split("#", subExpr);
      let (day, nth) =
        try (
          int_of_string(substitute(dayNames, arr[0])),
          int_of_string(arr[1]),
        ) {
        | Failure(_) => raise(MalformedCronExpression)
        };
      `NthDayOfWeekInMonth((day, nth));
    }
  | subExpr => parseSubExpr(subExpr, 0, 6, dayNames);

let parseYearsSubExpr = subExpr => {
  open Belt.Array;
  let (fieldToken, stepToken) =
    switch (subExpr |> Js.String.split("/")) {
    | [|field|] => (parseToken(field, [||]), None)
    | [|field, step|] => (
        parseToken(field, [||]),
        Some(getIntervalToken(step)),
      )
    | _ => raise(MalformedCronExpression)
    };
  switch (fieldToken, stepToken) {
  | (WildcardToken, None) => `Wildcard
  | (WildcardToken, Some(step)) => `UnboundedInterval((None, None, step))
  | (RangeToken(Some(a), Some(b)), None) => `Values(range(a, b))
  | (RangeToken(Some(a), Some(b)), Some(step)) =>
    `Values(rangeBy(a, b, ~step))
  | (RangeToken(a, b), Some(step)) => `UnboundedInterval((a, b, step))
  | (RangeToken(a, b), None) => `UnboundedInterval((a, b, 1))
  | (NumberToken(n), None) => `Values([|n|])
  | (NumberToken(n), Some(step)) =>
    `UnboundedInterval((Some(n), None, step))
  | (CommaSeparatedListToken(lst), None) =>
    `Values(lst |> Js.Array.sortInPlace)
  | (CommaSeparatedListToken(_), Some(_)) => raise(MalformedCronExpression)
  };
};

let spacesRegex = Js.Re.fromString("\\s+");

let parse = str =>
  switch (
    Js.String.trim(str)
    |> substituteMacros(_)
    |> Js.String.splitByRe(spacesRegex, _)
    |> Belt.List.fromArray
  ) {
  | [minutes, hours, daysOfMonth, months, daysOfWeek, ...rest]
      when Belt.List.length(rest) <= 1 => {
      minutes: parseSubExpr(minutes, 0, 59, [||]),
      hours: parseSubExpr(hours, 0, 23, [||]),
      daysOfMonth: parseDaysOfMonthSubExpr(daysOfMonth),
      months: parseSubExpr(months, 1, 12, monthNames),
      daysOfWeek: parseDaysOfWeekSubExpr(daysOfWeek),
      years:
        switch (Belt.List.head(rest)) {
        | Some(years) => parseYearsSubExpr(years)
        | None => `Wildcard
        },
      expression: str,
    }
  | _ => raise(MalformedCronExpression)
  };