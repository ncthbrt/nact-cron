/* module CronExpression = {

   };

    type scheduleId = string;

    module Protocol = {
      module Response = {
        type creationResponse('a) = 'a
        constraint [> | `ScheduleCreated(string)] = 'a;
        type cancellationResponse('a) = 'a
        constraint [> | `ScheduleCancelled(string)] = 'a;
      };
      module Request = {
        type t('a) = string;
        let create = (schedule, msg, actor, requestee) => "Hello";
        let cancel = (scheduleId, requestee) => "Goodbye";
      };
    };

    let createScheduleActor = (~parent, ~key) =>
      Nact.spawnPersistent(
        ~key,
        parent,
        (state, msg, ctx) => Js.Promise.resolve(state),
        [],
      ); */
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
  |> Belt.Array.concat(_, [|("7", "0")|]);

type parsedCronExpression = [ | `Values(array(int)) | `Wildcard];

type t = {
  minutes: parsedCronExpression,
  hours: parsedCronExpression,
  daysOfMonth: [ parsedCronExpression | `LastDayOfMonth],
  months: parsedCronExpression,
  daysOfWeek: [ parsedCronExpression | `LastDayOfMonth(int)],
  expression: string,
};

exception MalformedCronExpression;

type fieldToken =
  | WildcardToken
  | RangeToken(int, int)
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
  | [|"*"|] => WildcardToken
  | [|start, end_|]
      when isNumber(substitute(start)) && isNumber(substitute(end_)) =>
    RangeToken(
      int_of_string(substitute(start)),
      int_of_string(substitute(end_)),
    )
  | [|n|] when isNumber(substitute(n)) =>
    NumberToken(int_of_string(substitute(n)))
  | [|n|] => CommaSeparatedListToken(toCommaSeparatedList(n, substitute))
  | _ => raise(MalformedCronExpression)
  };
};

let getIntervalToken = interval =>
  try (int_of_string(interval)) {
  | Failure(_) => raise(MalformedCronExpression)
  };

let inRange = (start, end_, n) => start <= n && n <= end_;

let parseSubExpr = (subExpr, start, end_, substitutions) => {
  open Belt.Array;
  let (fieldToken, intervalToken) =
    switch (subExpr |> Js.String.split("/")) {
    | [|field|] => (parseToken(field, substitutions), None)
    | [|field, interval|] => (
        parseToken(field, substitutions),
        Some(getIntervalToken(interval)),
      )
    | _ => raise(MalformedCronExpression)
    };
  switch (fieldToken, intervalToken) {
  | (WildcardToken, None) => `Wildcard
  | (WildcardToken, Some(interval)) =>
    `Values(rangeBy(start, end_, ~step=interval))
  | (RangeToken(a, b), None) => `Values(range(a, b))
  | (RangeToken(a, b), Some(interval)) =>
    `Values(rangeBy(a, b, ~step=interval))
  | (NumberToken(n), None) when n |> inRange(start, end_) => `Values([|n|])
  | (NumberToken(n), Some(interval)) when n <= end_ =>
    `Values(rangeBy(n, end_, ~step=interval))
  | (NumberToken(_), _) => raise(MalformedCronExpression)
  | (CommaSeparatedListToken(lst), None)
      when every(lst, inRange(start, end_)) =>
    `Values(lst |> Js.Array.sortInPlace)
  | (CommaSeparatedListToken(_), _) => raise(MalformedCronExpression)
  };
};

let spacesRegex = Js.Re.fromString("\\s+");

let parse = str =>
  switch (
    Js.String.trim(str)
    |> substituteMacros(_)
    |> Js.String.splitByRe(spacesRegex, _)
  ) {
  | [|minutes, hours, daysOfMonth, months, daysOfWeek|] => {
      minutes: parseSubExpr(minutes, 0, 59, [||]),
      hours: parseSubExpr(hours, 0, 23, [||]),
      daysOfMonth: parseSubExpr(daysOfMonth, 1, 31, [||]),
      months: parseSubExpr(months, 1, 12, monthNames),
      daysOfWeek: daysOfWeek |> parseSubExpr(_, 0, 6, dayNames),
      expression: str,
    }
  | _ => raise(MalformedCronExpression)
  };