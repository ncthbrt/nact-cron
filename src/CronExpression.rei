type day = int;

type startYear = int;

type endYear = int;

type step = int;

type t = {
  minutes: [ | `Values(array(int)) | `Wildcard],
  hours: [ | `Values(array(int)) | `Wildcard],
  daysOfMonth: [
    | `Values(array(int))
    | `Wildcard
    | `LastDayOfMonth
    | `NearestWeekday(int)
    | `LastWeekdayOfMonth
  ],
  months: [ | `Values(array(int)) | `Wildcard],
  daysOfWeek: [
    | `Values(array(int))
    | `Wildcard
    | `LastDayOfMonth(int)
    | `NthDayOfMonth(day, int)
  ],
  years: [
    | `Values(array(int))
    | `Wildcard
    | `OpenInterval(option(startYear), option(endYear), step)
  ],
  expression: string,
};

exception MalformedCronExpression;

let parse: string => t;