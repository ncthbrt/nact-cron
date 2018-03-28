type day = int;

type step = int;

type t = {
  minutes: [ | `Values(array(int)) | `Wildcard | `Interval(int, int, step)],
  hours: [ | `Values(array(int)) | `Wildcard | `Interval(int, int, step)],
  daysOfMonth: [
    | `Values(array(int))
    | `Wildcard
    | `Interval(int, int, step)
    | `DaysBeforeEndOfMonth(int)
    | `NearestWeekday(int)
    | `LastWeekdayOfMonth
  ],
  months: [ | `Values(array(int)) | `Interval(int, int, step) | `Wildcard],
  daysOfWeek: [
    | `Values(array(int))
    | `Wildcard
    | `Interval(int, int, step)
    | `LastDayOfWeekInMonth(int)
    | `NthDayOfWeekInMonth(day, int)
  ],
  years: [
    | `Values(array(int))
    | `Wildcard
    | `Interval(option(int), option(int), step)
  ],
  expression: string,
};

exception MalformedCronExpression;

let parse: string => t;