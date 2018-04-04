let isInArray = (value, arr) => Belt.Array.some(arr, v => v == value);

let isInInterval = (value, start, end_, step) =>
  (value - start) mod step == 0 && value <= end_;

let isInYear = currentYear =>
  fun
  | `Values(arr) => isInArray(currentYear, arr)
  | `Wildcard => true
  | `Interval(Some(start), None, step) =>
    isInInterval(currentYear, start, max_int, step)
  | `Interval(Some(start), Some(end_), step) =>
    isInInterval(currentYear, start, end_, step)
  | `Interval(None, Some(end_), step) =>
    isInInterval(currentYear, 0, end_, step)
  | `Interval(None, None, step) =>
    isInInterval(currentYear, 0, max_int, step);

type commonCronExpr = [
  | `Values(array(int))
  | `Wildcard
  | `Interval(int, int, int)
];

let isInExpr = current =>
  fun
  | `Values(arr) => isInArray(current, arr)
  | `Wildcard => true
  | `Interval(start, end_, step) => isInInterval(current, start, end_, step);

let isInMinute = isInExpr;

let isInHour = isInExpr;

let isInMonth = isInExpr;

let isInDayOfWeek = (~dayOfMonth, ~dayOfWeek, ~daysInMonth) =>
  fun
  | `LastDayOfWeekInMonth(day) =>
    dayOfWeek == day && dayOfMonth + 7 > daysInMonth
  | `NthDayOfWeekInMonth(dayOfWeek, n) =>
    dayOfWeek == dayOfWeek && dayOfMonth / 7 + 1 == n
  | #commonCronExpr as expr => isInExpr(dayOfWeek, expr);

let isNearestWeekday =
    (currentDayOfMonth, daysInMonth, scheduledDayOfMonth, currentDayOfWeek) => {
  let dayOfWeekOfScheduledDay =
    (currentDayOfWeek + (scheduledDayOfMonth - currentDayOfMonth) + 7) mod 7;
  let daysInMonthRemaining = daysInMonth - scheduledDayOfMonth;
  let daysToNearestWeekdayFromScheduledDay =
    switch (dayOfWeekOfScheduledDay) {
    | 6 => (-1)
    | 0 => daysInMonthRemaining >= 1 ? 1 : (-2)
    | _ => 0
    };
  daysToNearestWeekdayFromScheduledDay
  + scheduledDayOfMonth == currentDayOfMonth;
};

let isLastWeekdayOfMonth = (currentDayOfMonth, daysInMonth, currentDayOfWeek) => {
  let daysInMonthRemaining = daysInMonth - currentDayOfMonth;
  let dayOfWeekAtMonthEnd = (currentDayOfWeek + daysInMonthRemaining) mod 7;
  let daysFromMonthEndToNearestWeekday =
    switch (dayOfWeekAtMonthEnd) {
    | 6 => 1
    | 0 => 2
    | _ => 0
    };
  let dayOfMonthOfLastWeekday = daysInMonth - daysFromMonthEndToNearestWeekday;
  currentDayOfMonth == dayOfMonthOfLastWeekday;
};

let isInDayOfMonth = (~dayOfMonth, ~dayOfWeek, ~daysInMonth) =>
  fun
  | `DaysBeforeEndOfMonth(daysBefore) =>
    daysInMonth - dayOfMonth == daysBefore
  | `NearestWeekday(day) =>
    isNearestWeekday(dayOfMonth, daysInMonth, day, dayOfWeek)
  | `LastWeekdayOfMonth =>
    isLastWeekdayOfMonth(dayOfMonth, daysInMonth, dayOfWeek)
  | #commonCronExpr as expr => isInExpr(dayOfMonth, expr);