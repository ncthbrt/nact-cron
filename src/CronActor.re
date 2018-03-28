let isInArray = (value, arr) => Belt.Array.some(arr, v => v == value);

let isInInterval = (value, start, end_, step) =>
  (value - start) mod step == 0 && value <= end_;

let isWeekday = day => day >= 1 && day <= 5;

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

let isInDayOfWeek = (currentDayOfWeek, currentDayOfMonth) =>
  fun
  | `LastDayOfWeekInMonth(_) => true
  | `NthDayOfWeekInMonth(dayOfWeek, n) =>
    currentDayOfWeek == dayOfWeek && currentDayOfMonth / 7 == n
  | #commonCronExpr as expr => isInExpr(currentDayOfWeek, expr);

let isNearestWeekday =
    (currentDayOfMonth, daysInMonth, scheduledDayOfMonth, currentDayOfWeek) => {
  let startDayOfWeekOfMonth = currentDayOfWeek - currentDayOfMonth mod 7 + 1;
  let dayOfWeekOfScheduledDay =
    (startDayOfWeekOfMonth + scheduledDayOfMonth) mod 7;
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

let isInDayOfMonth = (currentDayOfMonth, daysInMonth, currentDayOfWeek) =>
  fun
  | `DaysBeforeEndOfMonth(daysBefore) =>
    daysInMonth - currentDayOfMonth == daysBefore
  | `NearestWeekday(day) =>
    isNearestWeekday(currentDayOfMonth, daysInMonth, day, currentDayOfWeek)
  | `LastWeekdayOfMonth =>
    isLastWeekdayOfMonth(currentDayOfMonth, daysInMonth, currentDayOfWeek)
  | #commonCronExpr as expr => isInExpr(currentDayOfMonth, expr);