let isInYear:
  (
    int,
    [
      | `Values(array(int))
      | `Wildcard
      | `Interval(option(int), option(int), int)
    ]
  ) =>
  bool;

let isInMinute:
  (int, [ | `Values(array(int)) | `Wildcard | `Interval(int, int, int)]) =>
  bool;

let isInHour:
  (int, [ | `Values(array(int)) | `Wildcard | `Interval(int, int, int)]) =>
  bool;

let isInMonth:
  (int, [ | `Values(array(int)) | `Wildcard | `Interval(int, int, int)]) =>
  bool;

let isInDayOfWeek:
  (
    ~currentDayOfMonth: int,
    ~currentDayOfWeek: int,
    ~daysInMonth: int,
    [
      | `Values(array(int))
      | `Wildcard
      | `Interval(int, int, int)
      | `LastDayOfWeekInMonth(int)
      | `NthDayOfWeekInMonth(int, int)
    ]
  ) =>
  bool;

let isInDayOfMonth:
  (
    ~currentDayOfMonth: int,
    ~currentDayOfWeek: int,
    ~daysInMonth: int,
    [
      | `Values(array(int))
      | `Wildcard
      | `Interval(int, int, int)
      | `DaysBeforeEndOfMonth(int)
      | `NearestWeekday(int)
      | `LastWeekdayOfMonth
    ]
  ) =>
  bool;