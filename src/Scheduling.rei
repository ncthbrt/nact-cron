type time = {
  year: int,
  month: int,
  dayOfMonth: int,
  dayOfWeek: int,
  hour: int,
  minute: int,
};

type state('a);

let empty: state('a);

let update:
  (~prevTime: time, ~time: time, ~daysInMonth: int, state('a)) => state('a);