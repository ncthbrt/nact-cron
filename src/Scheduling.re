module ScheduleId = {
  type t = int;
  type key = t;
  type id = t;
};

type schedule('a) =
  | Schedule(ScheduleId.t, Expression.t, 'a);

type time = {
  year: int,
  month: int,
  dayOfMonth: int,
  dayOfWeek: int,
  hour: int,
  minute: int,
};

type state('a) = {
  removalList: Belt.Set.Int.t,
  schedules: Belt.List.t(schedule('a)),
  years: Belt.List.t(schedule('a)),
  months: Belt.List.t(schedule('a)),
  daysOfMonth: Belt.List.t(schedule('a)),
  daysOfWeek: Belt.List.t(schedule('a)),
  minutes: Belt.List.t(schedule('a)),
};

let updateYear =
    ({year: prevYear}, {year}, _, {schedules, years, removalList} as state) =>
  prevYear == year ? state : {...state, years};

let updateMonth = ({month: prevMonth}, {month}, _, state) =>
  prevMonth == month ? state : state;

let updateDaysOfMonth =
    ({dayOfMonth: prevDayOfMonth}, {dayOfMonth}, _, state) =>
  prevDayOfMonth == dayOfMonth ? state : state;

let updateDaysOfWeek = ({dayOfWeek: prevDayOfWeek}, {dayOfWeek}, _, state) =>
  prevDayOfWeek == dayOfWeek ? state : state;

let updateHours = ({hour: prevHour}, {hour}, _, state) =>
  prevHour == hour ? state : state;

let updateMinutes = ({minute: prevMinute}, {minute}, _, state) =>
  prevMinute == minute ? state : state;

let thread = (prevState, prevTime, time, daysInMonth, functions) =>
  Belt.List.reduce(functions, prevState, (state, f) =>
    f(prevTime, time, daysInMonth, state)
  );

let update = (prevState, prevTime, time, daysInMonth) =>
  thread(
    prevState,
    prevTime,
    time,
    daysInMonth,
    [
      updateYear,
      updateMonth,
      updateDaysOfMonth,
      updateDaysOfWeek,
      updateHours,
      updateMinutes,
    ],
  );