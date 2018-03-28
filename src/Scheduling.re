module ScheduleId = {
  type t = int;
  type key = t;
  type id = t;
};

type schedule('a) =
  | Schedule(ScheduleId.t, Expression.t, 'a, Nact.actorRef('a));

type time = {
  year: int,
  month: int,
  dayOfMonth: int,
  dayOfWeek: int,
  hour: int,
  minute: int,
};

type state('a) = {
  schedules: Belt.Map.Int.t(schedule('a)),
  years: Belt.List.t(schedule('a)),
  months: Belt.List.t(schedule('a)),
  days: Belt.List.t(schedule('a)),
  minutes: Belt.List.t(schedule('a)),
  prevTime: time,
};

let update = (prevState, time, daysInMonth) => {};