module Expression = NactCron_Expression;

module Time: {
  type t = {
    year: int,
    month: int,
    dayOfMonth: int,
    dayOfWeek: int,
    hour: int,
    minute: int,
    daysInMonth: int,
  };
  let fromDate: Js.Date.t => t;
};

module ScheduleId: {type t; let fromInteger: int => t;};

type schedule('a) =
  | Schedule(ScheduleId.t, Expression.t, 'a);

type state('a);

let empty: state('a);

let update: (state('a), Time.t) => state('a);

let getPendingJobs: state('a) => list(schedule('a));

let getJobs: state('a) => list(schedule('a));

let tryFindJob: (state('a), ScheduleId.t) => option(schedule('a));

let addJob: (state('a), Expression.t, 'a) => (schedule('a), state('a));

let removeJob: (state('a), ScheduleId.t) => state('a);