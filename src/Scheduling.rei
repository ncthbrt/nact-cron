module Time: {type t; let fromDate: Js.Date.t => t;};

module ScheduleId: {type t;};

type schedule('a) =
  | Schedule(ScheduleId.t, Expression.t, 'a);

type state('a);

let empty: state('a);

let update: (~time: Time.t, state('a)) => state('a);

let getPendingJobs: state('a) => list(schedule('a));

let getJobs: state('a) => list(schedule('a));

let tryFindJob: (state('a), ScheduleId.t) => option(schedule('a));

let addJob: (state('a), Expression.t, 'a) => (schedule('a), state('a));

let removeJob: (state('a), ScheduleId.t) => state('a);