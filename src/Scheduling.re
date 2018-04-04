module Time = {
  type t = {
    year: int,
    month: int,
    dayOfMonth: int,
    dayOfWeek: int,
    hour: int,
    minute: int,
    daysInMonth: int,
  };
  let fromDate = (date: Js.Date.t) => {
    year: int_of_float(Js.Date.getUTCFullYear(date)),
    month: int_of_float(Js.Date.getUTCMonth(date)) + 1,
    dayOfMonth: int_of_float(Js.Date.getUTCDate(date)),
    dayOfWeek: int_of_float(Js.Date.getUTCDay(date)),
    hour: int_of_float(Js.Date.getUTCHours(date)),
    minute: int_of_float(Js.Date.getUTCMinutes(date)),
    daysInMonth:
      int_of_float(
        Js.Date.getUTCDate(
          Js.Date.makeWithYMD(
            ~year=Js.Date.getUTCFullYear(date),
            ~month=Js.Date.getUTCMonth(date) +. 1.0,
            ~date=0.0,
            (),
          ),
        ),
      )
      + 1,
  };
};

open Time;

module ScheduleId = {
  type t = int;
  let random = () => Random.int(2 lsl 30);
};

type schedule('a) =
  | Schedule(ScheduleId.t, Expression.t, 'a);

type state('a) = {
  prevTime: Time.t,
  schedules: Belt.List.t(schedule('a)),
  years: Belt.List.t(schedule('a)),
  months: Belt.List.t(schedule('a)),
  days: Belt.List.t(schedule('a)),
  hours: Belt.List.t(schedule('a)),
  minutes: Belt.List.t(schedule('a)),
};

let partitionBy = (lists, criteria) => {
  let partitions = Belt.List.map(lists, Belt.List.partition(_, criteria));
  let toKeep = partitions |. Belt.List.map(fst);
  let toDiscard = partitions |. Belt.List.map(snd);
  (Belt.List.flatten(toKeep), Belt.List.flatten(toDiscard));
};

let updateYears = ({year: prevYear}, {year}, state) =>
  if (prevYear == year) {
    state;
  } else {
    let (toKeep, toDiscard) =
      partitionBy(
        [
          state.schedules,
          state.years,
          state.months,
          state.days,
          state.hours,
          state.minutes,
        ],
        (Schedule(_, {years}, _)) =>
        Evaluators.isInYear(year, years)
      );
    {
      ...state,
      schedules: toDiscard,
      years: toKeep,
      months: [],
      days: [],
      hours: [],
      minutes: [],
    };
  };

let updateMonths = ({month: prevMonth}, {month}, state) =>
  if (prevMonth == month) {
    state;
  } else {
    let (toKeep, toDiscard) =
      partitionBy(
        [state.years, state.months, state.days, state.hours, state.minutes],
        (Schedule(_, {months}, _)) =>
        Evaluators.isInMonth(month, months)
      );
    {
      ...state,
      years: toDiscard,
      months: toKeep,
      days: [],
      hours: [],
      minutes: [],
    };
  };

let updateDays =
    (
      {dayOfMonth: prevDayOfMonth},
      {dayOfMonth, dayOfWeek, daysInMonth},
      state,
    ) =>
  if (prevDayOfMonth == dayOfMonth) {
    state;
  } else {
    let (toKeep, toDiscard) =
      partitionBy(
        [state.months, state.days, state.hours, state.minutes],
        (Schedule(_, {daysOfMonth, daysOfWeek}, _)) =>
        Evaluators.isInDayOfMonth(
          ~dayOfMonth,
          ~dayOfWeek,
          ~daysInMonth,
          daysOfMonth,
        )
        && Evaluators.isInDayOfWeek(
             ~dayOfMonth,
             ~dayOfWeek,
             ~daysInMonth,
             daysOfWeek,
           )
      );
    {...state, months: toDiscard, days: toKeep, hours: [], minutes: []};
  };

let updateHours = ({hour: prevHour}, {hour}, state) =>
  if (prevHour == hour) {
    state;
  } else {
    let (toKeep, toDiscard) =
      partitionBy(
        [state.days, state.hours, state.minutes], (Schedule(_, {hours}, _)) =>
        Evaluators.isInHour(hour, hours)
      );
    {...state, days: toDiscard, hours: toKeep, minutes: []};
  };

let updateMinutes = ({minute: prevMinute}, {minute}, state) =>
  if (prevMinute == minute) {
    state;
  } else {
    let (toKeep, toDiscard) =
      partitionBy(
        [state.hours, state.minutes], (Schedule(_, {minutes}, _)) =>
        Evaluators.isInMinute(minute, minutes)
      );
    {...state, hours: toDiscard, minutes: toKeep};
  };

let thread = (prevState, prevTime, time, functions) =>
  Belt.List.reduce(functions, prevState, (state, f) =>
    f(prevTime, time, state)
  );

let empty: state('a) = {
  prevTime: {
    year: 0,
    month: 0,
    dayOfMonth: 0,
    dayOfWeek: 0,
    hour: 0,
    minute: 0,
    daysInMonth: 0,
  },
  schedules: [],
  years: [],
  months: [],
  days: [],
  hours: [],
  minutes: [],
};

let update = (~time, prevState) => {
  ...
    thread(
      prevState,
      prevState.prevTime,
      time,
      [updateYears, updateMonths, updateDays, updateHours, updateMinutes],
    ),
  prevTime: time,
};

let getPendingJobs = ({minutes}) => minutes;

let addJob = (state, expr, msg) => {
  open Evaluators;
  let job = Schedule(ScheduleId.random(), expr, msg);
  let time = state.prevTime;
  let updatedState =
    if (! isInYear(time.year, expr.years)) {
      {...state, schedules: [job, ...state.schedules]};
    } else if (! isInMonth(time.month, expr.months)) {
      {...state, years: [job, ...state.years]};
    } else if (!
                 isInDayOfMonth(
                   ~dayOfMonth=time.dayOfMonth,
                   ~dayOfWeek=time.dayOfWeek,
                   ~daysInMonth=time.daysInMonth,
                   expr.daysOfMonth,
                 )) {
      {...state, months: [job, ...state.months]};
    } else if (!
                 isInDayOfWeek(
                   ~dayOfMonth=time.dayOfMonth,
                   ~dayOfWeek=time.dayOfWeek,
                   ~daysInMonth=time.daysInMonth,
                   expr.daysOfWeek,
                 )) {
      {...state, months: [job, ...state.months]};
    } else if (! isInHour(time.hour, expr.hours)) {
      {...state, days: [job, ...state.days]};
    } else if (! isInMinute(time.hour, expr.hours)) {
      {...state, hours: [job, ...state.hours]};
    } else {
      {...state, minutes: [job, ...state.minutes]};
    };
  (job, updatedState);
};

let (>>) = (f, g, x) => g(f(x));

let scheduleDoesNotHaveId = (sId, Schedule(id, _, _)) => sId != id;

let getJobs = state =>
  Belt.List.concatMany([|
    state.schedules,
    state.years,
    state.months,
    state.days,
    state.hours,
    state.minutes,
  |]);

let tryFindJob = (state, targetId) =>
  [
    state.schedules |. Belt.List.getBy((Schedule(id, _, _)) => id == targetId),
    state.years |. Belt.List.getBy((Schedule(id, _, _)) => id == targetId),
    state.months |. Belt.List.getBy((Schedule(id, _, _)) => id == targetId),
    state.days |. Belt.List.getBy((Schedule(id, _, _)) => id == targetId),
    state.hours |. Belt.List.getBy((Schedule(id, _, _)) => id == targetId),
    state.minutes |. Belt.List.getBy((Schedule(id, _, _)) => id == targetId),
  ]
  |. Belt.List.getBy(
       fun
       | Some(_) => true
       | None => false,
     )
  |. Belt.Option.getWithDefault(None);

let removeJob = (state, id: ScheduleId.t) => {
  ...state,
  schedules: Belt.List.keep(state.schedules, scheduleDoesNotHaveId(id)),
  years: Belt.List.keep(state.years, scheduleDoesNotHaveId(id)),
  months: Belt.List.keep(state.months, scheduleDoesNotHaveId(id)),
  days: Belt.List.keep(state.days, scheduleDoesNotHaveId(id)),
  hours: Belt.List.keep(state.hours, scheduleDoesNotHaveId(id)),
  minutes: Belt.List.keep(state.minutes, scheduleDoesNotHaveId(id)),
};