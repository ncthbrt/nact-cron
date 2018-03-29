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
  hours: Belt.List.t(schedule('a)),
  minutes: Belt.List.t(schedule('a)),
};

let (>>) = (f, g, x) => g(f(x));

let partitionBy = (lists, criteria) => {
  let partitions = Belt.List.map(lists, Belt.List.partition(_, criteria));
  let toKeep = partitions |. Belt.List.map(fst);
  let toDiscard = partitions |. Belt.List.map(snd);
  (Belt.List.flatten(toKeep), Belt.List.flatten(toDiscard));
};

let updateYear = ({year: prevYear}, {year}, _, state) =>
  if (prevYear == year) {
    state;
  } else {
    let (toKeep, toDiscard) =
      partitionBy(
        [
          state.schedules,
          state.years,
          state.months,
          state.daysOfMonth,
          state.daysOfWeek,
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
      daysOfMonth: [],
      daysOfWeek: [],
      hours: [],
      minutes: [],
    };
  };

let updateMonth = ({month: prevMonth}, {month}, _, state) =>
  if (prevMonth == month) {
    state;
  } else {
    let (toKeep, toDiscard) =
      partitionBy(
        [
          state.years,
          state.months,
          state.daysOfMonth,
          state.daysOfWeek,
          state.hours,
          state.minutes,
        ],
        (Schedule(_, {months}, _)) =>
        Evaluators.isInMonth(month, months)
      );
    {
      ...state,
      years: toDiscard,
      months: toKeep,
      daysOfMonth: [],
      daysOfWeek: [],
      hours: [],
      minutes: [],
    };
  };

let updateDaysOfMonth =
    (
      {dayOfMonth: prevDayOfMonth},
      {dayOfMonth, dayOfWeek},
      daysInMonth,
      state,
    ) =>
  if (prevDayOfMonth == dayOfMonth) {
    state;
  } else {
    let (toKeep, toDiscard) =
      partitionBy(
        [
          state.months,
          state.daysOfMonth,
          state.daysOfWeek,
          state.hours,
          state.minutes,
        ],
        (Schedule(_, {daysOfMonth}, _)) =>
        Evaluators.isInDayOfMonth(
          ~currentDayOfMonth=dayOfMonth,
          ~daysInMonth,
          ~currentDayOfWeek=dayOfWeek,
          daysOfMonth,
        )
      );
    {
      ...state,
      months: toDiscard,
      daysOfMonth: toKeep,
      daysOfWeek: [],
      hours: [],
      minutes: [],
    };
  };

let updateDaysOfWeek =
    (
      {dayOfWeek: prevDayOfWeek},
      {dayOfWeek, dayOfMonth},
      daysInMonth,
      state,
    ) =>
  if (prevDayOfWeek == dayOfWeek) {
    state;
  } else {
    let (toKeep, toDiscard) =
      partitionBy(
        [state.daysOfMonth, state.daysOfWeek, state.hours, state.minutes],
        (Schedule(_, {daysOfWeek}, _)) =>
        Evaluators.isInDayOfWeek(
          ~currentDayOfMonth=dayOfMonth,
          ~daysInMonth,
          ~currentDayOfWeek=dayOfWeek,
          daysOfWeek,
        )
      );
    {
      ...state,
      daysOfMonth: toDiscard,
      daysOfWeek: toKeep,
      hours: [],
      minutes: [],
    };
  };

let updateHours = ({hour: prevHour}, {hour}, _, state) =>
  if (prevHour == hour) {
    state;
  } else {
    let (toKeep, toDiscard) =
      partitionBy(
        [state.daysOfMonth, state.daysOfWeek, state.hours, state.minutes],
        (Schedule(_, {hours}, _)) =>
        Evaluators.isInHour(hour, hours)
      );
    {...state, daysOfWeek: toDiscard, hours: toKeep, minutes: []};
  };

let updateMinutes = ({minute: prevMinute}, {minute}, _, state) =>
  if (prevMinute == minute) {
    state;
  } else {
    let (toKeep, toDiscard) =
      partitionBy(
        [state.daysOfMonth, state.daysOfWeek, state.hours, state.minutes],
        (Schedule(_, {minutes}, _)) =>
        Evaluators.isInMinute(minute, minutes)
      );
    {...state, hours: toDiscard, minutes: toKeep};
  };

let thread = (prevState, prevTime, time, daysInMonth, functions) =>
  Belt.List.reduce(functions, prevState, (state, f) =>
    f(prevTime, time, daysInMonth, state)
  );

let empty: state('a) = {
  removalList: Belt.Set.Int.empty,
  schedules: [],
  years: [],
  months: [],
  daysOfMonth: [],
  daysOfWeek: [],
  hours: [],
  minutes: [],
};

let update = (~prevTime, ~time, ~daysInMonth, prevState) =>
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

gi;