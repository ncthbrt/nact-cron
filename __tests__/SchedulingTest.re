open Jest;

open Expect;

open NactCron;

module Expression = NactCron_Expression;

describe("update", () => {
  test(
    "The schedule state updated twice with the same time should still return the same schedule",
    () => {
      let time: Scheduling.Time.t = {
        year: 2022,
        month: 1,
        dayOfMonth: 2,
        dayOfWeek: 2,
        hour: 0,
        minute: 0,
        daysInMonth: 31,
      };
      expect(
        Scheduling.empty
        |. Scheduling.addJob(Expression.parse("0 0 2 * *"), "job")
        |. snd
        |. Scheduling.update(time)
        |. Scheduling.update(time)
        |. Scheduling.getPendingJobs
        |. Belt.List.toArray,
      )
      |> toHaveLength(1);
    },
  );
  test("Should update schedules if the year changes", () => {
    let time1: Scheduling.Time.t = {
      year: 2022,
      month: 1,
      dayOfMonth: 2,
      dayOfWeek: 2,
      hour: 0,
      minute: 0,
      daysInMonth: 31,
    };
    let time2 = {...time1, year: 2023};
    let (job, state) =
      Scheduling.empty
      |. Scheduling.addJob(Expression.parse("* * * * * 2022"), "job")
      |. snd
      |. Scheduling.addJob(Expression.parse("* * * * * 2023"), "job");
    expect(
      state
      |. Scheduling.update(time1)
      |. Scheduling.update(time2)
      |. Scheduling.getPendingJobs,
    )
    |> toEqual([job]);
  });
  test("Should update schedules if the month changes", () => {
    let time1: Scheduling.Time.t = {
      year: 2022,
      month: 1,
      dayOfMonth: 2,
      dayOfWeek: 2,
      hour: 0,
      minute: 0,
      daysInMonth: 31,
    };
    let time2 = {...time1, month: 2};
    let (job, state) =
      Scheduling.empty
      |. Scheduling.addJob(Expression.parse("* * * JAN *"), "job")
      |. snd
      |. Scheduling.addJob(Expression.parse("* * * FEB *"), "job");
    expect(
      state
      |. Scheduling.update(time1)
      |. Scheduling.update(time2)
      |. Scheduling.getPendingJobs,
    )
    |> toEqual([job]);
  });
  test("Should update schedules if the day of month changes", () => {
    let time1: Scheduling.Time.t = {
      year: 2022,
      month: 1,
      dayOfMonth: 2,
      dayOfWeek: 2,
      hour: 0,
      minute: 0,
      daysInMonth: 31,
    };
    let time2 = {...time1, dayOfMonth: 3, dayOfWeek: 3};
    let (job, state) =
      Scheduling.empty
      |. Scheduling.addJob(Expression.parse("* * 2 * *"), "job")
      |. snd
      |. Scheduling.addJob(Expression.parse("* * 3 * *"), "job");
    expect(
      state
      |. Scheduling.update(time1)
      |. Scheduling.update(time2)
      |. Scheduling.getPendingJobs,
    )
    |> toEqual([job]);
  });
  test("Should update schedules if the hour changes", () => {
    let time1: Scheduling.Time.t = {
      year: 2022,
      month: 1,
      dayOfMonth: 2,
      dayOfWeek: 2,
      hour: 2,
      minute: 0,
      daysInMonth: 31,
    };
    let time2 = {...time1, hour: 3};
    let (job, state) =
      Scheduling.empty
      |. Scheduling.addJob(Expression.parse("* 2 * * *"), "job")
      |. snd
      |. Scheduling.addJob(Expression.parse("* 3 * * *"), "job");
    expect(
      state
      |. Scheduling.update(time1)
      |. Scheduling.update(time2)
      |. Scheduling.getPendingJobs,
    )
    |> toEqual([job]);
  });
  test("Should update schedules if the minute changes", () => {
    let time1: Scheduling.Time.t = {
      year: 2022,
      month: 1,
      dayOfMonth: 2,
      dayOfWeek: 2,
      hour: 2,
      minute: 0,
      daysInMonth: 31,
    };
    let time2 = {...time1, minute: 1};
    let (job, state) =
      Scheduling.empty
      |. Scheduling.addJob(Expression.parse("1 * * * *"), "job");
    expect(
      state
      |. Scheduling.update(time1)
      |. Scheduling.update(time2)
      |. Scheduling.getPendingJobs,
    )
    |> toEqual([job]);
  });
});

describe("getJobs", () =>
  test("All schedules should be returned (including non matching ones)", () =>
    expect(
      Scheduling.empty
      |. Scheduling.update({
           year: 2022,
           month: 1,
           dayOfMonth: 2,
           dayOfWeek: 2,
           hour: 0,
           minute: 0,
           daysInMonth: 31,
         })
      |. Scheduling.addJob(Expression.parse("0 0 1 * *"), "job")
      |. snd
      |. Scheduling.addJob(Expression.parse("0 0 * * TUE"), "job2")
      |. snd
      |. Scheduling.getJobs
      |. Belt.List.toArray,
    )
    |> toHaveLength(2)
  )
);

describe("tryFindJob", () => {
  let time: Scheduling.Time.t = {
    year: 2022,
    month: 1,
    dayOfMonth: 2,
    dayOfWeek: 2,
    hour: 2,
    minute: 0,
    daysInMonth: 31,
  };
  let initialState = Scheduling.empty |. Scheduling.update(time);
  test("Trying to find an existing schedule should return Some", () => {
    let (Scheduling.Schedule(id, _, _) as schedule, state) =
      initialState
      |. Scheduling.addJob(Expression.parse("* * * * * 2023"), "job");
    expect(Scheduling.tryFindJob(state, id)) |> toEqual(Some(schedule));
  });
  test("Trying to find an existing schedule should return Some", () => {
    let (Scheduling.Schedule(id, _, _) as schedule, state) =
      initialState |. Scheduling.addJob(Expression.parse("* * * 2 *"), "job");
    expect(Scheduling.tryFindJob(state, id)) |> toEqual(Some(schedule));
  });
  test("Trying to find an existing schedule should return Some", () => {
    let (Scheduling.Schedule(id, _, _) as schedule, state) =
      initialState |. Scheduling.addJob(Expression.parse("* * * * 1"), "job");
    expect(Scheduling.tryFindJob(state, id)) |> toEqual(Some(schedule));
  });
  test("Trying to find an existing schedule should return Some", () => {
    let (Scheduling.Schedule(id, _, _) as schedule, state) =
      initialState |. Scheduling.addJob(Expression.parse("* * 1 * *"), "job");
    expect(Scheduling.tryFindJob(state, id)) |> toEqual(Some(schedule));
  });
  test("Trying to find an existing schedule should return Some", () => {
    let (Scheduling.Schedule(id, _, _) as schedule, state) =
      initialState |. Scheduling.addJob(Expression.parse("* 3 * * *"), "job");
    expect(Scheduling.tryFindJob(state, id)) |> toEqual(Some(schedule));
  });
  test("Trying to find an existing schedule should return Some", () => {
    let (Scheduling.Schedule(id, _, _) as schedule, state) =
      initialState |. Scheduling.addJob(Expression.parse("1 * * * *"), "job");
    expect(Scheduling.tryFindJob(state, id)) |> toEqual(Some(schedule));
  });
  test("Trying to find an existing schedule should return Some", () => {
    let (Scheduling.Schedule(id, _, _) as schedule, state) =
      initialState |. Scheduling.addJob(Expression.parse("* * * * *"), "job");
    expect(Scheduling.tryFindJob(state, id)) |> toEqual(Some(schedule));
  });
  test("Trying to find a non existant schedule should return None", () => {
    let state =
      initialState
      |. Scheduling.addJob(Expression.parse("1 2 2 1 * 2022"), "job")
      |. snd;
    expect(
      Scheduling.tryFindJob(
        state,
        Scheduling.ScheduleId.fromInteger(Random.int(1000)),
      ),
    )
    |> toEqual(None);
  });
});

describe("removeJob", () => {
  test(
    "Trying to remove a job which never even existed should return successfully with the previous state",
    () => {
      let state =
        Scheduling.empty
        |. Scheduling.update({
             year: 2022,
             month: 1,
             dayOfMonth: 2,
             dayOfWeek: 2,
             hour: 0,
             minute: 0,
             daysInMonth: 31,
           })
        |. Scheduling.addJob(Expression.parse("0 0 1 * *"), "job")
        |. snd;
      expect(
        Scheduling.removeJob(
          state,
          Scheduling.ScheduleId.fromInteger(Random.int(1000)),
        ),
      )
      |> toEqual(state);
    },
  );
  let time: Scheduling.Time.t = {
    year: 2022,
    month: 2,
    dayOfMonth: 2,
    dayOfWeek: 2,
    hour: 2,
    minute: 0,
    daysInMonth: 31,
  };
  let initialState = Scheduling.empty |. Scheduling.update(time);
  test("Removing a job should successfully remove the job", () => {
    let (Scheduling.Schedule(id, _, _), state) =
      Scheduling.empty
      |. Scheduling.update(time)
      |. Scheduling.addJob(Expression.parse("* * * * * 2023"), "job");
    expect(Scheduling.removeJob(state, id)) |> toEqual(initialState);
  });
  test("Removing a job should successfully remove the job", () => {
    let (Scheduling.Schedule(id, _, _), state) =
      Scheduling.empty
      |. Scheduling.update(time)
      |. Scheduling.addJob(Expression.parse("* * * 1 *"), "job");
    expect(Scheduling.removeJob(state, id)) |> toEqual(initialState);
  });
  test("Removing a job should successfully remove the job", () => {
    let (Scheduling.Schedule(id, _, _), state) =
      Scheduling.empty
      |. Scheduling.update(time)
      |. Scheduling.addJob(Expression.parse("* * * * 1"), "job");
    expect(Scheduling.removeJob(state, id)) |> toEqual(initialState);
  });
  test("Removing a job should successfully remove the job", () => {
    let (Scheduling.Schedule(id, _, _), state) =
      Scheduling.empty
      |. Scheduling.update(time)
      |. Scheduling.addJob(Expression.parse("* * 1 * *"), "job");
    expect(Scheduling.removeJob(state, id)) |> toEqual(initialState);
  });
  test("Removing a job should successfully remove the job", () => {
    let (Scheduling.Schedule(id, _, _), state) =
      Scheduling.empty
      |. Scheduling.update(time)
      |. Scheduling.addJob(Expression.parse("* 1 * * *"), "job");
    expect(Scheduling.removeJob(state, id)) |> toEqual(initialState);
  });
  test("Removing a job should successfully remove the job", () => {
    let (Scheduling.Schedule(id, _, _), state) =
      Scheduling.empty
      |. Scheduling.update(time)
      |. Scheduling.addJob(Expression.parse("1 * * * *"), "job");
    expect(Scheduling.removeJob(state, id)) |> toEqual(initialState);
  });
  test("Removing a job should successfully remove the job", () => {
    let (Scheduling.Schedule(id, _, _), state) =
      Scheduling.empty
      |. Scheduling.update(time)
      |. Scheduling.addJob(Expression.parse("* * * * *"), "job");
    expect(Scheduling.removeJob(state, id)) |> toEqual(initialState);
  });
});

describe("getPendingJobs", () => {
  test("An all wildcard schedule should always be returned", () =>
    expect(
      Scheduling.empty
      |. Scheduling.update(Scheduling.Time.fromDate(Js.Date.make()))
      |. Scheduling.addJob(Expression.parse("* * * * *"), "job")
      |. snd
      |. Scheduling.getPendingJobs
      |. Belt.List.toArray,
    )
    |> toHaveLength(1)
  );
  test("A non matching schedule should not be returned", () =>
    expect(
      Scheduling.empty
      |. Scheduling.update({
           year: 2022,
           month: 1,
           dayOfMonth: 2,
           dayOfWeek: 2,
           hour: 0,
           minute: 0,
           daysInMonth: 31,
         })
      |. Scheduling.addJob(Expression.parse("0 0 1 * *"), "job")
      |. snd
      |. Scheduling.addJob(Expression.parse("0 0 * * TUE"), "job2")
      |. snd
      |. Scheduling.getPendingJobs
      |. Belt.List.toArray,
    )
    |> toHaveLength(1)
  );
});

describe("addJob", () => {
  let time: Scheduling.Time.t = {
    year: 2022,
    month: 1,
    dayOfMonth: 2,
    dayOfWeek: 2,
    hour: 2,
    minute: 1,
    daysInMonth: 31,
  };
  let initialState = Scheduling.empty |. Scheduling.update(time);
  test("A new job should be sucessfully added ", () => {
    let (job, state) =
      initialState
      |. Scheduling.addJob(Expression.parse("10 * * * *"), "job");
    expect(Scheduling.getJobs(state)) |> toEqual([job]);
  });
});