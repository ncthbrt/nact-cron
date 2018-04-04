open Nact;

open Nact.Operators;

type state('a) = {
  scheduleState: Scheduling.state((Nact.actorRef('a), 'a)),
  interval: option(Js.Global.intervalId),
};

let setInterval = ({interval} as state, ctx: Nact.persistentCtx(_)) =>
  switch (interval) {
  | Some(_) => state
  | None => {
      ...state,
      interval:
        Some(
          Js.Global.setInterval(
            () => ctx.self <-< `DispatchPendingJobs,
            30 * 1000,
          ),
        ),
    }
  };

let dispatchPendingJobs = ({scheduleState} as state) => {
  let scheduleState =
    Scheduling.update(
      ~time=Scheduling.Time.fromDate(Js.Date.make()),
      scheduleState,
    );
  Scheduling.getPendingJobs(scheduleState)
  |. Belt.List.forEach((Schedule(_, _, (actor, msg))) => actor <-< msg);
  Js.Promise.resolve({...state, scheduleState});
};

let startScheduledJob =
    (
      {scheduleState} as state,
      `StartScheduledJob(expr, msg, actor, requestee),
    ) => {
  let parsedExpression =
    try (Js.Result.Ok(Expression.parse(expr))) {
    | Expression.MalformedCronExpression =>
      Js.Result.Error(`MalformedCronExpression(expr))
    };
  switch (parsedExpression) {
  | Js.Result.Error(err) =>
    requestee <-< err;
    Js.Promise.resolve(state);
  | Js.Result.Ok(expr) =>
    let (Scheduling.Schedule(id, _, _), scheduleState) =
      Scheduling.addJob(scheduleState, expr, (actor, msg));
    requestee <-< `ScheduledJobStarted(id);
    Js.Promise.resolve({...state, scheduleState});
  };
};

let stopScheduledJob = ({scheduleState} as state, id, requestee) => {
  let scheduleState = Scheduling.removeJob(scheduleState, id);
  requestee <-< `ScheduledJobStarted(id);
  Js.Promise.resolve({...state, scheduleState});
};

let getScheduledJobs = ({scheduleState} as state, requestee) => {
  requestee <-< `ScheduledJobs(Scheduling.getJobs(scheduleState));
  Js.Promise.resolve(state);
};

let getScheduledJob = ({scheduleState} as state, id, requestee) => {
  switch (Scheduling.tryFindJob(scheduleState, id)) {
  | Some(job) => requestee <-< `FoundScheduledJob(job)
  | None => requestee <-< `ScheduledJobNotFound(id)
  };
  Js.Promise.resolve(state);
};

let make = (parent, ~key) => {
  let actor =
    Nact.spawnPersistent(
      ~key,
      parent,
      (state, msg, ctx) => {
        let state = setInterval(state, ctx);
        switch (msg) {
        | `DispatchPendingJobs => dispatchPendingJobs(state)
        | `StartScheduledJob(_, _, _, _) as job =>
          startScheduledJob(state, job)
        | `StopScheduledJob(id, requestee) =>
          stopScheduledJob(state, id, requestee)
        | `GetScheduledJobs(requestee) => getScheduledJobs(state, requestee)
        | `GetScheduledJob(id, requestee) =>
          getScheduledJob(state, id, requestee)
        };
      },
      {scheduleState: Scheduling.empty, interval: None},
    );
  actor;
};