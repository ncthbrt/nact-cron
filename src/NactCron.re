module Expression = NactCron_Expression;

module Evaluators = NactCron_Evaluators;

module Scheduling = NactCron_Scheduling;

open Nact;

open Nact.Operators;

let return = Js.Promise.resolve;

let then_ = Js.Promise.then_;

type state('a) = {
  scheduleState: Scheduling.state((Nact.actorRef('a), 'a)),
  interval: option(Js.Global.intervalId),
  lastMinute: int,
};

let currentMinute = () => int_of_float(Js.Date.now()) / 1000 / 60;

let dateFromMinute = minute =>
  Js.Date.fromFloat(float_of_int(minute * 1000 * 60));

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

let promiseReduce = (arr, initialValue, f) =>
  Belt.Array.reduce(arr, return(initialValue), (prev, value) =>
    then_(prev => f(prev, value), prev)
  );

let dispatchPendingJobs = ({scheduleState, lastMinute} as state, {persist}) => {
  let currentMinute = currentMinute();
  let lastMinute = lastMinute < 0 ? currentMinute - 1 : lastMinute;
  if (currentMinute > lastMinute) {
    let scheduleState =
      Belt.Array.range(lastMinute, currentMinute)
      |. promiseReduce(
           scheduleState,
           (state, minute) => {
             let state =
               Scheduling.update(
                 state,
                 Scheduling.Time.fromDate(dateFromMinute(minute)),
               );
             let jobs = Scheduling.getPendingJobs(scheduleState);
             Belt.List.forEach(jobs, (Schedule(_, _, (actor, msg))) =>
               actor <-< msg
             );
             persist(`ProcessedMinute(minute))
             |> Js.Promise.then_(() => return(state));
           },
         );
    then_(scheduleState => return({...state, scheduleState}), scheduleState);
  } else {
    return(state);
  };
};

let startScheduledJob =
    (
      {scheduleState} as state,
      `StartScheduledJob(expr, msg, actor, requestee),
      {persist, recovering},
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
  | Js.Result.Ok(parsedExpr) =>
    (
      recovering ?
        return() : persist(`StartScheduledJob((expr, msg, actor, nobody())))
    )
    |> Js.Promise.then_(() => {
         let (Scheduling.Schedule(id, _, _), scheduleState) =
           Scheduling.addJob(scheduleState, parsedExpr, (actor, msg));
         requestee <-< `ScheduledJobStarted(id);
         Js.Promise.resolve({...state, scheduleState});
       })
  };
};

let stopScheduledJob =
    ({scheduleState} as state, id, requestee, {recovering, persist}) => {
  let scheduleState = Scheduling.removeJob(scheduleState, id);
  requestee <-< `ScheduledJobStopped(id);
  (recovering ? return() : persist(`StopScheduledJob((id, nobody()))))
  |> Js.Promise.then_(() => Js.Promise.resolve({...state, scheduleState}));
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

let make = (parent, ~key) =>
  Nact.spawnPersistent(
    ~key,
    ~name=key,
    ~snapshotEvery=20 * messages,
    parent,
    (state, msg, ctx) => {
      let state = setInterval(state, ctx);
      switch (msg) {
      | `ProcessedMinute(minute) => return({...state, lastMinute: minute})
      | `DispatchPendingJobs => dispatchPendingJobs(state, ctx)
      | `StartScheduledJob(_, _, _, _) as job =>
        startScheduledJob(state, job, ctx)
      | `StopScheduledJob(id, requestee) =>
        stopScheduledJob(state, id, requestee, ctx)
      | `GetScheduledJobs(requestee) => getScheduledJobs(state, requestee)
      | `GetScheduledJob(id, requestee) =>
        getScheduledJob(state, id, requestee)
      };
    },
    {scheduleState: Scheduling.empty, interval: None, lastMinute: (-1)},
  );