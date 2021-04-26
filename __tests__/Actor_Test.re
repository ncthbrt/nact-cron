open Jest;

open Expect;

open Nact;

open Nact.Operators;

let (>=>) = (f, g) => Js.Promise.then_(g, f);

module TimeKeeper = {
  type t;
  [@bs.module] external timekeeper : t = "timekeeper";
  [@bs.send] external freeze : (t, Js.Date.t) => unit = "freeze";
  [@bs.send] external reset : t => unit = "reset";
  [@bs.send] external travel : (t, Js.Date.t) => unit = "travel";
  [@bs.send] external isKeepingTime : t => bool = "isKeepingTime";
};

let delay = ms =>
  Js.Promise.make((~resolve, ~reject as _) =>
    ignore(Js.Global.setTimeout(() => resolve(. (): unit), ms))
  );

let return = Js.Promise.resolve;

let reject = msg => Js.Promise.reject(Failure(msg));

let passP = Js.Promise.resolve(pass);

[@bs.module "nact/test/mock-persistence-engine"] [@bs.new]
external createMockPersistenceEngine : unit => persistenceEngine =
  "MockPersistenceEngine";

describe("`StartScheduledJob", () => {
  testPromise(
    "A well formed cron expression should be able to be added as a schedule in the cronActor",
    () => {
      let system =
        start(~persistenceEngine=createMockPersistenceEngine(), ());
      let cronActor = NactCron.make(system, ~key="test");
      let actor =
        Nact.spawnStateless(system, (msg, _) => return(Js.log(msg)));
      cronActor
      <? (
        temp => `StartScheduledJob(("@daily", "hi", actor, temp)),
        500 * milliseconds,
      )
      >=> (
        fun
        | `ScheduledJobStarted(_) => passP
        | `MalformedCronExpression(_) => reject("`MalformedCronExpression")
        | `FunRandomPolymorphicVariant =>
          reject("`FunRandomPolymorphicVariant")
      );
    },
  );
  testPromise(
    "A malformed cron expression should  return the message `MalformedCronExpression",
    () => {
      let system =
        start(~persistenceEngine=createMockPersistenceEngine(), ());
      let cronActor = NactCron.make(system, ~key="test");
      let actor =
        Nact.spawnStateless(system, (msg, _) => return(Js.log(msg)));
      cronActor
      <? (
        temp => `StartScheduledJob(("badbadnotgood", "hi", actor, temp)),
        500 * milliseconds,
      )
      >=> (
        fun
        | `ScheduledJobStarted(_) =>
          reject(
            "This cron expression should not have been accepted by the actor",
          )
        | `MalformedCronExpression(_) => passP
      );
    },
  );
});

describe("`StopScheduledJob", () => {
  testPromise("Removal should be idempotent", () => {
    let system = start(~persistenceEngine=createMockPersistenceEngine(), ());
    let cronActor = NactCron.make(system, ~key="test");
    let actor =
      Nact.spawnStateless(system, (msg, _) => return(Js.log(msg)));
    cronActor
    <? (
      temp => `StartScheduledJob(("@daily", "hi", actor, temp)),
      500 * milliseconds,
    )
    >=> (
      fun
      | `ScheduledJobStarted(id) => {
          cronActor <-< `StopScheduledJob((id, nobody()));
          cronActor
          <? ((temp => `StopScheduledJob((id, temp))), 500 * milliseconds);
        }
      | `MalformedCronExpression(_) => reject("`MalformedCronExpression")
    )
    >=> (
      fun
      | `ScheduledJobStopped(_) => passP
    );
  });
  testPromise(
    "A previously created job should be able to be sucessfully removed", () => {
    let system = start(~persistenceEngine=createMockPersistenceEngine(), ());
    let cronActor = NactCron.make(system, ~key="test");
    let actor =
      Nact.spawnStateless(system, (msg, _) => return(Js.log(msg)));
    cronActor
    <? (
      temp => `StartScheduledJob(("@daily", "hi", actor, temp)),
      500 * milliseconds,
    )
    >=> (
      fun
      | `ScheduledJobStarted(id) =>
        cronActor
        <? ((temp => `StopScheduledJob((id, temp))), 500 * milliseconds)
      | `MalformedCronExpression(_) => reject("`MalformedCronExpression")
    )
    >=> (
      fun
      | `ScheduledJobStopped(id) =>
        cronActor
        <? ((temp => `GetScheduledJob((id, temp))), 500 * milliseconds)
    )
    >=> (
      fun
      | `ScheduledJobNotFound(_) => passP
      | `FoundScheduledJob(_) =>
        reject("Scheduled job should have been removed")
    );
  });
});

describe("`GetScheduledJobs", () =>
  testPromise("Should be able to get a list of scheduled jobs", () => {
    let system = start(~persistenceEngine=createMockPersistenceEngine(), ());
    let cronActor = NactCron.make(system, ~key="test");
    let actor =
      Nact.spawnStateless(system, (msg, _) => return(Js.log(msg)));
    cronActor <-< `StartScheduledJob(("@daily", "daily", actor, nobody()));
    cronActor <-< `StartScheduledJob(("@weekly", "weekly", actor, nobody()));
    cronActor <-< `StartScheduledJob(("@monthly", "monthly", actor, nobody()));
    cronActor
    <? (temp => `GetScheduledJobs(temp), 500 * milliseconds)
    >=> (
      fun
      | `ScheduledJobs(jobs) =>
        return(expect(jobs |. Belt.List.toArray) |> toHaveLength(3))
    );
  })
);

describe("`DispatchPendingJobs", () =>
  ()
);
