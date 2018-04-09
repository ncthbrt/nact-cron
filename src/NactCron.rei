module Scheduling = NactCron_Scheduling;

let make:
  (Nact.actorRef('parent), ~key: string) =>
  Nact.actorRef(
    [
      | `DispatchPendingJobs
      | `ProcessedMinute(int)
      | `GetScheduledJob(
          Scheduling.ScheduleId.t,
          Nact.actorRef(
            [>
              | `FoundScheduledJob(
                  Scheduling.schedule((Nact.actorRef('msg), 'msg)),
                )
              | `ScheduledJobNotFound(Scheduling.ScheduleId.t)
            ],
          ),
        )
      | `GetScheduledJobs(
          Nact.actorRef(
            [>
              | `ScheduledJobs(
                  list(Scheduling.schedule((Nact.actorRef('msg), 'msg))),
                )
            ],
          ),
        )
      | `StartScheduledJob(
          string,
          'msg,
          Nact.actorRef('msg),
          Nact.actorRef(
            [>
              | `MalformedCronExpression(string)
              | `ScheduledJobStarted(Scheduling.ScheduleId.t)
            ],
          ),
        )
      | `StopScheduledJob(
          Scheduling.ScheduleId.t,
          Nact.actorRef([> | `ScheduledJobStopped(Scheduling.ScheduleId.t)]),
        )
    ],
  );