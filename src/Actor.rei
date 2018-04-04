let make:
  (Nact.actorRef('parent), ~key: string) =>
  Nact.actorRef(
    [
      | `DispatchPendingJobs
      | `GetScheduledJob(
          NactRecron.Scheduling.ScheduleId.t,
          Nact.actorRef(
            [>
              | `FoundScheduledJob(
                  NactRecron.Scheduling.schedule(
                    (Nact.actorRef('msg), 'msg),
                  ),
                )
              | `ScheduledJobNotFound(NactRecron.Scheduling.ScheduleId.t)
            ],
          ),
        )
      | `GetScheduledJobs(
          Nact.actorRef(
            [>
              | `ScheduledJobs(
                  list(
                    NactRecron.Scheduling.schedule(
                      (Nact.actorRef('msg), 'msg),
                    ),
                  ),
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
              | `ScheduledJobStarted(NactRecron.Scheduling.ScheduleId.t)
            ],
          ),
        )
      | `StopScheduledJob(
          NactRecron.Scheduling.ScheduleId.t,
          Nact.actorRef(
            [> | `ScheduledJobStarted(NactRecron.Scheduling.ScheduleId.t)],
          ),
        )
    ],
  );