/* module CronExpression: {
  type t;
  exception MalformedCronExpression;
  let parse: string => t;
};
/* module Protocol: {
     type scheduleId;
     module Response: {
       type creationResponse('a) = 'a
       constraint [> | `ScheduleCreated(scheduleId)] = 'a;
       type cancellationResponse('a) = 'a
       constraint [> | `ScheduleCancelled(scheduleId)] = 'a;
     };
     module Request: {
       type t('a);
       let createSchedule:
         (
           CronExpression.t,
           'a,
           Nact.actorRef('a),
           Nact.actorRef(Response.creationResponse(_))
         ) =>
         t('a);
       let cancel:
         (scheduleId, Nact.actorRef(Response.cancellationResponse(_))) => t('a);
     };
   };

   let createScheduleActor:
     (~parent: Nact.actorRef(_), ~key: string) =>
     Nact.actorRef(Protocol.Request.t('a)); */ */