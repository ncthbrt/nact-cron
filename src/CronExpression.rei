type t = {
  minutes: [ | `Values(array(int)) | `Wildcard],
  hours: [ | `Values(array(int)) | `Wildcard],
  daysOfMonth: [ | `Values(array(int)) | `Wildcard | `LastDayOfMonth],
  months: [ | `Values(array(int)) | `Wildcard],
  daysOfWeek: [ | `Values(array(int)) | `Wildcard | `LastDayOfMonth(int)],
  expression: string,
};

exception MalformedCronExpression;

let parse: string => t;