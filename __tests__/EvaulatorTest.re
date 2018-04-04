open Jest;

open ExpectJs;

open Evaluators;

let randomTime = (~start, ~end_, ~step) =>
  Random.int((end_ - start) / step) * step + start;

let testIsInExpr = (name, f, ~lowerBound=0, ~upperBound) => {
  let randomTime = (~start=lowerBound, ~end_=upperBound, ~step=1, ()) =>
    randomTime(~start, ~end_, ~step);
  let testRandomValues = size => {
    let values =
      Belt.Array.range(0, size)
      |. Belt.Array.map((_) => randomTime())
      |. Js.Array.sortInPlace;
    let interval =
      Belt.Array.range(lowerBound, upperBound) |. Belt.List.fromArray;
    let (inValues, outOfValues) =
      Belt.List.partition(interval, i => Belt.Array.some(values, j => i == j));
    let inValuesTest = i =>
      test(
        "Time "
        ++ string_of_int(i)
        ++ "which is in the set of supplied values should evaluate to true",
        () =>
        expect(f(i, `Values(values))) |> toBe(true)
      );
    let outOfValuesTest = i =>
      test(
        "Time "
        ++ string_of_int(i)
        ++ "which is not in the set of supplied values should evaluate to false",
        () =>
        expect(f(i, `Values(values))) |> toBe(false)
      );
    inValues |. Belt.List.forEach(inValuesTest);
    outOfValues |. Belt.List.forEach(outOfValuesTest);
  };
  let testRandomInterval = (_) => {
    let timeA = randomTime();
    let timeB = randomTime();
    let start = min(timeA, timeB);
    let end_ = max(timeA, timeB);
    let step = Random.int(10) + 1;
    let interval = Belt.Array.rangeBy(start, end_, ~step);
    let inIntervalTest = i =>
      test(
        "Time "
        ++ string_of_int(i)
        ++ " which is inside the interval ["
        ++ string_of_int(start)
        ++ ", "
        ++ string_of_int(end_)
        ++ "] with a step size of"
        ++ string_of_int(step)
        ++ " should evaluate to true",
        () =>
        expect(f(i, `Interval((start, end_, step)))) |> toBe(true)
      );
    let outOfIntervalTest = i =>
      test(
        "Time "
        ++ string_of_int(i)
        ++ " which is outside of the interval ["
        ++ string_of_int(start)
        ++ ", "
        ++ string_of_int(end_)
        ++ "] with a step size of"
        ++ string_of_int(step)
        ++ " should evaluate to false",
        () =>
        expect(f(i, `Interval((start, end_, step)))) |> toBe(false)
      );
    interval |. Belt.Array.forEach(inIntervalTest);
    Belt.Array.range(start, end_)
    |. Belt.Array.keep(i => ! Belt.Array.some(interval, j => i == j))
    |. Belt.Array.forEach(outOfIntervalTest);
  };
  describe(
    name,
    () => {
      test("`Wildcard always evaluate to true", () =>
        expect(f(randomTime(), `Wildcard)) |> toBe(true)
      );
      Belt.Range.forEach(0, 3, testRandomInterval);
      Belt.Range.forEach(0, 8, testRandomValues);
    },
  );
};

let testIsInYear = () =>
  describe("isInYear", () =>
    test("`Wildcard always evaluate to true", () =>
      expect(isInYear(2099, `Wildcard)) |> toBe(true)
    )
  );

testIsInExpr("isInMinute", isInMinute, ~upperBound=59);

testIsInExpr("isInHour", isInHour, ~lowerBound=0, ~upperBound=23);

testIsInExpr("isInMonth", isInMonth, ~lowerBound=0, ~upperBound=23);

testIsInYear();