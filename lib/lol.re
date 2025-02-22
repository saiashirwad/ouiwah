type something = {name: string};

type myVariant =
  | HasNothing
  | HasSingleInt(int)
  | HasSingleTuple((int, int))
  | HasMultipleInts(int, int)
  | HasMultipleTuples((int, int), (int, int));

let myVariant = HasNothing;

let myVariant2 = HasSingleInt(1);

let tup = (2, 3);

let someRecord = {name: "John"};

let list = [1, 2, 3];
let li = [2, ...list];

type intStringTuple = (int, string);
let ha = (1, "2");

let add = (a, b) => a + b;

let nameToString = (name: string) =>
  switch (name) {
  | "John" => "John is a good person"
  | "Jane" => "Jane is a good person"
  | _ => "Unknown person"
  };

let myFunc = (a: int, b: int): (int, int) => (a, b);

let myFunc2 = (a, b) => a + b;

let myFunc3 = (a: int, b: int): int => {
  let c = a + b;
  c;
};

let x: list(int) = [1, 2, 3];

let haha = (a: 'a) => a;
