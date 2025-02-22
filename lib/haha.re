open Bindings;
open Core;
open Yojson;

// [@deriving of_yojson]
// type user = {
//   id: int,
//   name: string,
// };

// [@deriving of_yojson]
// type int_pair = (int, int);

[@deriving compare]
type t = {
  id: int,
  aliases: Set.M(String).t,
};

let something = (x: option(string), y: option(string), z: option(string)) => {
  let*o a = x;
  let*o b = y;
  let*o c = z;
  Some(a ++ b ++ c);
};
