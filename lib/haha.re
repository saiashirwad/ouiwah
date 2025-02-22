open Bindings;
open Base;

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
