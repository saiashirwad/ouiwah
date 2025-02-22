module Option = {
  let bind = (opt, f) =>
    switch (opt) {
    | None => None
    | Some(x) => f(x)
    };

  let (>>=) = bind;
  let return = x => Some(x);

  let map = (f, opt) => bind(opt, x => return(f(x)));
};

let (let*o) = Option.bind;
let ($) = Option.bind;

let something = (x: option(string), y: option(string)) => {
  let*o a = x;
  let*o b = y;
  Some(a ++ b);
};
