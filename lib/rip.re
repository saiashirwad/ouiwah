open Base;

let (let*o) = (x, f) => Option.bind(x, ~f);
let (let|o) = (x, f) => Option.bind(x, ~f);

let c = {
  let*o aa = Some(5);
  Some(aa);
};
