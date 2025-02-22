type getter('value) = atom('value) => 'value;
type setter('value) = (atom('value), 'value) => unit;

type atomConfig('value) = {
  read: getter('value) => 'value,
  write: option((getter('value), setter('value), 'value) => unit),
};

/* Atom implementation */
module Atom = {
  type t('value) = {
    config: atomConfig('value),
    mutable cache: option('value),
    listeners: ref(list(unit => unit)),
  };

  let make = (~read, ~write=?, ()) => {
    config: {
      read,
      write,
    },
    cache: None,
    listeners: ref([]),
  };

  let subscribe = (atom, listener) => {
    atom.listeners := [listener, ...atom.listeners^];

    /* Return unsubscribe function */
    () => {
      atom.listeners := List.filter(l => l !== listener, atom.listeners^);
    };
  };

  let notify = atom => {
    List.iter(listener => listener(), atom.listeners^);
  };

  let invalidate = atom => {
    atom.cache = None;
  };
};

/* Store implementation */
module Store = {
  type t = {mutable atoms: list((Atom.t('value), 'value))};

  let make = () => {atoms: []};

  let get = (store, atom) => {
    switch (atom.Atom.cache) {
    | Some(value) => value
    | None =>
      let value =
        atom.config.read(atomValue => List.assoc(atomValue, store.atoms));
      atom.cache = Some(value);
      value;
    };
  };

  let set = (store, atom, value) => {
    switch (atom.config.write) {
    | Some(write) =>
      Atom.invalidate(atom);
      write(
        atomValue => List.assoc(atomValue, store.atoms),
        (atomToSet, newValue) => {
          store.atoms =
            List.map(
              ((a, v)) => a === atomToSet ? (a, newValue) : (a, v),
              store.atoms,
            )
        },
        value,
      );
      Atom.notify(atom);
    | None => raise(Invalid_argument("Atom is read-only"))
    };
  };
};

[@bs.module "react"]
external useState: 'a => ('a, ('a => 'a) => unit) = "useState";

[@bs.module "react"]
external useEffect: (unit => option(unit => unit), array('a)) => unit =
  "useEffect";

let store = Store.make();

let atom = initialValue => {
  Atom.make(
    ~read=_ => initialValue,
    ~write=(_get, set, atom, update) => {set(atom, update)},
    (),
  );
};

let useAtom = atom => {
  let (_, forceUpdate) = useState(0);

  useEffect(
    () => {
      let unsubscribe =
        Atom.subscribe(atom, () => {forceUpdate(count => count + 1)});
      Some(unsubscribe);
    },
    [|atom|],
  );

  let value = Store.get(store, atom);
  let setValue = update => Store.set(store, atom, update);

  (value, setValue);
};
