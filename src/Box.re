module System = {
  let spacingScale = [|4, 8, 12, 16, 24, 32, 48|];

  type paddingSpace = [ Css.length | `sc(int) | `mq(list(paddingSpace))];

  type marginSpace = [
    Css.length
    | `sc(int)
    | `mq(list(marginSpace))
    | `auto
  ];

  let rec createPadding = (paddingValue: paddingSpace): Css.length => {
    Belt.Array.(
      switch (paddingValue) {
      | `sc(v) =>
        spacingScale
        ->get(v)
        ->Belt.Option.mapWithDefault(`px(0), x => `px(x))
      | `mq(l) =>
        l
        ->Belt.List.head
        ->Belt.Option.mapWithDefault(`px(0), x => x->createPadding)
      | _ => `px(0)
      }
    );
  };

  let rec createMargin = (paddingValue: marginSpace): [ Css.length | `auto] => {
    Belt.Array.(
      switch (paddingValue) {
      | `sc(v) =>
        spacingScale
        ->get(v < 0 ? (-1) * v : v)
        ->Belt.Option.mapWithDefault(`px(0), x =>
            v < 0 ? `px((-1) * x) : `px(x)
          )
      | `mq(l) =>
        l
        ->Belt.List.head
        ->Belt.Option.mapWithDefault(`px(0), x => x->createMargin)
      | `auto => `auto
      | _ => `px(0)
      }
    );
  };

  let determineValue = (~main, ~axis, ~exact) => {
    switch (main, axis, exact) {
    | (_, _, Some(exactValue)) => exactValue
    | (_, Some(axisValue), None) => axisValue
    | (Some(mainValue), None, None) => mainValue
    | (None, None, None) => `zero
    };
  };

  let makeCss = (~all, ~horizontal, ~vertical, ~top, ~bottom, ~left, ~right) => {
    let left = determineValue(~main=all, ~axis=horizontal, ~exact=left);
    let right = determineValue(~main=all, ~axis=horizontal, ~exact=right);
    let top = determineValue(~main=all, ~axis=vertical, ~exact=top);
    let bottom = determineValue(~main=all, ~axis=vertical, ~exact=bottom);

    (top, bottom, left, right);
  };

  let make = (~all, ~horizontal, ~vertical, ~top, ~bottom, ~left, ~right) => {
    let (t, b, l, r) =
      makeCss(~all, ~horizontal, ~vertical, ~top, ~bottom, ~left, ~right);

    [
      t->createPadding->Css.paddingTop,
      b->createPadding->Css.paddingBottom,
      r->createPadding->Css.paddingRight,
      l->createPadding->Css.paddingLeft,
    ];
  };
};

[@react.component]
let make =
    (
      ~children,
      ~p: option(System.paddingSpace)=?,
      ~px: option(System.paddingSpace)=?,
      ~py: option(System.paddingSpace)=?,
      ~pt: option(System.paddingSpace)=?,
      ~pb: option(System.paddingSpace)=?,
      ~pl: option(System.paddingSpace)=?,
      ~pr: option(System.paddingSpace)=?,
      ~m: option(System.marginSpace)=?,
      ~mx: option(System.marginSpace)=?,
      ~my: option(System.marginSpace)=?,
      ~mt: option(System.marginSpace)=?,
      ~mb: option(System.marginSpace)=?,
      ~ml: option(System.marginSpace)=?,
      ~mr: option(System.marginSpace)=?,
    ) => {
  System.make(
    ~all=p,
    ~horizontal=px,
    ~vertical=py,
    ~top=pt,
    ~bottom=pb,
    ~left=pl,
    ~right=pr,
  );
  <div> children </div>;
};