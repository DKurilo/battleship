import * as React from 'react';
import * as Types from '../types';

export const Comp: (g:Types.Component) => Types.Box = g => ({
  concat: (other: Types.Box) =>
    Comp(x=>(
      <React.Fragment>
        {g(x)}
        {other.fold(x)}
      </React.Fragment>
    )),
  contramap: (f:(p:Types.Props) => Types.Props) => Comp(x => g(f(x))),
  fold: g,
  map: (f:(e:Types.Element) => Types.Element) => Comp(x => f(g(x))),
});
