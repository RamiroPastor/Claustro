import React from "react"

import { evenOddSplit } from "frontend/base/js/evenOddSplit";



export function StarPolygonFirstSpecies(
  props: 
    { vertices: number
    , radius  : number
    , center  : [number, number]
    , strokeWidth : number
    }
  ) {

  const n  = Math.round(props.vertices);
  const r  = props.radius - props.strokeWidth;
  const c1 = props.center[0];
  const c2 = props.center[1];
  const s  = props.strokeWidth;


  const α = 2 * Math.PI / n

  const vertice = (k: number) => [c1 + r * Math.cos(k*α) , c2 + r * Math.sin(k*α)];

  const nList = Array.from(Array(n).keys());
  const vs = nList.map(vertice);

  const oddVS : [number, number][] = evenOddSplit(vs)[0];
  const evenVS: [number, number][] = evenOddSplit(vs)[1];
  const vsvs  : [number, number][] = evenOddSplit(vs.concat(vs))[0];

  const f = (acc: string, v:[number, number]) => acc + ` L ${v[0]} ${v[1]}`

  const directions =
    (n % 2 === 0)
      ?   
          `M ${vs[0][0]} ${vs[0][1]}`
        + oddVS.reduce(f, "")
        + " Z"
        + `M ${vs[1][0]}  ${vs[1][0]}`
        + evenVS.reduce(f, "")
        + " Z"
      : 
          `M ${vs[0][0]} ${vs[0][1]}`
        + vsvs.reduce(f, "")
        + " Z"

  return(
    <path
      fill="red"
      stroke="black"
      strokeWidth={2*s}
      d={directions}
    />
  )
}