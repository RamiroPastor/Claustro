import React from "react";

import { StarPolygonFirstSpecies } from "frontend/base/svg/starPolygon";



export function StarryBorder(
  props:
    { children : React.ReactElement
    }
) {

  const size = "1.2rem";

  function star(modifier: string, fill: string) {

    const styles = 
      { "--StarryBorder_starSize": size
      } as React.CSSProperties

    return(
      <svg 
        className={`StarryBorder__star StarryBorder__star--${modifier}`}
        style={styles}
        viewBox="-1 -1 2 2"
      >
        <StarPolygonFirstSpecies
          vertices={5}
          radius={1}
          center={[0,0]}
          strokeWidth={0.01}
          stroke="gold"
          fill={fill}
        />
      </svg>
    )
  }


  return(
    <div className="StarryBorder">
      {star("0", "purple")}
      {star("1", "gold")}
      {star("2", "red")}
      {star("3", "crimson")}
      {star("4", "coral")}
      {star("5", "lime")}
      {star("6", "turquoise")}
      {star("7", "orangered")}
      {star("8", "deeppink")}
      {star("9", "teal")}
      {props.children}
    </div>
  )
}