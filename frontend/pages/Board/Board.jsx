import React from "react"



export function Board(props) {

  const board = props.board

  return(
    <div className="Board">
      <h3>{board.title}</h3>
      <p>{board.description}</p>
    </div>
  )
}