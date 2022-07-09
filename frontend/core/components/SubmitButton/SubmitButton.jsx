import React from "react";




export function SubmitButton(props) {

  const text       = props.text;
  const disabled   = props.disabled;

  const loadingIcon = <div className="lds-ellipsis"><div></div><div></div><div></div><div></div></div>

  return(
    <button
      type="submit"
      disabled={disabled}
      className={`SubmitButton ${disabled ? "SubmitButton--disabled" : ""}`}
    >
      {disabled ? loadingIcon : text}
    </button>
  )
}