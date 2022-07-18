import React, { useState } from "react";

import { eyeClosed } from "frontend/assets/svg/eyeClosed";
import { eyeOpened } from "frontend/assets/svg/eyeOpened";



export function TextInput(props) {

  const t = props.t;
  const inputType    = props.inputType;
  const identifier   = props.identifier;
  const labelText    = props.labelText;
  const register     = props.register;
  const errors       = props.errors;
  const watch        = props.watch;
  const isRequired   = props.isRequired;
  const minLen       = props.minLen;
  const maxLen       = props.maxLen;
  const onlyAlphanum = props.onlyAlphanum;

  let patternValue = /.*/;
  let patternMessage = "";

  if (onlyAlphanum) {
    // esta regex admite "@" Y NO SE POR QUÉ ?!?!?!?!?!
    patternValue = /^[a-z\d](?:[a-z\d]|-(?=[a-z\d]))*$/i
    patternMessage = t("notAlphanumError", {field: t(identifier)})
  }
  if (inputType === "email") {
    patternValue = /^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*$/
    patternMessage = t("invalidFormatError")
  }


  const inputValue = watch(identifier);
  const [showPassword, setShowPassword] = useState(false);



  const inputElement = 
    <input
      className="TextInput__trueInput"
      type={(inputType !== "password") ? inputType : (showPassword ? "text" : "password")} 
      {...register(identifier, 
        { required: 
          { value: isRequired
          , message: t("requiredError", {field: t(identifier)})
          }
        , minLength:
          { value: minLen
          , message: t("tooShortError", {field: t(identifier), minlen: minLen})
          }
        , maxLength: 
          { value: maxLen
          , message: t("tooLongError", {field: t(identifier), maxlen: maxLen})
          }
        , pattern: 
          { value: patternValue 
          , message: patternMessage
          }
        }
      )}
    />

  
  const passwordVisibilityButton = 
    inputType !== "password"
      ? ""
      :
      <button 
        type="button"
        className="TextInput__passwordButton" 
        onClick={() => setShowPassword(!showPassword)}
      >
        {showPassword ? eyeOpened : eyeClosed}
      </button>



  return (
    <div className="TextInput">

      <label className="TextInput__wrapper">
        <div className="TextInput__input">
          <span className={`TextInput__labelText ${inputValue && inputValue !== "" ? "TextInput__labelText--small" : ""}`}>
            {labelText}
          </span>
          {inputElement}
        </div>
        {passwordVisibilityButton}
      </label>

      {errors[identifier] && <p className="TextInput__error">{errors[identifier].message}</p>}

    </div>
  )
}