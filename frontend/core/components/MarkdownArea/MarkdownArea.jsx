import React from "react";

import MdEditor from "frontend/_MarkdownEditor/MarkdownEditor";



export function MarkdownArea(props) {

  const t = props.t;
  const extraClass   = props.extraClass;
  const identifier   = props.identifier;
  const labelText    = props.labelText;
  const placeholder  = props.placeholder;
  const register     = props.register;
  const errors       = props.errors;
  const isRequired   = props.isRequired;
  const minLen       = props.minLen;
  const maxLen       = props.maxLen;


  



  const { onChange, onBlur, name, ref } = 
    register(identifier, 
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
      }
    );

 



  return (
    <div className={`MarkdownArea ${extraClass}`}>
      <label className="MarkdownArea__label" htmlFor={identifier}>
        {t(labelText)}
      </label>
      <MdEditor
        placeholder={placeholder}
        identifier={identifier}
        rhf_onChange={onChange}
        rhf_onBlur={onBlur}
        rhf_name={name}
        rhf_ref={ref}
      />
      {errors[identifier] && <p className="MarkdownArea__error">{errors[identifier].message}</p>}
    </div>
  )
}




