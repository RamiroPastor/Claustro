import React from "react"

import { config } from "centre/config/config"
import { langToFlag } from "frontend/base/js/langToFlag" 
import { Msg2         } from "frontend/core/components/Msg2/Msg2"
import { SubmitButton } from "frontend/core/components/SubmitButton/SubmitButton"
import { TextInput    } from "frontend/core/components/TextInput/TextInput"



export function BoardForm(props){

  const t = props.t;
  const extraClass   = props.extraClass;
  const handleSubmit = props.handleSubmit;
  const register = props.register;
  const errors   = props.errors;
  const watch    = props.watch
  const submitText    = props.submitText;
  const disableSubmit = props.disableSubmit;
  const responseCode  = props.responseCode;



  return(
    <form 
      className={"BoardForm " + extraClass}
      onSubmit={handleSubmit}
    >

      <TextInput
        t={t}
        inputType="text"
        identifier="title"
        labelText={t("title")}
        register={register}
        errors={errors}
        watch={watch}
        isRequired={true}
        minLen={config.board.minLen_title}
        maxLen={config.board.maxLen_title}
        onlyAlphanum={false}
      />

      <TextInput
        t={t}
        inputType="text"
        identifier="description"
        labelText={t("description")}
        register={register}
        errors={errors}
        watch={watch}
        isRequired={true}
        minLen={config.board.minLen_description}
        maxLen={config.board.maxLen_description}
        onlyAlphanum={false}
      />

      <fieldset className="BoardForm__langSelector">
        <legend>{t("allowedLanguages")}</legend>
        { config.board.languages.map((lang, i) =>
          <label key={i}>
            <input 
              type="checkbox"
              value={lang}
              {...register("lang")}
            />
            {langToFlag(lang)}
            <span>{t(lang)}</span>
          </label>
        )}
      </fieldset>

      { responseCode !== 0 && responseCode !== 200 &&
      <Msg2
        isError={true}
        message={responseCode}
      />
      }

      <SubmitButton
        text={t(submitText)}
        disabled={disableSubmit}
      />
    </form>
  )
}