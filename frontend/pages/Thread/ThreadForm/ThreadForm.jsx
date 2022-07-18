import React from "react"

import { config } from "centre/config/config"
import { MarkdownArea } from "frontend/core/components/MarkdownArea/MarkdownArea"
import { Msg2         } from "frontend/core/components/Msg2/Msg2"
import { SubmitButton } from "frontend/core/components/SubmitButton/SubmitButton"
import { TextInput    } from "frontend/core/components/TextInput/TextInput"



export function ThreadForm(props) {

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
      className={`ThreadForm ${extraClass}`} 
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
        minLen={config.thread.minLen_title}
        maxLen={config.thread.maxLen_title}
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
        minLen={config.thread.minLen_description}
        maxLen={config.thread.maxLen_description}
        onlyAlphanum={false}
      />

      <MarkdownArea
        t={t}
        extraClass="ThreadForm__mdArea"
        identifier="post"
        labelText={t("firstPost")}
        placeholder=""
        register={register}
        errors={errors}
        isRequired={true}
        minLen={config.post.minLen}
        maxLen={config.post.maxLen}
      />

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