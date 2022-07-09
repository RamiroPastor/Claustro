import React, { useState } from "react"
import { useForm } from "react-hook-form"

import { config       } from "centre/config/config";
import { Msg2         } from "frontend/core/components/Msg2/Msg2"
import { SubmitButton } from "frontend/core/components/SubmitButton/SubmitButton"
import { TextInput    } from "frontend/core/components/TextInput/TextInput"



export function SignIn(props) {

  const t = x => x
  const {register, formState: { errors }, watch, handleSubmit} = useForm();

  const [disableSubmit, setDisableSubmit] = useState(false);
  const [responseCode , setResponseCode ] = useState(0);


  const onSubmit = data => {
    setResponseCode(0);
    setDisableSubmit(true);
    console.log(data)
  }

  return(
    <div className="SignIn">
      <form className="SignIn__form" onSubmit={handleSubmit(onSubmit)}>

      <h2 className="SignIn__header">
        Welcome to Claustro
      </h2>

      <TextInput
        t={t}
        inputType="text"
        identifier="name"
        labelText={t("username")}
        register={register}
        errors={errors}
        watch={watch}
        isRequired={true}
        minLen={config.user.minLen_name}
        maxLen={config.user.maxLen_name}
        onlyAlphanum={false}
      />

      <TextInput
        t={t}
        inputType="password"
        identifier="password"
        labelText={t("password")}
        register={register}
        errors={errors}
        watch={watch}
        isRequired={true}
        minLen={config.user.minLen_password}
        maxLen={config.user.maxLen_password}
        onlyAlphanum={false}
      />

      { responseCode !== 0 && responseCode !== 200
      ? <Msg2
          isError={true}
          message={responseCode}
        />
      : <></>
      }

      <SubmitButton
        text={t("enter")}
        disabled={disableSubmit}
      />

      </form>
    </div>
  )
}