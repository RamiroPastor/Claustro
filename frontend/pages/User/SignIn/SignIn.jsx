import React, { useState, useContext } from "react"
import { useRouter } from "next/router"
import { useForm } from "react-hook-form"
import { useTranslation } from "next-i18next"

import { config } from "centre/config/config"
import { API } from "frontend/base/js/axios"
import { Msg2         } from "frontend/core/components/Msg2/Msg2"
import { SubmitButton } from "frontend/core/components/SubmitButton/SubmitButton"
import { TextInput    } from "frontend/core/components/TextInput/TextInput"
import { AuthContext  } from "frontend/core/contexts/AuthContext"



export function SignIn(props) {

  const t = useTranslation("common").t;
  const {register, formState: { errors }, watch, handleSubmit} = useForm();

  const [disableSubmit, setDisableSubmit] = useState(false);
  const [responseCode , setResponseCode ] = useState(0);

  const router = useRouter();
  const setAuth = useContext(AuthContext).setAuth;


  const onSubmit = data => {
    setResponseCode(0);
    setDisableSubmit(true);
    API.post("/user/sign-in", data)
      .then(
        res => {
          setDisableSubmit(false);
          setResponseCode(res.status);
          setAuth({token: res.data.token, name: res.data.name})
          if (router.pathname === "/user/sign-in") {
            router.push("/")
          }
        },
        err => {
          setDisableSubmit(false);
          setResponseCode(err.message)
        }
      )
  }

  return(
    <div className="SignIn">
      <form className="SignIn__form" onSubmit={handleSubmit(onSubmit)}>

      <h2 className="SignIn__header">
        {t("welcomeTo")} Claustro
      </h2>

      <TextInput
        t={t}
        inputType="email"
        identifier="email"
        labelText={t("email")}
        register={register}
        errors={errors}
        watch={watch}
        isRequired={true}
        minLen={config.user.minLen_email}
        maxLen={config.user.maxLen_email}
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