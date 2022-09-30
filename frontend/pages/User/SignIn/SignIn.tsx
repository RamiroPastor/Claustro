import React, { useState, useContext } from "react"
import Link from "next/link"
import { useRouter } from "next/router"
import { useForm } from "react-hook-form"
import { useTranslation } from "next-i18next"

import { config } from "centre/config/config"
import { SignInData } from "centre/User/SignInData"
import { API } from "frontend/base/js/axios"
import { Msg2         } from "frontend/core/components/Msg2/Msg2"
import { SubmitButton } from "frontend/core/components/SubmitButton/SubmitButton"
import { TextInput    } from "frontend/core/components/TextInput/TextInput"
import { AuthContext  } from "frontend/core/contexts/AuthContext"



export function SignIn() {

  const t = useTranslation("common").t;
  const {register, formState: { errors }, watch, handleSubmit} = useForm<SignInData>();

  const [disableSubmit, setDisableSubmit] = useState(false);
  const [responseCode , setResponseCode ] = useState(0);

  const router = useRouter();
  const saveAuth = useContext(AuthContext).saveAuth;


  const onSubmit = (data : SignInData) => {
    setResponseCode(0);
    setDisableSubmit(true);
    API.post("/user/sign-in", data)
      .then(
        res => {
          setDisableSubmit(false);
          setResponseCode(res.status);
          saveAuth({token: res.data.token, name: res.data.userResData.name})
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
          isError={false}
          message={responseCode}
        />
      : <></>
      }

      <SubmitButton
        text={t("enter")}
        disabled={disableSubmit}
      />

      <Link href="/user/sign-up">
        <a className="SignIn__link">
          {t("goToSignUp")}
        </a>
      </Link>

      </form>
    </div>
  )
}