import React, { useState } from "react"
import { useRouter } from "next/router";
import { SubmitHandler, useForm } from "react-hook-form"
import { useTranslation } from "next-i18next";

import { config } from "centre/config/config";
import { SignUpData } from "centre/User/SignUpData";
import { API } from "frontend/base/js/axios";
import { Msg2         } from "frontend/core/components/Msg2/Msg2"
import { SubmitButton } from "frontend/core/components/SubmitButton/SubmitButton"
import { TextInput    } from "frontend/core/components/TextInput/TextInput"




export function SignUp() {

  const t = useTranslation("common").t;
  const {register, formState: { errors }, watch, handleSubmit} = useForm<SignUpData>();

  const [disableSubmit, setDisableSubmit] = useState(false);
  const [responseCode , setResponseCode ] = useState(0);

  const router = useRouter();


  const onSubmit : SubmitHandler<SignUpData> = (data: SignUpData) => {
    setResponseCode(0);
    setDisableSubmit(true);
    API.post("/user/sign-up", data)
      .then(
        res => {
          setDisableSubmit(false);
          setResponseCode(res.status);
          router.push("/user/community");
        },
        err => {
          setDisableSubmit(false);
          setResponseCode(err.message)
        }
      )
  }



  return(
    <div className="SignUp">
      <form className="SignUp__form" onSubmit={handleSubmit(onSubmit)}>

      <h2 className="SignUp__header">
        {t("registerNewUser")}
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
        text={t("createUser")}
        disabled={disableSubmit}
      />

      </form>
    </div>
  )
}