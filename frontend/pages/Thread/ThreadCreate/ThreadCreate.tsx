import React, { useState, useContext } from "react"
import { useForm } from "react-hook-form"
import { useRouter } from "next/router"
import { useTranslation } from "next-i18next"

import { ThreadFormData } from "centre/Thread/ThreadFormData"
import { API } from "frontend/base/js/axios"
import { AuthContext } from "frontend/core/contexts/AuthContext"
import { ThreadForm } from "frontend/pages/Thread/ThreadForm/ThreadForm"



export function ThreadCreate() {

  const t = useTranslation("common").t;

  const {register, formState: { errors }, watch, handleSubmit, reset} = useForm<ThreadFormData>();

  const [disableSubmit, setDisableSubmit] = useState(false);
  const [responseCode , setResponseCode ] = useState(0);

  const jwt = useContext(AuthContext).auth.token;
  const router = useRouter();


  const onSubmit = (data : ThreadFormData) => {
    setResponseCode(0);
    setDisableSubmit(true);
    API.post("/thread/create", 
      { token: jwt
      , boardId: router.query.board_id
      , ...data
      })
      .then(
        res => {
          setDisableSubmit(false);
          setResponseCode(res.status);
          reset()
          router.push("/")
        },
        err => {
          setDisableSubmit(false);
          setResponseCode(err.message)
        }
      )
  }



  return(
    <div className="ThreadCreate">
      <div className="ThreadCreate__inner">
        <h3 className="ThreadCreate__header">
          <span>{t("newThreadInBoard")}</span>
          <strong>{router.query.board_title}</strong>
        </h3>
        <ThreadForm
          t = {t}
          createOrEdit = "create"
          extraClass   = ""
          handleSubmit = {handleSubmit(onSubmit)}
          register = {register}
          errors   = {errors}
          watch    = {watch}
          disableSubmit = {disableSubmit}
          responseCode  = {responseCode}
        />
      </div>
    </div>
  )
}