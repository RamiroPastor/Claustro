import React, { useContext, useEffect, useState } from "react"
import { useForm } from "react-hook-form"
import { useRouter } from "next/router"
import { useTranslation } from "next-i18next"

import { API } from "frontend/base/js/axios"
import { AuthContext } from "frontend/core/contexts/AuthContext"
import { ThreadForm } from "frontend/pages/Thread/ThreadForm/ThreadForm"



export function ThreadEdit(props) {

  const thread = props.thread;

  const t = useTranslation("common").t;

  const {register, formState: { errors }, watch, handleSubmit, reset, setValue} = useForm();

  const [disableSubmit, setDisableSubmit] = useState(false);
  const [responseCode , setResponseCode ] = useState(0);

  const jwt = useContext(AuthContext).auth.token;
  const router = useRouter();


  useEffect(() => {
    if (thread) {
      setValue("title", thread.title);
      setValue("description", thread.description)
      setValue("pinned", thread.pinned)
      setValue("locked", thread.locked)
    }
  }, [thread, setValue])


  const onSubmit = data => {
    setResponseCode(0);
    setDisableSubmit(true);
    API.post("/thread/edit", 
      { token: jwt
      , threadId: thread._id
      , ...data
      })
      .then(
        res => {
          setDisableSubmit(false);
          setResponseCode(res.status);
          reset()
          router.push(`/thread/${thread._id}`)
        },
        err => {
          setDisableSubmit(false);
          setResponseCode(err.message)
        }
      )
  }



  return(
    <div className="ThreadEdit">
      <div className="ThreadEdit__inner">
        <h3 className="ThreadEdit__header">
          <span>{t("configureThread")}</span>
          <strong>{thread._id}</strong>
        </h3>
        <ThreadForm
          t = {t}
          createOrEdit = "edit"
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