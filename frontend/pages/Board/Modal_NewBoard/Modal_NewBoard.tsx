import React, { useState, useContext } from "react"
import { useForm } from "react-hook-form"
import { useRouter } from "next/router"

import { BoardFormData } from "centre/Board/BoardFormData"
import { API } from "frontend/base/js/axios"
import { ModalWindow  } from "frontend/core/components/ModalWindow/ModalWindow"
import { AuthContext  } from "frontend/core/contexts/AuthContext"
import { BoardForm } from "frontend/pages/Board/BoardForm/BoardForm"



export function Modal_NewBoard(
  props:
    { t          : (s: string) => string
    , isActive   : boolean
    , closeModal : () => void
    }
  ) {

  const t = props.t;
  const isActive = props.isActive;
  const closeModal = props.closeModal;

  const {register, formState: { errors }, watch, handleSubmit, reset} = useForm<BoardFormData>();

  const [disableSubmit, setDisableSubmit] = useState(false);
  const [responseCode , setResponseCode ] = useState(0);

  const jwt = useContext(AuthContext).auth.token;
  const router = useRouter();


  const onSubmit = (data : BoardFormData) => {
    setResponseCode(0);
    setDisableSubmit(true);
    API.post("/board/create", {token: jwt, ...data})
      .then(
        res => {
          setDisableSubmit(false);
          setResponseCode(res.status);
          reset()
          closeModal();
          router.replace(router.asPath)
        },
        err => {
          setDisableSubmit(false);
          setResponseCode(err.message)
        }
      )
  }



  return(
    <ModalWindow
      isActive   = {isActive}
      closeModal = {closeModal}
      title      = {t("createNewBoard")}
    >
      <BoardForm
        t = {t}
        extraClass   = "Modal_NewBoard"
        handleSubmit = {handleSubmit(onSubmit)}
        register = {register}
        errors   = {errors}
        watch    = {watch}
        submitText    = "create"
        disableSubmit = {disableSubmit}
        responseCode  = {responseCode}
      />
    </ModalWindow>
  )
}