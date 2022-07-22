import React, { useState, useContext, useEffect } from "react"
import { useForm } from "react-hook-form"
import { useRouter } from "next/router"

import { BoardFormData } from "centre/Board/BoardFormData" 
import { BoardResData  } from "centre/Board/BoardResData"
import { API } from "frontend/base/js/axios"
import { ModalWindow } from "frontend/core/components/ModalWindow/ModalWindow"
import { AuthContext } from "frontend/core/contexts/AuthContext"
import { BoardForm } from "frontend/pages/Board/BoardForm/BoardForm"



export function Modal_EditBoard(
  props:
    { t          : (s: string) => string
    , isActive   : boolean
    , closeModal : () => void
    , board      : BoardResData
    }
  ) {

  const t = props.t;
  const isActive = props.isActive;
  const closeModal = props.closeModal;
  const board = props.board;

  const {register, formState: { errors }, watch, handleSubmit, setValue} = useForm<BoardFormData>();

  const [disableSubmit, setDisableSubmit] = useState(false);
  const [responseCode , setResponseCode ] = useState(0);

  const jwt = useContext(AuthContext).auth.token;
  const router = useRouter();



  useEffect(() => {
    if (board) {
      setValue("title", board.title);
      setValue("description", board.description)
      for (let i = 0; i < board.languages.length; i++) {
        setValue(`languages.${i}`, board.languages[i]);
      }
    }
  }, [board, setValue])



  const onSubmit = (data : BoardFormData) => {
    setResponseCode(0);
    setDisableSubmit(true);
    API.post("/board/edit", 
      { token: jwt
      , boardId: board._id
      , ...data
      })
      .then(
        res => {
          setDisableSubmit(false);
          setResponseCode(res.status);
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
      title      = {t("editBoard")}
    >
      <BoardForm
        t = {t}
        extraClass   = "Modal_EditBoard"
        handleSubmit = {handleSubmit(onSubmit)}
        register = {register}
        errors   = {errors}
        watch    = {watch}
        submitText    = "saveChanges"
        disableSubmit = {disableSubmit}
        responseCode  = {responseCode}
      />
    </ModalWindow>
  )
}