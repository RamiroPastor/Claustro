import React, { useState, useContext, useEffect } from "react"
import { useForm } from "react-hook-form"
import { useRouter } from "next/router"

import { API        } from "frontend/base/js/axios"
import { ModalWindow  } from "frontend/core/components/ModalWindow/ModalWindow"
import { AuthContext  } from "frontend/core/contexts/AuthContext"
import { BoardForm } from "frontend/pages/Board/BoardForm/BoardForm"



export function Modal_EditBoard(props) {

  const t = props.t;
  const isActive = props.isActive;
  const setActive = props.setActive;
  const board = props.board;

  const {register, formState: { errors }, watch, handleSubmit, setValue} = useForm();

  const [disableSubmit, setDisableSubmit] = useState(false);
  const [responseCode , setResponseCode ] = useState(0);

  const jwt = useContext(AuthContext).auth.token;
  const router = useRouter();



  useEffect(() => {
    if (board) {
      setValue("title", board.title);
      setValue("description", board.description)
      for (let i = 0; i < board.languages.length; i++) {
        setValue("lang." + i, board.languages[i]);
      }
    }
  }, [board, setValue])



  const onSubmit = data => {
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
          setActive(false);
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
      setActive  = {setActive}
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