import React, { useState, useContext } from "react"
import { useForm } from "react-hook-form"
import { useRouter } from "next/router"

import { config } from "centre/config/config"
import { API } from "frontend/base/js/axios"
import { MarkdownArea } from "frontend/core/components/MarkdownArea/MarkdownArea"
import { Msg2         } from "frontend/core/components/Msg2/Msg2"
import { SubmitButton } from "frontend/core/components/SubmitButton/SubmitButton"
import { AuthContext } from "frontend/core/contexts/AuthContext"



export function PostCreate(props) {

  const t = props.t;
  const threadId = props.threadId;
  const replyBoxRef = props.replyBoxRef;

  const {register, formState: { errors }, handleSubmit, reset} = useForm();

  const [disableSubmit, setDisableSubmit] = useState(false);
  const [responseCode , setResponseCode ] = useState(0);

  const jwt = useContext(AuthContext).auth.token;
  const router = useRouter();


  const onSubmit = data => {
    setResponseCode(0);
    setDisableSubmit(true);
    API.post("/post/create", 
      { token: jwt
      , threadId: threadId
      , ...data
      })
      .then(
        res => {
          setDisableSubmit(false);
          setResponseCode(res.status);
          reset()
          router.push(`/thread/${threadId}`)
        },
        err => {
          setDisableSubmit(false);
          setResponseCode(err.message)
        }
      )
  }



  return(
    <form 
      className="PostCreate"
      ref={replyBoxRef}
      onSubmit={handleSubmit(onSubmit)}
    >

      <MarkdownArea
        t={t}
        extraClass="PostCreate__mdArea"
        identifier="post"
        labelText={t("yourPost")}
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
        text={t("publish")}
        disabled={disableSubmit}
      />
    </form>
  )
}