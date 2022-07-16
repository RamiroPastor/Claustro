import React, { useState, useContext } from "react"
import { useForm } from "react-hook-form"

import { config } from "centre/config/config"
import { API        } from "frontend/base/js/axios"
import { langToFlag } from "frontend/base/js/langToFlag" 
import { ModalWindow  } from "frontend/core/components/ModalWindow/ModalWindow"
import { Msg2         } from "frontend/core/components/Msg2/Msg2"
import { SubmitButton } from "frontend/core/components/SubmitButton/SubmitButton"
import { TextInput    } from "frontend/core/components/TextInput/TextInput"
import { AuthContext  } from "frontend/core/contexts/AuthContext"



export function NewBoardModal(props) {

  const t = props.t;
  const isActive = props.isActive;
  const setActive = props.setActive;

  const {register, formState: { errors }, watch, handleSubmit} = useForm();

  const [disableSubmit, setDisableSubmit] = useState(false);
  const [responseCode , setResponseCode ] = useState(0);

  const jwt = useContext(AuthContext).auth.token;


  const onSubmit = data => {
    setResponseCode(0);
    setDisableSubmit(true);
    API.post("/board/create", {token: jwt, ...data})
      .then(
        res => {
          setDisableSubmit(false);
          setResponseCode(res.status);
          setActive(false);
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
      title      = {t("createNewBoard")}
    >
      <form className="NewBoardModal" onSubmit={handleSubmit(onSubmit)}>

        <TextInput
          t={t}
          inputType="text"
          identifier="title"
          labelText={t("title")}
          register={register}
          errors={errors}
          watch={watch}
          isRequired={true}
          minLen={config.board.minLen_title}
          maxLen={config.board.maxLen_title}
          onlyAlphanum={false}
        />

        <TextInput
          t={t}
          inputType="text"
          identifier="description"
          labelText={t("description")}
          register={register}
          errors={errors}
          watch={watch}
          isRequired={true}
          minLen={config.board.minLen_description}
          maxLen={config.board.maxLen_description}
          onlyAlphanum={false}
        />

        <fieldset className="NewBoardModal__langSelector">
          <legend>{t("allowedLanguages")}</legend>
          { config.board.languages.map((lang, i) =>
            <label key={i}>
              <input 
                type="checkbox"
                value={lang}
                {...register("lang")}
              />
              {langToFlag(lang)}
              <span>{t(lang)}</span>
            </label>
          )}
        </fieldset>

        { responseCode !== 0 && responseCode !== 200 &&
        <Msg2
          isError={true}
          message={responseCode}
        />
        }

        <SubmitButton
          text={t("create")}
          disabled={disableSubmit}
        />
      </form>
    </ModalWindow>
  )
}