import React from "react"

import { langToFlag } from "frontend/base/js/langToFlag";



export function BoardHeader(props) {

  const t = props.t;
  const title = props.title;
  const desc  = props.desc;
  const langs = props.langs

  return(
    <div className="BoardHeader">

      <div className="BoardHeader__head">
        <h3>
          {title}
        </h3>
        <div className="BoardHeader__control">

        </div>
      </div>

      <div className="BoardHeader__description">
        <p>
          {desc}
        </p>
      </div>

      <div className="BoardHeader__langList">
        <em>{t("allowedLanguages")}:</em>
        { langs.map((lang, i) =>
          <span key={i} className="BoardHeader__lang">
            {langToFlag(lang)}
            {t(lang)}
          </span>
        )}
      </div>

    </div>
  )
}