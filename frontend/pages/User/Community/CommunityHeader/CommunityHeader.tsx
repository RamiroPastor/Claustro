import Link from "next/link"
import { TFunction } from "next-i18next"

import { carnet } from "frontend/assets/svg/carnet"
import { cogset } from "frontend/assets/svg/cogset"
import { people } from "frontend/assets/svg/people"



export function CommunityHeader(
  props :
    { t : (s : string) => string
    , viewPermissions : boolean
    , switchView      : () => void
    }
  ){

  const t = props.t;
  const viewPermissions = props.viewPermissions;
  const switchView = props.switchView;



  return(
    <div className="CommunityHeader">

      <h1 className="CommunityHeader__title">
        {t("community")}
      </h1>

      <div className="CommunityHeader__control">
        <button
          className="CommunityHeader__controlButton"
          type="button"
          onClick={switchView}
        >
          { viewPermissions
          ? <>
              {people}
              <span>{t("userList")}</span>
            </>
          : <>
              {cogset}
              <span>{t("permissionSettings")}</span>
            </>
          }
        </button>
        <Link href="/user/sign-up">
          <a className="CommunityHeader__controlButton">
            {carnet}
            <span>{t("createUser")}</span>
          </a>
        </Link>
      </div>

    </div>
  )
}