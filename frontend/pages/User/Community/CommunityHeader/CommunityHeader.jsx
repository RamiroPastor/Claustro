import Link from "next/link"

import { carnet   } from "frontend/assets/svg/carnet"
import { cogwheel } from "frontend/assets/svg/cogwheel"
import { people   } from "frontend/assets/svg/people"



export function CommunityHeader(props){

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
              {cogwheel}
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