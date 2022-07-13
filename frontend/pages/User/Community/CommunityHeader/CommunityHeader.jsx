import Link from "next/link"


export function CommunityHeader(props){

  const t = props.t;
  const switchView = props.switchView;



  return(
    <div className="CommunityHeader">

      <h1 className="CommunityHeader__title">
        {t("users")}
      </h1>

      <div className="CommunityHeader__control">
        <button
          className="CommunityHeader__controlButton"
          type="button"
          onClick={switchView}
        >
          {t("permissionSettings")}
        </button>
        <Link href="/user/sign-up">
          <a className="CommunityHeader__controlButton">
            {t("createUser")}
          </a>
        </Link>
      </div>

    </div>
  )
}