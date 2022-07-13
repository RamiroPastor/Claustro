import React, { useContext } from "react"

import { AuthContext } from "frontend/core/contexts/AuthContext"
import { SignIn } from "frontend/pages/User/SignIn/SignIn"


export function AuthGuard({children, ...props}) {

  const isUserAuthenticated = useContext(AuthContext).isUserAuthenticated;

  return(
    ! isUserAuthenticated()
    ? <SignIn/>
    : children
  )
}