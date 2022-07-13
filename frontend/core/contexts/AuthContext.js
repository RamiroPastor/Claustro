import React, { useState } from 'react'

export 
  { AuthContext
  , AuthProvider
  }




const AuthContext = React.createContext();



function AuthProvider({children, ...props}){

  const anonymousState =
    { token: null
    , name : null
    }

  const [auth, setAuth] = useState(anonymousState);

  const kickUser = () => setAuth(anonymousState);

  const isUserAuthenticated = () => auth.token !== null



  return(
    <AuthContext.Provider
      value={
        { auth
        , setAuth
        , kickUser
        , isUserAuthenticated
        }
      }
    >
      {children}
    </AuthContext.Provider>
  )
}