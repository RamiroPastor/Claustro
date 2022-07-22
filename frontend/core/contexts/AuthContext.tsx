import React, { useState } from 'react'

export 
  { AuthContext
  , AuthProvider
  }

  


interface IAuth 
  { token: string | null
  , name:  string | null
  }


const anonymousState : IAuth =
  { token: null
  , name : null
  }


interface IAuthContext 
  { auth                : IAuth
  , saveAuth            : (auth : IAuth) => void
  , kickUser            : () => void
  , isUserAuthenticated : () => void
  }


const defaultContext : IAuthContext =
  { auth                : anonymousState
  , saveAuth            : (auth) => {}
  , kickUser            : () => {}
  , isUserAuthenticated : () => {}
  }




const AuthContext = React.createContext<IAuthContext>(defaultContext);



function AuthProvider({children} : {children: React.ReactNode }){

  const [auth, setAuth] = useState(anonymousState);

  const saveAuth = (auth : IAuth) => {
    // TODO: save in local storage
    setAuth(auth)
  }

  const kickUser = () => setAuth(anonymousState);

  const isUserAuthenticated = () => auth.token !== null



  return(
    <AuthContext.Provider
      value={
        { auth
        , saveAuth
        , kickUser
        , isUserAuthenticated
        }
      }
    >
      {children}
    </AuthContext.Provider>
  )
}