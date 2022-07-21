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


const AuthContext = React.createContext({});



function AuthProvider({children} : {children: React.ReactNode }){

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