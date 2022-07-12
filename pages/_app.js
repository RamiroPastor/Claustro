import { useState } from "react"
import Head from "next/head"
import { appWithTranslation } from "next-i18next"

import { UserContext } from "frontend/core/contexts/UserContext"
import { Header  } from "frontend/core/layout/Header/Header"
import { LangNav } from "frontend/core/layout/LangNav/LangNav"

import './index.scss'



function App({ Component, pageProps }) {

  const [user, setUser] = useState(
    { jwt:  null
    , name: null
    }
  )
  


  return (
    <div className="App">

      <UserContext.Provider value={{user, setUser}}>

        <Head>
          <meta name="viewport" content="width=device-width, initial-scale=1" />
          <link rel="icon" href="/logo.svg" />
          <title>Claustro</title>
          <meta name="description" content="Enclaustrados"/>
        </Head>

        <Header/>
        <LangNav/>
        <main>
          <Component {...pageProps} />
        </main>

        </UserContext.Provider>

    </div>
  )
}

export default appWithTranslation(App)
