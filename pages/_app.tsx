import React from "react"
import { AppProps } from "next/app"
import Head from "next/head"
import { appWithTranslation } from "next-i18next"

import { AuthProvider } from "frontend/core/contexts/AuthContext"
import { Header  } from "frontend/core/layout/Header/Header"
import { LangNav } from "frontend/core/layout/LangNav/LangNav"

import './index.scss'



function App({ Component, pageProps } : AppProps) {
  


  return (
    <div className="App">

      <AuthProvider>

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

        </AuthProvider>

    </div>
  )
}

export default appWithTranslation(App)
