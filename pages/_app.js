import Head from 'next/head'
import { appWithTranslation } from 'next-i18next';

import { Header  } from "frontend/core/layout/Header/Header"
import { LangNav } from 'frontend/core/layout/LangNav/LangNav';

import './index.scss'



function App({ Component, pageProps }) {
  return (
    <div className="App">
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
    </div>
  )
}

export default appWithTranslation(App)
