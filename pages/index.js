import Head from 'next/head'
import { serverSideTranslations } from 'next-i18next/serverSideTranslations';

import { Header  } from "frontend/core/layout/Header/Header"
import { LangNav } from 'frontend/core/layout/LangNav/LangNav';
import { SignIn } from "frontend/pages/SignIn/SignIn"



export async function getStaticProps({locale}) {
  const translations = 
    await serverSideTranslations(locale, ["common"])
  return({ props: {...translations}})
}



export default function Home() {
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
        <SignIn/>
      </main>
    </div>
  )
}
