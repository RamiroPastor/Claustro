import Head from 'next/head'

import { Header } from "frontend/core/layout/Header/Header"
import { SignIn } from "frontend/pages/SignIn/SignIn"



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
      <main>
        <SignIn/>
      </main>
    </div>
  )
}
