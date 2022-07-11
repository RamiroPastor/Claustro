import { serverSideTranslations } from 'next-i18next/serverSideTranslations';

import { SignUp } from "/frontend/pages/User/SignUp/SignUp";



export async function getStaticProps({locale}) {
  const translations = await serverSideTranslations(locale, ["common"])
  return({ props: {...translations}})
}

export default function Handler(props) {
  return(
    <SignUp/>
  )
}