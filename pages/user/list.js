import { serverSideTranslations } from 'next-i18next/serverSideTranslations';




export async function getStaticProps({locale}) {
  const translations = await serverSideTranslations(locale, ["common"])
  return({ props: {...translations}})
}

export default function Handler(props) {
  return(
    <h1>
      ESTO SERÁ LA LISTA DE USUARIOS
    </h1>
  )
}