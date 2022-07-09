import { appWithTranslation } from 'next-i18next';

import './index.scss'



function App({ Component, pageProps }) {
  return <Component {...pageProps} />
}

export default appWithTranslation(App)
