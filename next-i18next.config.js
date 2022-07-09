const path = require('path');



const i18nextConfig = 
  { i18n:
    { locales: ["es", "en"]
    , defaultLocale: "es"
    }
  , localePath: path.resolve("./frontend/assets/i18n/")
  , reloadOnPrerender: ("true" === process.env.I18N_RELOAD)
  }


module.exports = i18nextConfig