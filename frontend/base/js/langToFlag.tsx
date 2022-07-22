
import { flagES } from "frontend/assets/svg/flagES"
import { flagGB } from "frontend/assets/svg/flagGB"



export function langToFlag(lang : string) : React.ReactNode {
  if (lang === "es") { return flagES }
  return flagGB
} 