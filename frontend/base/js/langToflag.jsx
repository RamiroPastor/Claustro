
import { flagES } from "frontend/assets/svg/flagES"
import { flagGB } from "frontend/assets/svg/flagGB"



export function langToFlag(lang){
  if (lang === "es") { return flagES }
  return flagGB
} 