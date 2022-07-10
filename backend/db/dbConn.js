import { connect } from "mongoose"


export async function dbConn() {
  const conn = connect(process.env.DB_URI);
  return conn
}