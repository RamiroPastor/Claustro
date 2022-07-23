import { serverSideTranslations } from 'next-i18next/serverSideTranslations'

import { BoardResData } from "centre/Board/BoardResData"
import { PostResData } from "centre/Post/PostResData"
import { ThreadResData } from "centre/Thread/ThreadResData"
import { UserResData } from "centre/User/UserResData"
import { Thread } from "frontend/pages/Thread/Thread"



interface PostResData_Typicode 
  { userId : number
  , id     : number
  , title  : string
  , body   : string
  }


function toPostResData_Claustro(p: PostResData_Typicode) : PostResData {

  return(
    { _id       : p.id.toString()
    , userId    : p.userId.toString()
    , threadId  : "Typicode_posts"
    , body      : `## ${p.title} \n\n ${p.body}`
    , createdAt : new Date().toJSON()
    , updatedAt : new Date().toJSON()
    }
  )
}

function mockTypicodeUser(n: number, postCount: number) : UserResData {

  const numberToWord = (k: number) : string => {
    if (k === 1) { return "One"}
    if (k === 2) { return "Two"}
    if (k === 3) { return "Three"}
    if (k === 4) { return "Four"}
    if (k === 5) { return "Five"}
    if (k === 6) { return "Six"}
    if (k === 7) { return "Seven"}
    if (k === 8) { return "Eight"}
    if (k === 9) { return "Nine"}
    if (k === 10) { return "Ten"}
    return "NaN"
  }

  return(
    { _id     : n.toString()
    , name    : numberToWord(n)
    , email   : `${n}@users.typicode`
    , posts   : postCount
    , picture : `https://picsum.photos/seed/${n}/300`
    , createdAt : new Date().toJSON()
    , updatedAt : new Date().toJSON()
    } 
  )
}



export async function getServerSideProps(
  props: 
    { locale : string
    }
  ) {

  const translations = await serverSideTranslations(props.locale, ["common"])

  const rawData = await fetch("https://jsonplaceholder.typicode.com/posts");

  const data : PostResData_Typicode[] = await rawData.json();

  const board : BoardResData =
    { _id: "Typicode_jsonplaceholder"
    , createdByUser: "Typicode"
    , title: "Typicode Json Placeholder"
    , description: "-"
    , languages: ["latin"]
    , priority: 0
    , archived: false
    , createdAt: new Date().toJSON()
    , updatedAt: new Date().toJSON()
    }

  const thread : ThreadResData =
    { _id: "Typicode_posts"
    , createdByUser: "Typicode"
    , boardId: "Typicode_jsonplaceholder"
    , title: "Typicode Posts API"
    , description: "Posts fetched from Typicode jsonplaceholder API"
    , pinned: 0
    , locked: true
    , postCount: data.length
    , lastActivity: {userId: data[0].id.toString(), date: new Date().toJSON()}
    , createdAt: new Date().toJSON()
    , updatedAt: new Date().toJSON()
    }

  const postList = data.map(toPostResData_Claustro);

  const uniqueUserIds = Array.from(new Set(data.map(p => p.userId)))

  const userList = uniqueUserIds.map(n => mockTypicodeUser(n, data.filter(p => n === p.userId).length))
  
  return ({ props: {...translations, board, thread, postList, userList}})
}



export default function Handler(
  props:
    { board    : BoardResData
    , thread   : ThreadResData
    , postList : PostResData[]
    , userList : UserResData[]
    }
  ) {
  
  return(
    <Thread
      board={props.board}
      thread={props.thread}
      postList={props.postList}
      userList={props.userList}
    />
  )
}