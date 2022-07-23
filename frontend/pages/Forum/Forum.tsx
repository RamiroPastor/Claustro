import React, { useState } from "react"
import { useTranslation } from "next-i18next"

import { BoardResData } from 'centre/Board/BoardResData'
import { ThreadResData } from 'centre/Thread/ThreadResData'
import { UserResData } from 'centre/User/UserResData'
import { Board          } from "frontend/pages/Board/Board"
import { Modal_NewBoard } from "frontend/pages/Board/Modal_NewBoard/Modal_NewBoard"
import { ForumHeader   } from "./ForumHeader/ForumHeader"


export function Forum(
  props:
    { boardList  : BoardResData[]
    , threadList : ThreadResData[]
    , userList   : UserResData[]
    }
  ) {

  const boards     = props.boardList
  const threadList = props.threadList
  const userList   = props.userList
  
  const t = useTranslation("common").t;
  const [isModalActive_newBoard, setModalActive_newBoard ] = useState(false);
  const openModal_newBoard  = () => {setModalActive_newBoard(true)}
  const closeModal_newBoard = () => {setModalActive_newBoard(false)}

  /* useEffect(() => {
    const p = router.asPath;
    if (p.includes("#")) {
      const elemID = p.substring(p.indexOf("#"));
      router.replace("/", elemID, {shallow: true})
    }
  }, [router.basePath]) */

  /* seEffect(() => {
    const p = router.asPath;
    if (p.includes("#")) {
      const elemID = p.substring(1 + p.indexOf("#"));
      const y0 = document.getElementById(elemID).getBoundingClientRect().top;
      window.scrollTo(0, y0)
      console.log(y0)
    }
  }, [router.asPath]) */


  return(
    <div className="Forum">
      <div className="Forum__inner">
        <Modal_NewBoard
          t={t}
          isActive={isModalActive_newBoard}
          closeModal={closeModal_newBoard}
        />
        <ForumHeader 
          t={t}
          openModal={openModal_newBoard}
        />
        { (boards.length === 0)
        ? <div className="Forum__inactive">
            <p>{t("noActiveForums")}</p>
          </div>
        : boards.map((b,i) =>
            <Board
              key={i}
              board={b}
              threads={threadList.filter(th => b._id === th.boardId)}
              userList={userList}
            />
          )
        }
      </div>
    </div>
  )
}