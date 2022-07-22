import { ThreadResData } from "centre/Thread/ThreadResData"



export function sortThreads(threadList : ThreadResData[]) {

  const compare = (t1 : ThreadResData, t2 : ThreadResData) => {
    if (t1.pinned === t2.pinned) {
      const d1 = new Date(t1.lastActivity.date);
      const d2 = new Date(t2.lastActivity.date);
      return d2.getTime() - d1.getTime()
    } else {
      return t2.pinned - t1.pinned
    }
  }

  threadList.sort(compare)
}