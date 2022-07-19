

export function sortThreads(threadList) {

  const compare = (t1,t2) => {
    if (t1.pinned === t2.pinned) {
      const d1 = new Date(t1.lastActivity.date);
      const d2 = new Date(t2.lastActivity.date);
      return d2 - d1
    } else {
      return t2.pinned - t1.pinned
    }
  }

  threadList.sort(compare)
}