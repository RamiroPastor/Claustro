

export function sort_newerFirstWithPrio(list, dateField, prioField) {

  // Esta función es un nido de bugs

  const compare = (a,b) => {
    if (a[prioField] === b[prioField]) {
      const aDate = new Date(a[dateField]);
      const bDate = new Date(b[dateField]);
      return bDate - aDate
    } else {
      return b[prioField] - a[prioField]
    }
  }

  list.sort(compare)
}