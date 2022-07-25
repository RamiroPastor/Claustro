



export function evenOddSplit(list: any[]) {

  const odd  = list.filter((x,i) => i % 2 === 0);
  const even = list.filter((x,i) => i % 2 === 1);

  return(
    [odd, even]
  )
}