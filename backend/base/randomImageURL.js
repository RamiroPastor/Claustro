

export function randomImg() {

  const seed = Math.floor( Math.random() * 100000 );

  return `https://picsum.photos/seed/${seed}/300`
}