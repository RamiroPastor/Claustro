
export const utils = 
  { markBothSides
  , insertLink
  , insertHeading
  , insertHR
  , startList
  }



function insert(taElem, text, x1, x2) {
  // insert text (or replace), and update selection ends (relatively)
  let v    = taElem.value
  let sel1 = taElem.selectionStart
  let sel2 = taElem.selectionEnd
  let init = v.substring( 0    , sel1     )
  let tail = v.substring( sel2 , v.length )
  taElem.value = init + text + tail
  taElem.selectionStart = sel1 + x1
  taElem.selectionEnd   = sel2 + x2
}


function alreadyMarkedBothSides(v, sel1, sel2, mark) {
  let lSide = v.substring(sel1 - mark.length, sel1)
  let rSide = v.substring(sel2 , sel2 + mark.length)
  return lSide === mark && rSide === mark
}


function markBothSides(taElem, mark, ph) {
  let v    = taElem.value
  let sel1 = taElem.selectionStart
  let sel2 = taElem.selectionEnd
  if (sel1 === sel2)
    insert(taElem, mark + ph + mark, mark.length, ph.length + mark.length);
    else {
      let text = v.substring(sel1,sel2);
      if (!alreadyMarkedBothSides(v, sel1, sel2, mark))
        insert(taElem, mark + text + mark, mark.length, mark.length);
        else {
          taElem.selectionStart = sel1 - mark.length;
          taElem.selectionEnd   = sel2 + mark.length;
          insert(taElem, text, 0, (-2)*mark.length)
        }
    }
}


function insertLink(taElem, linkType) {
  let prefix = "";
  let text   = "";
  let url    = "";
  if (linkType === "link") {
    text = "example link"
    url  = "http://example.com/"
  }
  if (linkType === "image") {
    prefix = "!"
    text   = "alternative text"
    url    = "https://picsum.photos/100.jpg"
  }
  if (linkType === "video") {
    prefix = "@"
    text   = "youtube"
    url    = "https://www.youtube.com/watch?v=CoJ6u8trLvg"
  }
  let t = '\n' + prefix + '[' + text + '](' + url + ') '
  insert(taElem, "",0,0)
  taElem.selectionEnd = taElem.selectionStart
  insert(taElem, t, 2 + prefix.length, 2 + prefix.length + text.length);
}



function insertHeading(taElem) {
  let v = taElem.value
  let selection = v.substring(taElem.selectionStart, taElem.selectionEnd);
  if (selection === "") {selection = "heading"};
  let prefix = "\n\n # "
  let suffix = "\n\n"
  let text = prefix + selection + suffix
  insert(taElem, text, prefix.length , prefix.length);
  taElem.selectionEnd = taElem.selectionStart + selection.length
}


function insertHR(taElem) {
  let prefix = "\n\n-----"
  let suffix = "\n\n"
  let text = prefix + suffix
  insert(taElem, text, text.length , text.length);
  taElem.selectionEnd = taElem.selectionStart
}


function startList(taElem, mark) {
  let v = taElem.value
  let selection = v.substring(taElem.selectionStart, taElem.selectionEnd);
  if (selection === "") {selection = "List item"};
  let prefix = "\n\n" + mark + " "
  let suffix = "\n\n"
  let text = prefix + selection + suffix
  insert(taElem, text, prefix.length , prefix.length);
  taElem.selectionEnd = taElem.selectionStart + selection.length
}