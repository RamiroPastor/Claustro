import React, { useState } from "react";
import MarkdownIt from "markdown-it";
import MarkdownItVideo from "markdown-it-video";

import { bold    } from "./icons/bold"
import { bulletL } from "./icons/bulletL"
import { header  } from "./icons/header"
import { help    } from "./icons/help"
import { hr      } from "./icons/hr"
import { image   } from "./icons/image"
import { italic  } from "./icons/italic"
import { link    } from "./icons/link"
import { numberL } from "./icons/numberL"
import { preview } from "./icons/preview"
import { redo    } from "./icons/redo"
import { screen  } from "./icons/screen"
import { undo    } from "./icons/undo"
import { video   } from "./icons/video"

// import "./_MarkdownEditor.scss"
import { utils } from "./MarkdownEditorUtils";





export default function MarkdownEditor(props) {
  

  const placeholder  = props.placeholder
  const identifier   = props.identifier
  const rhf_onChange = props.rhf_onChange
  const rhf_onBlur   = props.rhf_onBlur
  const rhf_name     = props.rhf_name
  const rhf_ref      = props.rhf_ref



  const mdParser = new MarkdownIt(
    { linkify: true
    }
  ).use(MarkdownItVideo, 
    { youtube: { width: 640, height: 390 }
    , vimeo: { width: 500, height: 281 }
    , vine: { width: 600, height: 600, embed: 'simple' }
    , prezi: { width: 550, height: 400 }
    })

  

  const mde$$     = document.querySelector(`#mde-${identifier}`);
  const area$$    = mde$$ ? mde$$.querySelector(".mde-textarea") : null;
  const preview$$ = mde$$ ? mde$$.querySelector(".mde-preview")  : null;

  const getAreaState = () => [area$$.value, area$$.selectionStart, area$$.selectionEnd];
  const applyAreaState = (info) => {
    area$$.value = info[0];
    area$$.selectionStart = info[1];
    area$$.selectionEnd = info[2];
  }

  const [areaState  , setAreaState  ] = useState([]);
  const [undoHistory, setUndoHistory] = useState([]);
  const [redoHistory, setRedoHistory] = useState([]);
  const [undoDisabled, setUndoDisabled] = useState(true);
  const [redoDisabled, setRedoDisabled] = useState(true);





  function areaInputHandler() {
    preview$$.innerHTML = mdParser.render(area$$.value);
    pushToUndoHistory()
  }
  
  function areaKeydownHandler(event) {
    let ctrlZ = event.keyCode === 90 && event.ctrlKey
    let ctrlY = event.keyCode === 89 && event.ctrlKey
    if (undoHistory.length !== 1 && ctrlZ)
      {event.preventDefault(); undoHandler(); return};
    if (redoHistory.length !== 0 && ctrlY)
      {event.preventDefault(); redoHandler(); return};
  };


  function pushToUndoHistory() {
    setRedoHistory([]);
    setRedoDisabled(true);
    setUndoHistory([...undoHistory, areaState]);
    setUndoDisabled(false);
    setAreaState(getAreaState());
    };


  function taInputProtocol() {
    pushToUndoHistory();
    preview$$.innerHTML = mdParser.render(area$$.value);
    area$$.focus();
  }


  function undoHandler() {
    setRedoHistory([...redoHistory, areaState]);
    setRedoDisabled(false);
    const info = undoHistory[undoHistory.length - 1];
    setUndoHistory(undoHistory.filter((v, i) => i !== (undoHistory.length - 1)))
    if (undoHistory.length === 2) {
      setUndoDisabled(true);
    };
    applyAreaState(info)
    setAreaState(info)
    preview$$.innerHTML = mdParser.render(area$$.value);
    area$$.focus();
  }

  function redoHandler() {
    setUndoHistory([...undoHistory, areaState]);
    setUndoDisabled(false);
    let info = redoHistory[redoHistory.length - 1]
    setRedoHistory(redoHistory.filter((v, i) => i !== (redoHistory.length - 1)))
    if (redoHistory.length === 1) {
      setRedoDisabled(true);
    }
    applyAreaState(info)
    setAreaState(info);
    preview$$.innerHTML = mdParser.render(area$$.value);
    area$$.focus();
  }



  function boldClickHandler()   {utils.markBothSides(area$$, "**", "bold text") ; taInputProtocol()}
  function italicClickHandler() {utils.markBothSides(area$$, "_", "italic text"); taInputProtocol()}
  function linkClickHandler()   {utils.insertLink(area$$, "link" ); taInputProtocol()}
  function imageClickHandler()  {utils.insertLink(area$$, "image"); taInputProtocol()}
  function videoClickHandler()  {utils.insertLink(area$$, "video"); taInputProtocol()}
  function headerClickHandler() {utils.insertHeading(area$$); taInputProtocol()}
  function hrClickHandler()     {utils.insertHR(area$$); taInputProtocol()}
  function bulletListHandler()  {utils.startList(area$$, "1."); taInputProtocol()}
  function numberListHandler()  {utils.startList(area$$, "-"); taInputProtocol()}
  



  function toggleFullscreen() {mde$$.classList.toggle("MarkdownEditor--fullscreen")}
  function toggleEditorMode() {
    const areaIsHidden = area$$.classList.contains("mde--hide");
    const previewIsHidden = preview$$.classList.contains("mde--hide");
    if (!areaIsHidden && !previewIsHidden) {
      preview$$.classList.add("mde--hide");
    } else {
      if (previewIsHidden) {
        area$$.classList.add("mde--hide");
        preview$$.classList.remove("mde--hide");
      } else {
        area$$.classList.remove("mde--hide");
      }
    }
  }
  



  const mkButton = (disabled, icon, tooltip, clickHandler) => {
    return(
      <button
        type="button"
        disabled={disabled}
        className={`MarkdownEditor__controlButton ${disabled ? "mde--disabled" : ""}`}
        title={tooltip}
        onClick={clickHandler}
      >
        {icon}
      </button>
    )
  }

  const boldBtn       = mkButton(false       , bold   , "bold"              , boldClickHandler);
  const italicBtn     = mkButton(false       , italic , "italic"            , italicClickHandler);
  const linkBtn       = mkButton(false       , link   , "hyperlink"         , linkClickHandler);
  const imageBtn      = mkButton(false       , image  , "image"             , imageClickHandler);
  const videoBtn      = mkButton(false       , video  , "video"             , videoClickHandler);
  const headerBtn     = mkButton(false       , header , "header"            , headerClickHandler);
  const hrBtn         = mkButton(false       , hr     , "horizontal rule"   , hrClickHandler);
  const bulletListBtn = mkButton(false       , bulletL, "bullet list"       , bulletListHandler);
  const numberListBtn = mkButton(false       , numberL, "number list"       , numberListHandler);
  const undoBtn       = mkButton(undoDisabled, undo   , "undo - Ctrl+Z"     , undoHandler);
  const redoBtn       = mkButton(redoDisabled, redo   , "redo - Ctrl+Y"     , redoHandler);
  const previewBtn    = mkButton(false       , preview, "switch editor mode", toggleEditorMode);
  const screenBtn     = mkButton(false       , screen , "switch screen mode", toggleFullscreen);
  const helpBtn = 
    <a 
      className="MarkdownEditor__controlButton"
      href="https://www.markdownguide.org/cheat-sheet/"
      title="help about markdown"
      target="blank_"
    >
      {help}
    </a>


  return(
    <div className="MarkdownEditor" id={`mde-${identifier}`}>
      <div className="MarkdownEditor__controlBar">
        <div className="MarkdownEditor__controlWrapper">
          <div className="MarkdownEditor__controlGroup">
            {boldBtn} {italicBtn}  
          </div>
          <div className="MarkdownEditor__controlGroup">
            {linkBtn} {imageBtn} {videoBtn}
          </div>
          <div className="MarkdownEditor__controlGroup">
            {headerBtn} {hrBtn} {bulletListBtn} {numberListBtn}
          </div>
          <div className="MarkdownEditor__controlGroup">
            {undoBtn} {redoBtn} {helpBtn}
          </div>
        </div>
        <div className="MarkdownEditor__controlWrapper MarkdownEditor__controlWrapper--extra">
          {previewBtn} {screenBtn}
        </div>
      </div>

      <div className="MarkdownEditor__textWrapper">
        <textarea
          id={identifier}
          className="mde-textarea MarkdownEditor__text MarkdownEditor__text--area"
          onInput={areaInputHandler}
          onKeyDown={areaKeydownHandler}
          placeholder={placeholder}
          onChange={rhf_onChange}
          onBlur={rhf_onBlur}
          name={rhf_name}
          ref={rhf_ref}
        />
        <div
          className="mde-preview MarkdownEditor__text MarkdownEditor__text--preview"
        ></div>
      </div>

    </div>
  )
}