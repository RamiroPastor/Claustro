import React from "react"
import ReactPaginate from "react-paginate"



export function ThreadPaginator(props) {

  const handlePaginatorClick = props.handlePaginatorClick;
  const pageCount = props.pageCount;


  return (
    <ReactPaginate
      breakLabel="..."
      onPageChange={handlePaginatorClick}
      pageRangeDisplayed={2}
      pageCount={pageCount}
      renderOnZeroPageCount={null}
      nextLabel="⟩"
      previousLabel="⟨"
      containerClassName="ThreadPaginator"
      breakClassName="ThreadPaginator__li ThreadPaginator__li--break"
      breakLinkClassName="ThreadPaginator__a ThreadPaginator__a--break"
      pageClassName="ThreadPaginator__li"
      pageLinkClassName="ThreadPaginator__a"
      activeClassName="ThreadPaginator__li--active"
      activeLinkClassName="ThreadPaginator__a--active"
      previousClassName="ThreadPaginator__li"
      previousLinkClassName="ThreadPaginator__a"
      nextClassName="ThreadPaginator__li"
      nextLinkClassName="ThreadPaginator__a"
      disabledClassName="ThreadPaginator__li--disabled"
      disabledLinkClassName="ThreadPaginator__a--disabled"
    />
  )
}