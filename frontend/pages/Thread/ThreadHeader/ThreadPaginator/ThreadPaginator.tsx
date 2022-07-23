import React from "react"
import Pagination from "react-js-pagination"



export function ThreadPaginator(
  props:
    { activePage       : number
    , itemsPerPage     : number
    , totalItemsCount  : number
    , handlePageChange : (n: number) => void
    }
  ) {

  const activePage = props.activePage;
  const itemsCountPerPage = props.itemsPerPage;
  const totalItemsCount = props.totalItemsCount;
  const handlePageChange = props.handlePageChange;



  return (
    <Pagination
      activePage={activePage}
      itemsCountPerPage={itemsCountPerPage}
      totalItemsCount={totalItemsCount}
      pageRangeDisplayed={5}
      onChange={handlePageChange}
      innerClass="ThreadPaginator"
      itemClass="ThreadPaginator__li"
      itemClassFirst="ThreadPaginator__li"
      itemClassPrev="ThreadPaginator__li"
      itemClassNext="ThreadPaginator__li"
      itemClassLast="ThreadPaginator__li"
      disabledClass="ThreadPaginator__li--disabled"
      linkClass="ThreadPaginator__a"
      activeLinkClass="ThreadPaginator__a--active"
    />
  )
}