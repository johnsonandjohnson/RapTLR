#' Add bookmark to docx output titles
#'
#' Output IDs will be bookmarked
#'
#' @param x Cursor place in a docx document.
#' @param id ID where the cursor is place.
#' @return Creates bookmarked output that can be referenced in the body text.
#' @noRd
#' @import xml2

body_bookmark2 <- function(x, id) {
  # x=tb1; id=toupper(iduj)

  cursor_elt <- officer::docx_current_block_xml(x)
  ns_ <- "xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\""
  new_id <- uuid::UUIDgenerate()
  # id <- check_bookmark_id(id)
  bm_start_str <- sprintf("<w:bookmarkStart w:id=\"%s\" w:name=\"%s\" %s/>",
                          new_id, str_remove(id, "PART.*"), ns_)
  #
  # bm_start_str <- sprintf("<w:bookmarkStart w:id=\"%s\" w:name=\"%s\" %s/>",
  #                         new_id, str_remove( id, "PART.*"), ns_)

  bm_start_end <- sprintf("<w:bookmarkEnd %s w:id=\"%s\"/>",
                          ns_, new_id)
  path_ <- paste0(xml2::xml_path(cursor_elt), "//w:r")
  #
  # # ###
  node <- xml2::xml_find_first(x$doc_obj$get(), path_)
  node2 <- xml2::xml_find_all(x$doc_obj$get(), path_)
  # #
  t1 <- node
  t2 <- xml2::as_xml_document(t1)
  # #
  xml2::xml_text(t1) <- id
  xml2::xml_text(t2) <- ":"
  xml2::xml_add_sibling(node, t2, .where = "after")
  # #
  #
  # ###
  node <- xml2::xml_find_first(x$doc_obj$get(), path_)
  node2 <- xml_find_all(x$doc_obj$get(), path_)
  xml2::xml_add_sibling(node, xml2::as_xml_document(bm_start_str), .where = "before")
  xml2::xml_add_sibling(node, xml2::as_xml_document(bm_start_end), .where = "after")
  x
}
