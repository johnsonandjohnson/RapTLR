#' UP - Add bookmarks to docx output titles
#'
#' In the folder where the docx outputs are, it will look for each output names, access and for each
#' output, the ID title will be bookmarked and can be cross-referenced in the body text
#'
#' @param path_TLFs Path of the TLFs.
#' @param outputs Outputs to be bookmarked.
#' @return Creates bookmarked output that can be referenced in the body text.
#' @noRd
#'
## Create Linked outputs for TLR ----
addLink <- function(path_TLFs = NULL,
                    outputs = NULL) {

  # Create a bookmarked directory to store new bookmarked outputs
  if (!dir.exists(file.path(path_TLFs, "bookmarked"))) {

    dir.create(file.path(path_TLFs, "bookmarked"))
    message("Creation of bookmarked folder here: ", file.path(path_TLFs, "bookmarked"))

  } else {

    unlink(file.path(path_TLFs, "bookmarked"), recursive = TRUE)
    message("Bookmarked folder already exist, reinitialisation of bookmarked folder")
    dir.create(file.path(path_TLFs, "bookmarked"))

  }

  # Save new path for bookmarked output
  path_docx_bk <- file.path(path_TLFs, "bookmarked")

  # Gather all files in path
  vect_file <- file.path(path_TLFs, outputs)

  message("Outputs to be bookmarked: ", paste(str_remove(outputs, ".docx"), collapse = ", "))

  ## New code to ignore errors
  for (i in seq_along(vect_file)) {
    x <- vect_file[i]
    y <- outputs[i] ## please keep this small inefficiency as it conserves the legacy annotation from above

    tb1 <- officer::read_docx(x)

    tbi <- officer::docx_summary(tb1)

    ## To address the issue Yannick discovered, search the title by row_id instead of is_header when users create their own TLF files.
    if (sum(tbi$is_header, na.rm = TRUE) > 0) {
      tbj <- tbi[which(tbi$is_header), ]
      tbj <- tbj[order(tbj$row_id), ]
    } else {
      tbj <- tbi[!is.na(tbi$row_id) & tbi$row_id == 1, ]
    }

    if (nrow(tbj) > 0) {
      idujj <- strsplit(tbj$text[1], ":")[[1]][1]

      tb1 <- officer::cursor_reach(tb1, idujj)

      #tb1 <- RapTLR:::body_bookmark2(tb1, idujj)
      tb1 <- body_bookmark2(tb1, idujj)

      print(tb1, target = file.path(path_docx_bk, paste0("LK", y)))

      message(y, " processed. Saved here: ", file.path(path_docx_bk, paste0("LK", y)))
    } else {
      warning(paste0("Your output file, ", y,
                     ", table title is not formatted from standard rtf file from programmer's output. Adding withtout a bookmark instead (your table of content will show a broken link). This may be due to the fact that you are not using docx converted from programmers' rtf outputs."))
    }
  }
}
