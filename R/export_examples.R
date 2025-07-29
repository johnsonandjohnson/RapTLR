#' Export built-in examples to a user-specified folder.
#'
#' This function takes in a user-specified folder and copy-paste the built-in examples (TLR shell, dummy TLFs, section structure csv, etc.) into the folder
#'
#' @param path_workingFolder user-specified folder path. Must be a valid path to a local folder.
#' @returns empty, writing to files only
#' @examples
#' # By default, not returning to file, pass the officer object to the next function one by one
#' export_examples(path_workingFolder = "C:/Documents/TLR_folder/")
#'
#' @export

export_examples <- function(path_workingFolder) {

  if (is.null(path_workingFolder)) {
    warning("path_workingFolder must be specified. Exporting to your working directory instead. Please see function help for details, help(export_examples).")
    path_workingFolder <- getwd()
  } else if (!file.exists(path_workingFolder)) {
    warning("path_workingFolder is not a valid path. Exporting to your working directory instead. Please double check the file path.")
    path_workingFolder <- getwd()
  }

  pkg_path <- path.package("RapTLR")
  pkg_path <- file.path(pkg_path, "/extdata")

  allfiles <- list.files(pkg_path)
  for (i in allfiles) {
    file.copy(file.path(pkg_path, i),
              paste0(path_workingFolder, "/"), recursive = TRUE)

  }

}
