#' Create the appendix of a docx file - combine link creation and add TLF tables
#'
#' This function takes an empty (or headed) docx file, links your output TLF files, and adds them to the docx. Please note that this function will automatically create a new folder "new_linked" within your path_TLFs for processing intermediate TLF files.
#'
#' @param path_TLFs Path to the folder where your table outputs (TLFs) are saved.
#' @param docx_object Path to the folder where your target docx is saved.
#' @param keyword IMPORTANT: please ensure your doc has this unique keyword placed where you want the Appendix section to start.
#' @param sections_structure: Optionnal: If NULL then all outputs from path_TLFs will be used and no section will be created. Otherwise format of list, csv or excel and subsections.
#' @param return_to_file Optionnal: Path and name of the docx document with the appendice. If NULL then the new docx document will be saved at the working directory as appendix_processed.docx 
#' @returns NULL, output a docx to the destination folder.
#' @examples
#' path_TLFs <- system.file("extdata/TLF_outputs", package = "RapTLR")
#' path_docx <- system.file("extdata", "TLR_shell.docx", package = "RapTLR")
#' path_result <- file.path(getwd( ), "docx_with_appendice")
#' # By default, no sections are specified. All tables are automatically grouped into a single section called "Outputs" in the appendice and saved in the current working directory as appendix_processed.docx.
#' run_apdx(path_TLFs = path_TLFs,
#'          docx_object = file.path(path_docx, "TLR_shell.docx"))
#'
#' # The path and name of the new docx object can be modifed with return_to_file argument.
#' run_apdx(path_TLFs = path_TLFs,
#'          docx_object = path_docx,
#'          return_to_file = path_result)
#'
#' # A CSV file can be used to define the structure of the appendices.
#' TLF_list_csv <- system.file("extdata", "TLF_list.csv", package = "RapTLR")
#' 
#' run_apdx(path_TLFs = path_TLFs,
#'          docx_object = path_docx,
#'          sections_structure = TLF_list_csv,
#'          return_to_file = path_result)
#' 
#' # Or an excel file 
#' TLF_list_xlsx <- system.file("extdata", "TLF_list.xlsx", package = "RapTLR")
#' 
#' run_apdx(path_TLFs = path_TLFs,
#'          docx_object = path_docx,
#'          sections_structure = TLF_list_xlsx,
#'          return_to_file = path_result)
#'
#' # Or a list following the below format :
#' output_list <- list(Efficacy = "tefmad01a",
#'                     Overall = "tsidem03",
#'                     Safety = c("tsfae10", "lsfae03"))
#' run_apdx(path_TLFs = path_TLFs,
#'          docx_object = path_docx,
#'          sections_structure = output_list)
#'
#'
#' @export


run_apdx <- function(path_TLFs,
                     docx_object,
                     keyword = "LISTOFAPPENDICES",
                     sections_structure = NULL,
                     return_to_file = NULL) {

  # Self Diagnostics ----

  ## path_TLFs ----
  if (missing(path_TLFs) || is.null(path_TLFs)) {
    stop("Error: 'path_TLFs' cannot be NULL. Please specify a valid path.")
  }

  if (!file.exists(path_TLFs)) stop("Error: Please provide a valid path for TLFs")
  if (!any(str_detect(list.files(path_TLFs), "docx"))) stop("Error: No docx outputs found in 'path_TLFs'")
  
  ## docx_object ----
  if (missing(docx_object) || is.null(docx_object)) {
    stop("Error: 'docx_object' cannot be NULL. Please specify a docx object.") }

  if (!file.exists(docx_object)) stop("Error: Please provide a valid path for 'docx_object'")
  if (!str_detect(docx_object, "docx")) stop("Error: No docx object in the path")

  ## Keyword ----
  if (is.null(keyword)) {
    stop("Error: 'keyword' cannot be NULL. Please specify a keyword.")
  }

  ## return_to_file ----
  if (is.null(return_to_file)) {
    return_to_file <- file.path(getwd(), "appendix_processed.docx")

  } else {
    return_to_file <- file.path(getwd(), return_to_file)

    if (!dir.exists(dirname(return_to_file)))
      stop("Error: Directory for 'return_to_file' does not exist")

    if (!grepl("\\.docx$", return_to_file, ignore.case = TRUE))
      return_to_file <- paste0(return_to_file, ".docx")
  }


  ##################################### Read in Sections Structure #####################################
  # Check output structure, if null all outputs otherwise outputs in Outputs columns
  if (!is.null(sections_structure)) {

    # Check if sections_structure is a link or list in R
    if (is.character(sections_structure)) {

      # Save the extension of file
      df_ext <- tools::file_ext(sections_structure)

      # Importing dataset depending on the extension. First row as colnames
      output_structure_in <- switch(df_ext,
                                    csv = read.csv(sections_structure),
                                    xls = readxl::read_excel(sections_structure, col_names = TRUE),
                                    xlsx = readxl::read_excel(sections_structure, col_names = TRUE),
                                    stop("Unsupported file type!"))

      if(ncol(output_structure_in) != 2) stop("Please import data with two columns only: Section and Outputs")
      
      # Renaming Column to Section and Outputs
      output_structure_in <- output_structure_in %>%
        dplyr::rename(Section = 1,  Outputs = 2)

      # Transforming dataset into list
      output_structure_list <- split(output_structure_in$Outputs, 
                                     factor(output_structure_in$Section, levels = unique(output_structure_in$Section)),
                                     drop = FALSE)

      # If list return sections_structure
    } else if (is.list(sections_structure)) {
      output_structure_list <- sections_structure
    }


    # Selected outputs
    all_TLFs <- paste0(purrr::list_c(output_structure_list), ".docx")

    # If null, all outputs
  } else {

    all_TLFs <- str_subset(list.files(path_TLFs), ".docx")
    output_structure_list <- setNames(list(stringr::str_remove(all_TLFs, ".docx")), "Outputs")

  }

  ##################################### Import and add Appendix #####################################
  # Importing docx_object in R
  imported_docx_object <- officer::read_docx(docx_object)


  ## This function is for catching addLink() function error
  ## If the return_to_file is not a valid path, catch the error and print to working directory instead
  addLink(path_TLFs = path_TLFs,
          outputs = all_TLFs)


  # Appendix creation
  outTLR1 <- addTLF2TLR(imported_docx_object = imported_docx_object,
                        path_TLFs = path_TLFs,
                        output_structure_list = output_structure_list,
                        keyword = "LISTOFAPPENDICES")
  

  print( outTLR1, return_to_file)
  message(paste0("Printing the result docx file with Appendix to ", return_to_file))

}
