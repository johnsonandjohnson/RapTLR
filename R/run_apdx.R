#' @import officer
#' @import xml2
#' @import stringr
#' @import uuid
#' @import dplyr
#' @import rlang
NULL


#' Create the appendix of a docx file - combine link creation and add TLF tables
#'
#' This function takes an empty (or headed) docx file, links your output TLF files, and adds them to the docx. Please note that this function will automatically create a new folder "new_linked" within your path_TLFs for processing intermediate TLF files.
#'
#' @param path_TLFs Path to the folder where your table outputs (TLFs) are saved.
#' @param docx_object Path to the folder where your target docx is saved.
#' @param keyword IMPORTANT: please ensure your doc has this unique keyword placed where you want the Appendix section to start.
#' @param output FOR LATER: To select only some desired TLFs and not all.
#' @param sections_structure: If set to Null then all outputs from path_TLFs will be used and no section will be created. Otherwise format of list, csv or excel and subsections.
#' @param return_to_file Optional - if not specified, saving result docx file to the same folder with default name "RapTLR_Appendix_processed.docx". This function can auto-detect if the user specified a docx name along with it. If not, auto-create a file name and save to file.
#' @param return_object Optional - if set to T, the function will return an officer object in global environment regardless of return_to_file
#' @returns NULL, output a docx to the destination folder.
#' @examples
#' path_TLFs = "REPLACE WITH YOUR OWN PATH C:/Documents/TLR_folder/TLF_outputs"
#' path_docx = "REPLACE WITH YOUR OWN PATH C:/Documents/TLR_folder/"
#' path_result = "REPLACE WITH YOUR OWN PATH C:/Documents/TLR_folder/TLR_shell_processed.docx"
#' # By default, no sections are specified. All tables are automatically grouped into a single section called "Outputs" in the appendice.
#' run_apdx(path_TLFs = path_TLFs,
#'           docx_object = file.path(path_docx, "TLR_shell.docx"))
#'
#' # An excel file can be read to define the structure of the appendice with the corresponding outputs.
#' run_apdx(path_TLFs = path_TLFs,
#'          docx_object = file.path(path_docx, "TLR_shell.docx"),
#'          keyword = "LISTOFAPPENDICES",
#'          return_to_file = path_result)
#'
#' # A CSV file can also be used to define the structure of the appendices.
#' run_apdx(path_TLFs = path_TLFs,
#'          docx_object = file.path(path_docx, "TLR_shell.docx"),
#'          keyword = "LISTOFAPPENDICES",
#'          sections_structure = "REPLACE WITH YOUR OWN PATH C:/Documents/TLR_folder/TLF_list.csv",
#'          return_to_file = path_result)
#'
#' # Or a list following the below format :
#' output_list <- list(Efficacy = "tefmad01a",
#'                     Overall = "tsidem03",
#'                     Safety = c("tsfae10", "lsfae03"))
#' run_apdx(path_TLFs = path_TLFs,
#'          docx_object = file.path(path_docx, "TLR_shell.docx"),
#'          sections_structure = output_list)
#'
#'
#' @export


run_apdx <- function(path_TLFs,
                     docx_object,
                     keyword = "LISTOFAPPENDICES",
                     sections_structure = NULL,
                     return_to_file = NULL,
                     return_object = FALSE) {

  ##################################### Self Diagnostics #####################################

  # Function argument that can't be missing or NULL
  if (missing(path_TLFs) || is.null(path_TLFs)) {
    stop("Error: 'path_TLFs' cannot be NULL. Please specify a valid path.")
  }
  if (missing(docx_object) || is.null(docx_object)) {
    stop("Error: 'docx_object' cannot be NULL. Please specify a docx object.")
  }
  if (is.null(keyword)) {
    stop("Error: 'keyword' cannot be NULL. Please specify a keyword.")
  }

  # If not NULL check it function argument exist
  if (!file.exists(path_TLFs)) stop("Error: Please provide a valid path for TLFs")
  if (!any(str_detect(list.files(path_TLFs), "docx"))) stop("Error: No docx outputs found in 'path_TLFs'")

  if (!file.exists(docx_object)) stop("Error: Please provide a valid path for 'docx_object'")
  if (!str_detect(docx_object, "docx")) stop("Error: No docx object in the path")

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

      # Renaming Column to Section and Outputs
      output_structure_in <- output_structure_in %>%
        rename(Section = 1,  Outputs = 2)

      # Transforming dataset into list
      output_structure_list <- split(output_structure_in$Outputs, output_structure_in$Section)

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


  #message("Outputs to be imported into the appendice: ", paste(str_remove(all_TLFs, ".docx"), collapse = ", "))


  ## This function is for catching addLink() function error
  ## If the return_to_file is not a valid path, catch the error and print to working directory instead
  addLink(path_TLFs = path_TLFs,
          outputs = all_TLFs)




  # Appendix creation
  outTLR1 <- addTLF2TLR(imported_docx_object = imported_docx_object,
                        path_TLFs = path_TLFs,
                        output_structure_list = output_structure_list,
                        keyword = "LISTOFAPPENDICES")


  ## This function is for the chunk right below.
  ## If the return_to_file is not a valid path, catch the error and print to working directory instead
  printToFile <- function(outTLR1, return_to_file) {
    tryCatch(
      {
        message(paste0("Printing the result docx file with Appendix to ", return_to_file, "..."))
        suppressWarnings(print(outTLR1, return_to_file))
      },
      error = function(cond) {
        warning(paste0("return_to_file, the output path that you provided is invalid. Printing to the current working directory, which is \n",
                       getwd(), "RapTLR_Appendix_processed.docx"))
        print(outTLR1, "./RapTLR_Appendix_processed.docx")
        NA
      },
      warning = function(cond) {
        NULL
      },
      finally = {
        NULL
      }
    )
  }

  ## If return_to_file is specified, print to file.
  if (!is.null(return_to_file)) {
    ## Use the function above. If the return_to_file is not a valid path,
    ## catch the error and print to working directory instead
    printToFile(outTLR1, return_to_file)
  }

  # Return the officer object if return_object is set to True, or if return_to_file is not specified
  if (return_object == TRUE || is.null(return_to_file)) {
    return(outTLR1)
  }


}
