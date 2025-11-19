#' Count Records in Dataset
#' 
#' Returns the count of records in a dataset. The dataset can be optonially filtered. 
#'
#' @param arg_dataset A data frame or tibble to filter and count
#' @param arg_rest An optional filtering expression to apply before counting
#'
#' @return An integer count of records after an optionnal filtering
#' 
#' @examples
#' # Load adsl data from this package
#' data("tlr_adsl", package = "RapTLR")
#' 
#' # Without filtering, return the number of rows
#' fct_smpl_rest(arg_dataset = tlr_adsl)
#' 
#' # Adding one restriction on Xanomeline High Dose
#' fct_smpl_rest(arg_dataset = tlr_adsl, 
#'               arg_rest = ARM == "Xanomeline High Dose")
#' 
#' # Can also have multiple condition using '&'
#' fct_smpl_rest(arg_dataset = tlr_adsl, 
#'               arg_rest = ARM == "Xanomeline High Dose" & AGE <= 60)
#'
#' @export
#'
fct_smpl_rest <- function( arg_dataset, arg_rest ){
  
  if( !rlang::quo_is_null( enquo( arg_rest ) ) ) arg_dataset <- arg_dataset %>% dplyr::filter( !!rlang::enexpr( arg_rest ) )
  
  arg_dataset %>% 
    count( ) %>% 
    pull( )
  
}

