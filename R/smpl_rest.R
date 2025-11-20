#' Count Records in Dataset
#' 
#' Returns the count of records in a dataset. The dataset can be filtered using two optional filtering conditions.
#'
#' @param arg_dataset A data frame or tibble to filter and count
#' @param arg_fl_rest An optional first filtering expression to apply before counting. Better to use on flag like safety set and to compute the % on that population.
#' @param arg_var_rest An optional second filtering expression to apply after the first filter
#' @param arg_lbl Output format for the count. Can be "nbr" (number only), "pct" (percentage only), or "nbr-pct" (number and percentage)
#'
#' @return A character string containing the count in the specified format:
#'   - For "nbr": The count as a number
#'   - For "pct": The percentage with % symbol
#'   - For "nbr-pct": Count followed by percentage in parentheses
#' 
#' @examples
#' # Load adsl data from this package
#' data("tlr_adsl", package = "RapTLR")
#' 
#' # Without filtering, return the number of rows
#' fct_smpl_rest(arg_dataset = tlr_adsl)
#' 
#' # Adding one restriction on safety set only 
#' fct_smpl_rest(arg_dataset = tlr_adsl, 
#'               arg_fl_rest = SAFFL == "Y" )
#' 
#' # Return the number of subjects Xanomeline High Dose in safety set
#' fct_smpl_rest(arg_dataset = tlr_adsl, 
#'               arg_fl_rest = SAFFL == "Y", 
#'               arg_var_rest = ARM == "Xanomeline High Dose") 
#' 
#' # Return number and pct based on overall safety set subject.
#' fct_smpl_rest(arg_dataset = tlr_adsl, 
#'               arg_fl_rest = SAFFL == "Y", 
#'               arg_var_rest = ARM == "Xanomeline High Dose", 
#'               arg_lbl = "nbr-pct") 
#' 
#' # Can also have multiple condition using '&'
#' fct_smpl_rest(arg_dataset = tlr_adsl, 
#'               arg_fl_rest = ARM == "Xanomeline High Dose" & AGE <= 60)
#'
#' # Using percentage format
#' fct_smpl_rest(arg_dataset = tlr_adsl,
#'               arg_var_rest = SEX == "F",
#'               arg_lbl = "pct")
#' 
#'
#' @export
#'
fct_smpl_rest <- function( arg_dataset, arg_fl_rest, arg_var_rest, arg_lbl = c( "nbr", "pct", "nbr-pct" ) ){

  arg_lbl <- match.arg( arg_lbl )
  
  if( !rlang::quo_is_null( enquo( arg_fl_rest ) ) ) arg_dataset <- arg_dataset %>% dplyr::filter( !!rlang::enexpr( arg_fl_rest ) )

  tot_obs <- nrow( arg_dataset )

  if( tot_obs == 0 ) return( NA_character_ )

  if( !rlang::quo_is_null( enquo( arg_var_rest ) ) ) arg_dataset_1 <- arg_dataset %>% dplyr::filter( !!rlang::enexpr( arg_var_rest ) ) else arg_dataset_1 <- arg_dataset
  
  arg_dataset_1 %>% 
    dplyr::count( ) %>% 
    dplyr::mutate( lbl =  dplyr::case_when( arg_lbl == "nbr" ~ as.character( n ),
                                            arg_lbl == "pct" ~ paste0( round( 100 * n / tot_obs, 1 ), "%" ),
                                            arg_lbl == "nbr-pct" ~ paste0( n, " (", round( 100 * n / tot_obs, 1 ), "%)" ) ) ) %>%
    dplyr::pull( lbl )
  
}

