#' Group and Count Unique Subjects by Variable
#'
#' This function groups a dataset by a specified variable and counts the number of
#' unique subjects (USUBJID) in each group. Optionally applies a filter condition
#' before grouping.
#'
#' @param arg_data A data frame containing the data to be analyzed. Must include
#'   a USUBJID column for subject identification.
#' @param arg_grp A grouping variable (unquoted). The data will be grouped by this
#'   variable before counting unique subjects.
#' @param arg_fl_rest Optional filter condition (unquoted). If provided, the data
#'   will be filtered before grouping. Default is NULL (no filtering).
#'
#' @return A data frame with two columns:
#'   \item{arg_grp}{The grouping variable values}
#'   \item{n_rand}{Count of unique subjects (USUBJID) in each group}
#'
#' @examples
#' data(tlr_adsl)
#'
#' # Count subjects by treatment arm
#' fct_grp_tbl(tlr_adsl, TRT01A)
#'
#' # Count subjects by treatment arm, filtered to safety population
#' fct_grp_tbl(tlr_adsl, TRT01A, SAFFL == "Y")
#'
#' # Count subjects by age group for 24weeks completed subjects
#' fct_grp_tbl(tlr_adsl, AGEGR1, COMP24FL == "Y")
#'
#' @export
fct_grp_tbl <- function( arg_data, arg_grp, arg_fl_rest = NULL ){ 
    
  cond_fl <- rlang::enquo( arg_fl_rest )

  data_tmp <- arg_data %>% 
    dplyr::filter( if( !rlang::quo_is_null( cond_fl )) {{ cond_fl }} else TRUE ) %>%
    dplyr::group_by( {{ arg_grp }} ) %>% 
    dplyr::summarise( n_rand = dplyr::n_distinct( USUBJID ) )

  return( data_tmp )
}


