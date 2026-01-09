#' Create a Formatted Text Description of Variable Distribution
#' 
#' @description
#' Creates a formatted text string describing the distribution of a categorical variable,
#' including counts and percentages for each level.
#'
#' @param arg_text_desc Character string to prepend to the output (default: "")
#' @param arg_dataset Data frame containing the categorical variable to analyze
#' @param arg_fl_rest Optional flag filtering condition (default: NULL)
#' @param arg_var_rest Unquoted name of the variable to analyze
#' @param arg_order Logical indicating whether to sort by frequency (default: FALSE)
#' @param arg_cvt_stg Function to apply to labels (default: identity function)
#' @param arg_nbr_obs Number of levels to include (default: Inf for all)
#' @param arg_lbl Format for displaying counts: "nbr" (count only), "pct" (percentage only), or "nbr-pct" (both)
#'
#' @return A character string containing the formatted description
#' 
#' @examples
#' # Basic usage - show race distribution with default formatting
#' fct_var_text(arg_dataset = tlr_adsl, arg_var_rest = RACE)
#' 
#' # Add descriptive prefix text
#' fct_var_text(arg_text_desc = "Patients were", arg_dataset = tlr_adsl, arg_var_rest = RACE)
#' 
#' # Convert labels to sentence case
#' fct_var_text(arg_dataset = tlr_adsl, arg_var_rest = RACE, arg_cvt_stg = str_to_sentence)
#' 
#' # Sort by frequency (most common first)
#' fct_var_text(arg_dataset = tlr_adsl, arg_var_rest = RACE, arg_order = TRUE)
#' 
#' # Show only counts
#' fct_var_text(arg_dataset = tlr_adsl, arg_var_rest = RACE, arg_lbl = "nbr")
#' 
#' # Show only percentages
#' fct_var_text(arg_dataset = tlr_adsl, arg_var_rest = RACE, arg_lbl = "pct")
#' 
#' # Show both counts and percentages
#' fct_var_text(arg_dataset = tlr_adsl, arg_var_rest = RACE, arg_lbl = "nbr-pct")
#' 
#' # Limit to top 3 most frequent categories
#' fct_var_text(arg_dataset = tlr_adsl, arg_var_rest = RACE, arg_order = TRUE, arg_nbr_obs = 3)
#' 
#' # Filter data before summarizing (e.g., only safety population)
#' fct_var_text(arg_dataset = tlr_adsl, arg_fl_rest = SAFFL == "Y", arg_var_rest = RACE)
#' 
#' # Complete example with multiple options
#' fct_var_text(
#'   arg_text_desc = "The most common reasons for treatment discontinuation were:",
#'   arg_dataset = tlr_adsl,
#'   arg_fl_rest = DISCONFL == "Y", 
#'   arg_var_rest = DCREASCD,
#'   arg_order = TRUE,
#'   arg_nbr_obs = 3,
#'   arg_cvt_stg = str_to_lower,
#'   arg_lbl = "nbr-pct"
#' )
#'
#' @export
#' 

fct_var_text <- function( arg_text_desc = "", arg_dataset, arg_fl_rest = NULL, arg_var_rest, arg_order = FALSE, arg_cvt_stg = I, arg_nbr_obs = Inf, arg_lbl = c( "pct", "nbr", "nbr-pct" ) ){
  
  arg_lbl <- match.arg(arg_lbl)

  cond_fl <- rlang::enquo( arg_fl_rest )

  lbl_expr <- switch(
    arg_lbl,
    "nbr" = rlang::expr( as.character( paste0( "n=", n ) ) ),
    "pct" = rlang::expr( paste0( pct, "%" ) ),
    "nbr-pct" = rlang::expr( paste0( "n=", n, "; ", pct, "%" ) ),
    rlang::expr( as.character( n ) )
  )
  
  paste( arg_text_desc, 
         arg_dataset %>% 
           dplyr::filter( if( !rlang::quo_is_null( cond_fl ) ) {{ cond_fl }} else TRUE ) %>% 
           dplyr::select( {{ arg_var_rest }} ) %>%
           dplyr::count( {{ arg_var_rest }}, sort = arg_order ) %>% 
           dplyr::mutate( pct = round( n / sum( n ) * 100, 1 ),  
                   lbl = paste0( arg_cvt_stg( {{ arg_var_rest }} ), " (", !!lbl_expr, ")"  )) %>% 
           dplyr::slice_head( n = arg_nbr_obs ) %>% 
           dplyr::pull( lbl ) %>% 
           paste( collapse = ", ") %>% 
           stringr::str_replace( replacement = " and", pattern = ",(?!.*,)" ), 
         sep = " " ) %>% 
    stringr::str_squish( )
}



