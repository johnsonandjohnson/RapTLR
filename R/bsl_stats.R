#' Generate Baseline Statistics Summary Text
#'
#' This function calculates baseline statistics (mean/median with SD/range) for a numeric
#' variable by treatment group and generates a formatted text summary suitable for clinical
#' study reports. It can include overall totals and specific treatment groups in the output.
#'
#' @param arg_dataset A data frame containing the baseline data to analyze.
#' @param arg_grp A grouping variable (unquoted) for treatment groups (e.g., ARM, TRT01A).
#' @param arg_trt_inc Optional character vector of treatment group names to include in the
#'   detailed summary. If NULL, only the total is reported. Default is NULL.
#' @param arg_var_num A numeric variable (unquoted) to calculate statistics for (e.g., AGE, WEIGHT).
#' @param arg_var1 The central tendency measure to calculate. Options are "mean" or "median".
#'   Default is "mean".
#' @param arg_var2 The dispersion measure to calculate. Options are "sd" (standard deviation)
#'   or "range" (min, max). Default is "sd".
#' @param arg_unit Character string for the unit of measurement (e.g., "years", "kg", "mg/dL").
#'   Default is "" (no unit).
#' @param arg_label Character string describing the variable for the output text
#'   (e.g., "age", "weight", "baseline score"). Default is "".
#'
#' @return A character string containing a formatted summary of baseline statistics,
#'   including the specified central tendency and dispersion measures, suitable for
#'   inclusion in clinical study reports.
#'
#' @examples
#' data(tlr_adsl)
#'
#' # Calculate mean age with SD for all subjects
#' fct_bsl_stats(
#'   arg_dataset = tlr_adsl,
#'   arg_grp = ARM,
#'   arg_var_num = AGE,
#'   arg_var1 = "mean",
#'   arg_var2 = "sd",
#'   arg_unit = "years",
#'   arg_label = "age"
#' )
#'
#' # Calculate median weight with range, including treatment groups
#' fct_bsl_stats(
#'   arg_dataset = tlr_adsl,
#'   arg_grp = ARM,
#'   arg_trt_inc = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"),
#'   arg_var_num = WEIGHTBL,
#'   arg_var1 = "median",
#'   arg_var2 = "range",
#'   arg_unit = "kg",
#'   arg_label = "baseline weight"
#' )
#'
#' # Calculate mean height with SD by treatment arm
#' fct_bsl_stats(
#'   arg_dataset = tlr_adsl,
#'   arg_grp = TRT01A,
#'   arg_trt_inc = c("Placebo", "Xanomeline High Dose"),
#'   arg_var_num = HEIGHTBL,
#'   arg_var1 = "mean",
#'   arg_var2 = "sd",
#'   arg_unit = "cm",
#'   arg_label = "baseline height"
#' )
#'
#' # Calculate median baseline score with range (no treatment breakdown)
#' fct_bsl_stats(
#'   arg_dataset = tlr_adsl,
#'   arg_grp = ARM,
#'   arg_var_num = BMIBL,
#'   arg_var1 = "median",
#'   arg_var2 = "range",
#'   arg_unit = "kg/m²",
#'   arg_label = "baseline BMI"
#' )
#'
#' @export
fct_bsl_stats <- function( arg_dataset,
                           arg_grp, 
                           arg_trt_inc = NULL, 
                           arg_var_num,
                           arg_var1 = c( "mean", "median" ), 
                           arg_var2 = c( "sd", "range" ), 
                           arg_unit = "", 
                           arg_label = "", 
                           arg_cvt_stg = I ) { 
      
      # Add match.arg for arg_var1 and arg_var2
                             
      arg1 <- match.fun( arg_var1 )
      arg2 <- match.fun( arg_var2 )
      
      arg_dataset %>%
        rbind( arg_dataset %>% dplyr::mutate( "{{arg_grp}}" :=  "Total" ) ) %>% 
        dplyr::group_by( {{ arg_grp }} ) %>% 
        dplyr::summarise( var1 = round( arg1( {{ arg_var_num }}, na.rm = T ), 1 ),
                   var2 = if( arg_var2 == "sd") as.character( round( arg2( {{ arg_var_num }}, na.rm = T ), 1 ) ) else paste0( round( min( {{ arg_var_num }}, na.rm = T ), 1 ), ", ", round( max( {{ arg_var_num }}, na.rm = T ), 1 ) ), .groups = "drop" ) %>% 
        dplyr::mutate( LABEL = paste( var1, arg_unit, paste0( "(", if( arg_var2 == "range" ) "range:" else str_to_upper( arg_var2 ), " " ,var2, ")" ) ) ) %>% 
        tidyr::pivot_wider( id_cols = -c( dplyr::starts_with( "var" ) ),
                     names_from = {{ arg_grp }}, 
                     values_from = LABEL ) %>% 
        
        { if ( !is.null( arg_trt_inc ) ) { 
          
          dplyr::mutate( ., dplyr::across( dplyr::all_of( arg_trt_inc ), ~ paste( stringr::str_to_title( dplyr::cur_column( ) ), "the", arg_var1, "was", .x ) ) ) %>%
            tidyr::unite( "TRT", dplyr::all_of( arg_trt_inc ), remove = T, sep = ", for " )
          
        } else { . } } %>% 
        
        dplyr::mutate( ., 
                VAR = rlang::as_name( rlang::enquo( arg_var_num ) ), 
                LABEL = paste0( arg_cvt_stg( "For " ), arg_label, " the overall ", arg_var1 , " was ", Total, 
                                if( !is.null( arg_trt_inc ) ) paste0( arg_cvt_stg( ", for " ), TRT ) else "" ),
                .keep = "none" ) %>% 
        
        dplyr::pull( LABEL ) %>% 
        paste( collapse = ", " ) %>% 
        stringr::str_replace( replacement = paste0( "), ", "and" ), pattern = "(\\),)(?!(.|\n)*\\),)" ) %>% 
        stringr::str_squish( )
  
        }
