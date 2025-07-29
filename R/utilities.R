#' Add brackets and % as optional
#'
#' This function adds bracket around input and optionaly can include percentage.
#'
#' @param arg_bkrt An input to have between brackets.
#' @param arg_pct If TRUE will add %.
#' @return user input in bracket and optionally with a "%".
#' @examples
#' fct_brkt(arg_bkrt = c( "Hello, how are you ?"))
#' fct_brkt(arg_bkrt = 78, arg_pct = TRUE)
#' @noRd
#'
fct_brkt <- function( arg_bkrt, arg_pct = FALSE ){
  if( !arg_pct ) paste0( " (", arg_bkrt, ")" ) else paste0( " (", arg_bkrt, "%)" )
}


#' Concatenate sentences with option to add ., ) or a break
#'
#' This function concatenate sentences and add a dot and add a break for next sentence as standard. Can also include an ).
#'
#' @param ... Inputs to be concatenated.
#' @param arg_dot Add a "." at the end of the sentence
#' @param arg_bkt Add a ")" at the end of the sentence
#' @param arg_brk Add a break at the end of the sentence
#' @return Return a concatenated sentence.
#' @examples
#' pasteF("Hello, how are you ?", "I'am fine and you?")
#' @noRd
#'
pasteF <- function( ..., arg_dot = T, arg_bkt = F, arg_brk = T ){

  if( arg_dot ) dot <- "." else dot <- ""
  if( arg_bkt ) bkt <- ")" else bkt <- ""
  if( arg_brk ) brk <- "<br>" else br <- ""

  paste0( ..., dot, bkt, brk )
}
