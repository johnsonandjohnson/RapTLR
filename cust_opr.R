#' Concatenate strings with space separator
#'
#' @description
#' A custom infix operator that concatenates strings using a space as separator.
#' This is a convenient wrapper around \code{paste()} with default separator.
#'
#' @param ... Character vectors or objects to be concatenated. Multiple arguments
#'   are combined with spaces between them.
#'
#' @return A character vector of concatenated strings separated by spaces.
#'
#' @examples
#' # Basic concatenation
#' "Hello" %c% "World"
#' # Returns: "Hello World"
#'
#' # Multiple elements
#' "The" %c% "quick" %c% "brown" %c% "fox"
#' # Returns: "The quick brown fox"
#'
#' # With variables
#' study_name <- "ABC-123"
#' phase <- "Phase 3"
#' study_name %c% phase
#' # Returns: "ABC-123 Phase 3"
#'
#' @export
"%c%" <- function( ... ){ 

  paste( ... )

}

#' Concatenate strings with comma separator
#'
#' @description
#' A custom infix operator that concatenates strings using a comma and space
#' as separator. Useful for creating comma-separated lists in text.
#'
#' @param ... Character vectors or objects to be concatenated. Multiple arguments
#'   are combined with ", " between them.
#'
#' @return A character vector of concatenated strings separated by ", ".
#'
#' @examples
#' # Basic concatenation
#' "apple" %c,% "banana"
#' # Returns: "apple, banana"
#'
#' # Multiple elements
#' "red" %c,% "green" %c,% "blue"
#' # Returns: "red, green, blue"
#'
#' # Creating lists for reports
#' endpoint1 <- "efficacy"
#' endpoint2 <- "safety"
#' endpoint3 <- "tolerability"
#' endpoint1 %c,% endpoint2 %c,% endpoint3
#' # Returns: "efficacy, safety, tolerability"
#'
#' @export
"%c,%" <- function( ... ){ 

  paste( ..., sep = ", " )

}

#' Concatenate strings with 'and' separator
#'
#' @description
#' A custom infix operator that concatenates strings using " and " as separator.
#' Particularly useful for creating natural language lists in reports and documents.
#'
#' @param ... Character vectors or objects to be concatenated. Multiple arguments
#'   are combined with " and " between them.
#'
#' @return A character vector of concatenated strings separated by " and ".
#'
#' @examples
#' # Basic concatenation
#' "efficacy" %c&% "safety"
#' # Returns: "efficacy and safety"
#'
#' # Multiple elements
#' "tables" %c&% "listings" %c&% "figures"
#' # Returns: "tables and listings and figures"
#'
#' # Creating natural language text for TLR
#' arm1 <- "placebo"
#' arm2 <- "treatment"
#' "Comparison between" %c% arm1 %c&% arm2 %c% "arms"
#' # Returns: "Comparison between placebo and treatment arms"
#'
#' # Building report sentences
#' n_safety <- 500
#' n_efficacy <- 480
#' paste0("The study included ", n_safety, " safety") %c&%
#'   paste0(n_efficacy, " efficacy participants")
#' # Returns: "The study included 500 safety and 480 efficacy participants"
#'
#' @export
"%c&%" <- function( ... ){ 

  paste( ..., sep = " and " )

}


