#' Utility functions to transform likert scale values to proportions & log-odds.
#' @param x `Numeric vector` Summed likert value to transform. 
#' @param n `Numeric scalar` How many items were summed in `x`. 
#' @param range `Numeric scalar` Number of possible answers in the likert scale.
#' @importFrom S7 S7_data
#' 
#' @export
#' 
as_prop <- function(x, n, range) {
  x <- LikertScore(x, n, range)
  .likert2prop(S7::S7_data(x), x@n, x@range)
}

#' @inheritParams as_prop
#' @inheritParams logp
#' @importFrom S7 S7_data
#' @export
#' 
as_logOdds <- function(x, n, range, base) {
  x <- LikertScore(x, n, range)
  if(missing(base)) .likert2logOdds(S7::S7_data(x), x@n, x@range)
  .likert2logOdds(S7::S7_data(x), x@n, x@range, base)
}


#' @noRd
.likert2prop <- function(x, n, range) (1L + (x-n) ) / ((n * range) - n + 2L)

#' @noRd
.likert2logOdds <- function(x, n, range, base) {
  p <- .likert2prop(x, n, range)
  logb(p/(1-p), base)
}

