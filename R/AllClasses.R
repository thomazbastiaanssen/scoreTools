LikertScore <- S7::new_class(
    name = "LikertScore",
    package = "scoreTools",
    parent = S7::class_double,
    properties = list(
        n = S7::class_integer,
        range = S7::class_integer
    ),
    constructor = function(x, n, range) {
        if(inherits(x, "scoreTools::LikertScore")) return(x)
        if(any(c(x, n, range) < 0, na.rm = TRUE)) stop("Input must be positive")
        n     <- int_or_stop(n)
        range <- int_or_stop(range)
        x     <- as.numeric(x)
        S7::new_object(x, n = n, range = range)
    },
    validator = function(self) {
        if(min(self, na.rm = TRUE) <= 0) return(
            "Scores cannot be negative."
        )
        if(min(self, na.rm = TRUE) < self@n) return(
            "'x' cannot contain values lower than number of items 'n'."
        )
        if(max(self, na.rm = TRUE) > prod(self@n, self@range)) return(
            "'x' cannot contain values higher than the prod(n, range)."
        )
        invisible(NULL)
    }
)


#' @noRd
#' @description
#' from ?as.integer examples, named `is.wholenumber`
#'
is.whole <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

#' @noRd
#'
int_or_stop <- function(x) {
    if(is.whole(x)) return(as.integer(x))
    stop("'", deparse1(substitute(x)), "' must be an integer or whole number.")
}
