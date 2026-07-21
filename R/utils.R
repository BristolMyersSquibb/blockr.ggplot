#' Fold ggplot layers into a `a + b + c` chain
#'
#' Combine a list of language objects into a single left-associative `+`
#' call, matching what a user writes by hand (`ggplot(...) + geom_*() + ...`).
#'
#' @param terms List of language objects (at least one).
#' @return A single language object.
#' @noRd
gg_add <- function(terms) {
  Reduce(function(a, b) call("+", a, b), terms)
}
