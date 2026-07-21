#' Check if names need backticks for ggplot operations (vectorized)
#'
#' @param names Character vector of names to check
#' @return Logical vector indicating if backticks are needed
#' @noRd
#' @examples
#' needs_backticks(c("normal_name", "2025 Sales", "Product-Name"))
#' # Returns: c(FALSE, TRUE, TRUE)
needs_backticks <- function(names) {
  # Check which names are non-syntactic
  needs_bt <- make.names(names) != names
  # Empty or NA names don't need backticks (handled separately)
  needs_bt[is.na(names) | names == ""] <- FALSE
  needs_bt
}

#' Wrap names in backticks if needed (vectorized)
#'
#' @param names Character vector of names to potentially wrap
#' @return Character vector with non-syntactic names wrapped in backticks
#' @noRd
#' @examples
#' backtick_if_needed(c("normal_name", "2025 Sales", "Product-Name"))
#' # Returns: c("normal_name", "`2025 Sales`", "`Product-Name`")
backtick_if_needed <- function(names) {
  needs_bt <- needs_backticks(names)
  names[needs_bt] <- sprintf("`%s`", names[needs_bt])
  names
}

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
