#' Find the Closest Previous True Value Index
#'
#' This function returns the index of the closest previous `TRUE` value in a logical vector for each element. If an element is `TRUE`, it returns its own index. If no `TRUE` value is found before a given position, it returns `NA`.
#'
#' @param logical_vector A logical vector where `TRUE` and `FALSE` values are used to identify positions of interest. `NA` values are ignored.
#'
#' @return A numeric vector of the same length as `logical_vector`. Each element corresponds to the index of the closest previous `TRUE` value, or `NA` if none exists.
#'
#' @details
#' The function scans through the input logical vector and identifies the closest previous index where a `TRUE` value occurs. If the element itself is `TRUE`, its index is returned.
#'
closest_true_indices <- function(logical_vector) {

  true_indices <- which(!is.na(logical_vector) & logical_vector)

  if (length(true_indices) == 0) {
    return(rep(NA, length(logical_vector)))
  }

  result <- sapply(seq_along(logical_vector), function(i) {
    if (!is.na(logical_vector[i]) && logical_vector[i]) {
      return(i)
    } else {
      if (length(true_indices[true_indices < i]) == 0) {
        return(NA)
      } else {
        closest_true_index <- tail(true_indices[true_indices < i], 1)
        return(closest_true_index)
      }
    }
  })

  return(result)
}
