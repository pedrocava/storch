#' Is context data context?
#'
#' @import purrr
#' @import dplyr
#'
#'

is_data_context <- function() {

  calling <- purrr::possibly(dplyr::n, otherwise = NULL)

  if_else(is.null(calling()),
          FALSE,
          TRUE)

}
