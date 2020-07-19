#' Is context data context?
#'
#' @import purrr
#' @import dplyr
#'
#'


is_data_context <- function() {

  calling <- possibly(dplyr::n, otherwise = "error")

  if_else(calling == "error",
          FALSE,
          TRUE)

}
