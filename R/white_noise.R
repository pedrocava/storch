
#' White Noise
#'
#'
#' @param .distfn What function should be used to draw the innovations from? Must be a string. 
#' Defaults to `"rnorm"`, should work with any `r_` compatible function.
#' @param y0 The initial condition
#' @param ... arguments to be passed to `.distfn`
#'
#' @import tibble
#' @import purrr
#' @import dplyr
#' 
#' @examples 
#' 
#' white_noise(n = 10, sd = 2)
#' 
#' white_noise("runif", n = 10, max = 2)
#' 
#' 


white_noise <- function(.distfn = "rnorm", n = NULL, y0 = 0, ...) {

  if(is_data_context() == TRUE) {

    n <- dplyr::n()

  } else if(is.null(n)) {

    abort("n must be a non-negative integer")

  }

  shocks <- eval(call(.distfn, list(n, ...)))

  accumulate(shocks,
             .init = y0,
             `+`)

}
