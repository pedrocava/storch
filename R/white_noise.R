
#' @title White Noise
#'
#' 
#' @param .distfn What function should be used to draw the innovations from? 
#' Must be a string. 
#' Defaults to `"rnorm"`, should work with any `r_` compatible function.
#' @param y0 The initial condition
#' @param ... arguments to be passed to `.distfn`
#' @param n sample size, only has to be specified if function is called 
#' outside of a tibble or similar data context
#' 
#' @import tibble
#' @import purrr
#' @import dplyr
#' @import rlang
#' 
#' @export
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

    rlang::abort("n must be a non-negative integer")

  }

  shocks <- eval(call(.distfn, list(n, list(...))))

  purrr::accumulate(shocks, .init = y0, `+`)

}


