
#' White Noise
#'
#'
#' @param .distfn What function should be used to draw the innovations from? Must be a string. Defaults to `"rnorm"`, should work with any `r_` compatible function.
#' @param y0 The initial condition
#' @param ... arguments to be passed to `.distfn`
#'
#' @import tibble
#' @import purrr
#' @import dplyr

white_noise <- function(.distfn = "rnorm", y0 = 0, ...) {

  shocks <- eval(call(.distfn, n = dplyr::n(), ...))

  accumulate_dbl(shocks,
                 .init = y0,
                 `+`)

}

