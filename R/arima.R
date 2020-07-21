#'
#' @title ARIMA processes
#' 
#' @param .distfn What function should be used to draw the innovations from? 
#' Must be a string. 
#' Defaults to `"rnorm"`, should work with any `r_` compatible function.
#' @param y0 The initial condition
#' @param ... arguments to be passed to `.distfn`
#' @param n sample size, only has to be specified if function is called 
#' outside of a tibble or similar data context
#' @param P If an integer, then adds lags from 1 to P, if a numeric vector,
#'  then only given lags.
#' @param D If an integer, then adds lags from 1 to P, if a numeric vector,
#'  then only given lags.
#' @param Q If an integer, then adds lags from 1 to P, if a numeric vector,
#'  then only given lags.
#' @param beta autoregressive parameters
#' @param theta moving average parameters
#' 
#' @import tibble
#' @import dplyr 
#' @import purrr 
#' @import tidyr
#' @import tidyselect
#' 
#' @export  
#' 


arima_process <- function(
    .distfn = "rnorm",
    n = NULL,
    y0 = 0,
    P = 1,
    D = 0,
    Q = 0,
    beta = .5,
    theta = .5,
    ...) {

if(is_data_context() == TRUE) {

    n <- dplyr::n()

  } else if(is.null(n)) {

    rlang::abort("n must be a non-negative integer")

  }

if(length(P) == 1) P <- 1:P
if(length(D) == 1) D <- 1:D
if(length(Q) == 1) Q <- 1:Q

if(length(beta) == 1) beta <- rep(beta, times = length(P))
if(length(theta) == 1) theta <- rep(theta, times = length(Q))

shocks <- eval(call(.distfn, list(n, list(...))))

tib2list <- function(.data) {

    num <- nrow(.data)

    purrr::map(1:num, ~ dplyr::slice(.data, .x))

}

tibble::tibble(eps = shocks) %>%
 dplyr::mutate(purrr::map2_dfc(Q, theta, ~ dplyr::lag(eps*.y, .x))) %>%
 tib2list() %>%
 purrr::map_dfr(~ mutate(.x, parcial = sum(.[1:ncol(.)], na.rm = TRUE))) %>%
 dplyr::mutate(map2_dfc(P, beta, ~ dplyr::lag(parcial*.y, .x)),
               map_dfc(D ~ dplyr::lag(parcial, .x))) %>%
 dplyr::select(-parcial) %>%
 tib2list() %>%
 purrr::map_dbl(~ tidyr::pivot_longer(.x, tidyselect::everything()) %>%
        dplyr::summarise(val = sum(value, na.rm = TRUE)) %>%
        dplyr::pull(val)) 



}
