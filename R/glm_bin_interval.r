#' Title
#'
#' @param formula 
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
glm_bin_interval <- function(formula = formula, data = dat) {
  
  # First guess.
  r <- glm(formula, family = binomial(link = "logit"), data = dat)
  
  # Functions needed below.
  z <- function(par, dat) #############
  p <- function(x) 1/(1+exp(-x))
  loglik <- function(par, dat, mx) sum(y * log(p^(dat$tdiff/mx)) + (1-y) * log(1-p^(dat$tdiff/mx)))

  # Mean time difference.
  mean_tdiff <- mean(dat$tdiff)
  
  # Maximization of log-likelihood.
  
  
  
  
}