#' Title
#'
#' @param rhs 
#' @param dat 
#' @param dist 
#'
#' @return
#' @export
#'
#' @examples
survreg_trees <- function(rhs, dat, dist = "exponential") {
  
  if (!inherits(rhs, "formula")) stop("Input 'rhs' must be the right-hand-side of a formula")

  # Construction of data.frame with interval-censored data. time1 is starting value.
  # dd <- dat["time1"]
  # 
  # # time2 may vary depending on whether or not the tree is alive.
  # dd$time2 <- ifelse(unlist(dat$alive), NA, unlist(dat$time2))

  # Survival object.
  obj <- Surv(time = dat$time1, time2 = dat$time2, type = "interval2")

  # Parametric survival regression model.
  reg_obj <- survreg(as.formula(paste0("obj~", paste0(attr(terms(rhs),"term.labels"), collapse = "+"))),
               data = dat, control = list(maxiter = 1000),
               dist = dist)
  
  return(reg_obj)
}