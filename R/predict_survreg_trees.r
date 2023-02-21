#' Title
#'
#' @param obj a \code{survreg} object.
#' @param newdat \code{data.frame} with covariates. One of the variables must be
#' called \code{tdiff} and contain the time interval at which to evaluate survival
#' probability.
#' @param p vector of percentiles. See \code{predict.survreg} for details.
#'
#' @return
#' @export
#'
#' @examples
predict_survreg_trees <- function(obj, newdat, p = NULL) {
  
  # Checks.
  if (!any("tdiff" %in% colnames(newdat))) stop("Input data.frame 'newdat' must contain a 'tdiff' column")
  if (!is.null(p)) {
    if (min(p) != 0 | max(p) != 1) stop("Vector of percentiles 'p' must have min and max equal to 0,1")
  }
  if (is.null(p)) p <- seq(0, 1, by = 0.01)

  # Predictions for percentiles.
  q <- predict(obj, newdata = newdat, type = "quantile", p = p, se = F)
  
  # Linear interpolation to find out survival probabilities.
  prob <- sapply(1:nrow(q), function(i) approx(q[i, ], p, newdat$tdiff[i])$y)

  return(prob)
  
}