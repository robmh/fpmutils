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
predict_survreg_trees <- function(obj, newdat, x) {
  
  # Checks.
  if (!any("tdiff" %in% colnames(newdat))) stop("Input data.frame 'newdat' must contain a 'tdiff' column")

  # Link.
  pred <- predict(obj, newdata = newdat, type = "link", se = F)
  
  p <- location_scale(obj, pred, x)

  return(p)
  
}