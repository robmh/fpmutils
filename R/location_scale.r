location_scale <- function(obj, pred, x) {
  
  p <- switch(obj$dist,
              exponential = pexp(x, rate = exp(-pred)),
              weibull = pweibull(x, shape = 1/obj$scale, scale = exp(pred)),
              logistic = plogis(x, location = 1/obj$scale, scale = exp(pred))
  )

  return(1-p)

}