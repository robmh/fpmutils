test_that("glm_bin_interval", {
  
  n <- 10000
  tdiff <- rpois(n,lambda = 4) + 1
  p <- 0.6^tdiff
  dat <- data.frame(tdiff = tdiff, y = rbinom(n, 1, p))
  
  # Regression assuming no differences.
  r <- glm(y~1, data = dat, family = binomial(link = "logit"))
  
  
})