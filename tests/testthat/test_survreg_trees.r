test_that("survreg_trees", {
  library(survival)
  library(survminer)
  
  # For interval2 type.
  # dbh <- c(runif(50)*10+7.5, runif(50)*40+7.5)
  # ba <- (runif(100)+5)*30
  # trees <- data.frame(time1 = rep(1, 50), time2 = rep(8, 50))
  # trees <- rbind(trees, data.frame(time1 = rep(8,50), time2 = NA))
  # trees$alive <- ifelse(is.na(trees$time2), T, F)
  # trees$dbh <- dbh
  # trees$ba <- ba
  # trees$event <- c(rep(3, 50), rep(0,50))
  # obj <- survival::Surv(time = trees$time1, time2 = trees$time2, type = "interval2")
  # rhs <- ~ dbh + ba
  # reg <- survival::survreg(as.formula(paste0("obj~", paste0(attr(terms(rhs),"term.labels"), collapse = "+"))),
  #                          data = trees, control = list(maxiter = 1000),
  #                          dist = "exponential")
  
  trees <- data.frame(dbh = seq(7.5, 100, length=50))
  trees$ba <- runif(nrow(trees))*30 + 5
  trees$alive <- rexp(nrow(trees), trees$dbh/10)>.5
  trees$time2 <- sapply(trees$alive, function(x) ifelse(x, NA, sample(4:9,1)))
  trees$time1 <- sapply(trees$time2, function(x) ifelse(is.na(x), sample(10:12,1), 1))
  trees <- trees[, c("ba", "dbh", "alive", "time1", "time2")]
  
  rhs <- ~ dbh + ba
  rr <- survreg_trees(rhs, trees)
  trees$tdiff <- 10
  pp <- predict_survreg_trees(rr, trees)
  
  # predict_survreg_trees(rr, data.frame(dbh=55, ba=10))
  pct <- seq(0, 1, by = 0.001)
  q <- predict(reg, newdata = data.frame(dbh=c(15,35,55), ba=30), type = "quantile", p = pct, se = F)
  plot(q[1, ],pct, xlim=c(0, 100),type="l", lwd=2)
  points(q[2,],pct,type="l", lwd=2,lty=2)
  points(q[3,],pct,type="l", lwd=2,lty=3)
  
  tdiff <- c(80, 50, 10)
  br <- sapply(1:3, function(i) findInterval(tdiff[i], q[1, ]))
  
  prob <- sapply(1:3, function(i) approx(q[1, ], pct, tdiff[i])$y)
  
  imin <- apply(q, 1, which.min)
  plot(q,pct)
  
  kk <- summary(reg, times = 1:100)
  print(summary(reg))
  plot(predict(reg, newdata = trees,type = "link"))
  kk <- summary(reg)
  
  
  n <- 10000
  tdiff <- rpois(n,lambda = 4) + 1
  p <- 0.6^tdiff
  dat <- data.frame(tdiff = tdiff, y = rbinom(n, 1, p))
  
  utils::data(anorexia, package = "MASS")
  
  anorex.1 <- glm(Postwt ~ Prewt + Treat + offset(Prewt),
                  family = gaussian, data = anorexia)
  
  aa <- glm(Postwt ~ Prewt + Treat + offset(Prewt),
            family = gaussian, data = anorexia)
  bb <- aa
  bb$coefficients <- runif(4)
  zz <- predict(bb, newdata = anorexia)
  
  
  
  # Regression assuming no differences.
  r1 <- glm(y~1, data = dat, family = binomial(link = "logit"))
  x <- r1$model
  
  print(1/(1+exp(-coef(r1))))
  
  
  r2 <- glm_bin_interval(y~1, dat)
  
  
  
})