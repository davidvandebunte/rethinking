source('iplot.R')
library(rethinking)

display_markdown("
**9H6.** Modify the Metropolis algorithm code from the chapter to write your own simple MCMC
estimator for globe tossing data and model from Chapter 2.

**Answer.** See `R code 2.8`, which provides most of the answer.
")

set.seed(14)
n_samples <- 1000
p <- rep(NA, n_samples)
p[1] <- 0.5
Water <- 6
Land <- 3
for (i in 2:n_samples) {
  p_new <- rnorm(1, p[i - 1], 0.1)
  if (p_new < 0) p_new <- abs(p_new)
  if (p_new > 1) p_new <- 2 - p_new
  q0 <- dbinom(Water, Water + Land, p[i - 1])
  q1 <- dbinom(Water, Water + Land, p_new)
  p[i] <- ifelse(runif(1) < q1 / q0, p_new, p[i - 1])
}

iplot(function() {
  par(mfrow=c(1,2))
  plot(1:100, p[1:100], main="Samples from posterior", xlab="sample", ylab="p")
  dens(p, xlim = c(0, 1))
  curve(dbeta(x, Water + 1, Land + 1), lty = 2, add = TRUE)
  title(main="Sampled/analytical posterior")
}, ar=1.8)
