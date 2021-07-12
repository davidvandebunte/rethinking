source('iplot.R')

## R code 11.36
library(rethinking)
data(Kline)
d <- Kline

## R code 11.37
d$P <- scale(log(d$population))
d$contact_id <- ifelse(d$contact == "high", 2, 1)

## R code 11.45
dat <- list(
  T = d$total_tools,
  P = d$P,
  cid = d$contact_id
)

# interaction model
m11.10 <- ulam(
  alist(
    T ~ dpois(lambda),
    log(lambda) <- a[cid] + b[cid] * P,
    a[cid] ~ dnorm(3, 0.5),
    b[cid] ~ dnorm(0, 0.2)
  ),
  data = dat, chains = 4, log_lik = TRUE
)
flush.console()

k <- PSIS(m11.10, pointwise = TRUE)$k

iplot(function() {
  par(mfrow=c(1,2))
  ## R code 11.47
  plot(dat$P, dat$T,
    xlab = "log population (std)", ylab = "total tools",
    col = rangi2, pch = ifelse(dat$cid == 1, 1, 16), lwd = 2,
    ylim = c(0, 75), cex = 1 + normalize(k)
  )
  
  # set up the horizontal axis values to compute predictions at
  ns <- 100
  P_seq <- seq(from = -1.4, to = 3, length.out = ns)
  
  # predictions for cid=1 (low contact)
  lambda <- link(m11.10, data = data.frame(P = P_seq, cid = 1))
  lmu <- apply(lambda, 2, mean)
  lci <- apply(lambda, 2, PI)
  lines(P_seq, lmu, lty = 2, lwd = 1.5)
  shade(lci, P_seq, xpd = TRUE)
  
  # predictions for cid=2 (high contact)
  lambda <- link(m11.10, data = data.frame(P = P_seq, cid = 2))
  lmu <- apply(lambda, 2, mean)
  lci <- apply(lambda, 2, PI)
  lines(P_seq, lmu, lty = 1, lwd = 1.5)
  shade(lci, P_seq, xpd = TRUE)

  ## R code 11.48
  plot(d$population, d$total_tools,
    xlab = "population", ylab = "total tools",
    col = rangi2, pch = ifelse(dat$cid == 1, 1, 16), lwd = 2,
    ylim = c(0, 75), cex = 1 + normalize(k)
  )
  
  ns <- 100
  P_seq <- seq(from = -5, to = 3, length.out = ns)
  # 1.53 is sd of log(population)
  # 9 is mean of log(population)
  pop_seq <- exp(P_seq * 1.53 + 9)
  
  lambda <- link(m11.10, data = data.frame(P = P_seq, cid = 1))
  lmu <- apply(lambda, 2, mean)
  lci <- apply(lambda, 2, PI)
  lines(pop_seq, lmu, lty = 2, lwd = 1.5)
  shade(lci, pop_seq, xpd = TRUE)
  
  lambda <- link(m11.10, data = data.frame(P = P_seq, cid = 2))
  lmu <- apply(lambda, 2, mean)
  lci <- apply(lambda, 2, PI)
  lines(pop_seq, lmu, lty = 1, lwd = 1.5)
  shade(lci, pop_seq, xpd = TRUE)
}, ar=1.8)
