source('iplot.R')

display_markdown("
**11M8.** Revisit the `data(Kline)` islands example. This time drop Hawaii from the sample and refit
the models. What changes do you observe?

**Answer.** The results of inference are significantly different, essentially changing the nature of
the answer. The 'outlier' is relevant to what we conclude for both low and high contact cultures.
")

source('load-basic-islands-model.R')

d_drop_hawaii <- subset(d, culture!='Hawaii')

# For some reason, we need to rescale and log to include the 'scale' attributes in the call to ulam.
d_drop_hawaii$P <- scale(log(d_drop_hawaii$population))
d_drop_hawaii$contact_id <- ifelse(d_drop_hawaii$contact == "high", 2, 1)

dat_drop_hawaii <- list(
  T = d_drop_hawaii$total_tools,
  P = d_drop_hawaii$P,
  cid = d_drop_hawaii$contact_id[1:9]
)

# interaction model
m_drop_hawaii <- ulam(
  alist(
    T ~ dpois(lambda),
    log(lambda) <- a[cid] + b[cid] * P,
    a[cid] ~ dnorm(3, 0.5),
    b[cid] ~ dnorm(0, 0.2)
  ),
  data = dat_drop_hawaii, chains = 4, log_lik = TRUE
)
flush.console()

k <- PSIS(m_drop_hawaii, pointwise = TRUE)$k

iplot(function() {
  par(mfrow=c(1,2))
  ## R code 11.47
  plot(dat_drop_hawaii$P, dat_drop_hawaii$T,
    xlab = "log population (std)", ylab = "total tools",
    col = rangi2, pch = ifelse(dat_drop_hawaii$cid == 1, 1, 16), lwd = 2,
    ylim = c(0, 75), cex = 1 + normalize(k)
  )

  # set up the horizontal axis values to compute predictions at
  ns <- 100
  P_seq <- seq(from = -1.4, to = 3, length.out = ns)

  # predictions for cid=1 (low contact)
  lambda <- link(m_drop_hawaii, data = data.frame(P = P_seq, cid = 1))
  lmu <- apply(lambda, 2, mean)
  lci <- apply(lambda, 2, PI)
  lines(P_seq, lmu, lty = 2, lwd = 1.5)
  shade(lci, P_seq, xpd = TRUE)

  # predictions for cid=2 (high contact)
  lambda <- link(m_drop_hawaii, data = data.frame(P = P_seq, cid = 2))
  lmu <- apply(lambda, 2, mean)
  lci <- apply(lambda, 2, PI)
  lines(P_seq, lmu, lty = 1, lwd = 1.5)
  shade(lci, P_seq, xpd = TRUE)

  ## R code 11.48
  plot(d_drop_hawaii$population, d_drop_hawaii$total_tools,
    xlab = "population", ylab = "total tools",
    col = rangi2, pch = ifelse(dat_drop_hawaii$cid == 1, 1, 16), lwd = 2,
    ylim = c(0, 75), cex = 1 + normalize(k)
  )

  ns <- 100
  P_seq <- seq(from = -5, to = 3, length.out = ns)
  # 1.53 is sd of log(population)
  # 9 is mean of log(population)
  pop_seq <- exp(P_seq * 1.53 + 9)

  lambda <- link(m_drop_hawaii, data = data.frame(P = P_seq, cid = 1))
  lmu <- apply(lambda, 2, mean)
  lci <- apply(lambda, 2, PI)
  lines(pop_seq, lmu, lty = 2, lwd = 1.5)
  shade(lci, pop_seq, xpd = TRUE)

  lambda <- link(m_drop_hawaii, data = data.frame(P = P_seq, cid = 2))
  lmu <- apply(lambda, 2, mean)
  lci <- apply(lambda, 2, PI)
  lines(pop_seq, lmu, lty = 1, lwd = 1.5)
  shade(lci, pop_seq, xpd = TRUE)
}, ar=1.8)
