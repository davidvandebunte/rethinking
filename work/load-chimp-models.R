## R code 11.1
library(rethinking)
data(chimpanzees)
d <- chimpanzees

## R code 11.2
d$treatment <- 1 + d$prosoc_left + 2 * d$condition

## R code 11.4
m11.1 <- quap(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a,
    a ~ dnorm(0, 10)
  ),
  data = d
)

## R code 11.7
m11.2 <- quap(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a + b[treatment],
    a ~ dnorm(0, 1.5),
    b[treatment] ~ dnorm(0, 10)
  ),
  data = d
)

## R code 11.9
m11.3 <- quap(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a + b[treatment],
    a ~ dnorm(0, 1.5),
    b[treatment] ~ dnorm(0, 0.5)
  ),
  data = d
)

## R code 11.10
# trimmed data list
dat_list <- list(
  pulled_left = d$pulled_left,
  actor = d$actor,
  treatment = as.integer(d$treatment)
)

## R code 11.11
m11.4 <- ulam(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + b[treatment],
    a[actor] ~ dnorm(0, 1.5),
    b[treatment] ~ dnorm(0, 0.5)
  ),
  data = dat_list, chains = 4, log_lik = TRUE
)

m11.4_quap <- quap(
  m11.4@formula,
  data = dat_list
)
