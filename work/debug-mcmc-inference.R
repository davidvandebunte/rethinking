source('introduce-mcmc-algorithms.R')

display_markdown("## 9.4. Easy HMC: `ulam`")

source('load-slim-rugged.R')

## R code 9.12
m8.3 <- quap(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid] * (rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ),
  data = dd
)

precis(m8.3, depth = 2)
str(dat_slim)

## R code 9.14
m9.1 <- ulam(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid] * (rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ),
  data = dat_slim, chains = 1
)

## R code 9.15
precis(m9.1, depth = 2)

## R code 9.16
m9.1 <- ulam(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid] * (rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ),
  data = dat_slim, chains = 4, cores = 4
)

## R code 9.17
show(m9.1)

## R code 9.18
precis(m9.1, 2)

## R code 9.19
pairs(m9.1)

## R code 9.20
traceplot(m9.1)

## R code 9.21
trankplot(m9.1)

## R code 9.22
y <- c(-1, 1)
set.seed(11)
m9.2 <- ulam(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- alpha,
    alpha ~ dnorm(0, 1000),
    sigma ~ dexp(0.0001)
  ),
  data = list(y = y), chains = 3
)

## R code 9.23
precis(m9.2)

## R code 9.24
set.seed(11)
m9.3 <- ulam(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- alpha,
    alpha ~ dnorm(1, 10),
    sigma ~ dexp(1)
  ),
  data = list(y = y), chains = 3
)
precis(m9.3)

## R code 9.25
set.seed(41)
y <- rnorm(100, mean = 0, sd = 1)

## R code 9.26
set.seed(384)
m9.4 <- ulam(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a1 + a2,
    a1 ~ dnorm(0, 1000),
    a2 ~ dnorm(0, 1000),
    sigma ~ dexp(1)
  ),
  data = list(y = y), chains = 3
)
precis(m9.4)

## R code 9.27
m9.5 <- ulam(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a1 + a2,
    a1 ~ dnorm(0, 10),
    a2 ~ dnorm(0, 10),
    sigma ~ dexp(1)
  ),
  data = list(y = y), chains = 3
)
precis(m9.5)
