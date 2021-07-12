library(rethinking)

data(WaffleDivorce)
d <- WaffleDivorce

# standardize variables
d$D <- scale( d$Divorce )
d$M <- scale( d$Marriage )
d$A <- scale( d$MedianAgeMarriage )

div_slim <- list(
  D = d$D,
  M = d$M,
  A = d$A
)

## R code 5.3
m5.1 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bA * A,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)

## R code 5.6
m5.2 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM * M,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)

## R code 5.10
m5.3 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM * M + bA * A,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)
