# Notice chp5 and chp6 were a single chapter in the first edition of this book:
# - https://elevanth.org/blog/2018/07/14/statistical-rethinking-edition-2-eta-2020/
#
# TODO: One way you could break up the two chapters is between "Omitted" and "Included" variable
# bias. The second chapter is about including too many variables: Multicollinearity, Post-treatment,
# Collider. The first chapter is about not including enough: Confounders, Masked Relationships.

source("deconfound-spurious-association.R")

## R code 5.28
library(rethinking)
data(milk)
d <- milk
str(d)
display(precis(d))

## R code 5.29
d$K <- standardize(d$kcal.per.g)
d$N <- standardize(d$neocortex.perc)
d$M <- standardize(log(d$mass))

## R code 5.30
# m5.5_draft <- quap(
#     alist(
#         K ~ dnorm( mu , sigma ) ,
#         mu <- a + bN*N ,
#         a ~ dnorm( 0 , 1 ) ,
#         bN ~ dnorm( 0 , 1 ) ,
#         sigma ~ dexp( 1 )
#     ) , data=d )

## R code 5.31
d$neocortex.perc

## R code 5.32
dcc <- d[complete.cases(d$K, d$N, d$M), ]

## R code 5.33
m5.5_draft <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bN * N,
    a ~ dnorm(0, 1),
    bN ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = dcc
)

## R code 5.34
prior <- extract.prior(m5.5_draft)
xseq <- c(-2, 2)
mu <- link(m5.5_draft, post = prior, data = list(N = xseq))
plot(NULL, xlim = xseq, ylim = xseq)
for (i in 1:50) lines(xseq, mu[i, ], col = col.alpha("black", 0.3))

## R code 5.35
m5.5 <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bN * N,
    a ~ dnorm(0, 0.2),
    bN ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = dcc
)

## R code 5.36
display(precis(m5.5))

## R code 5.37
xseq <- seq(from = min(dcc$N) - 0.15, to = max(dcc$N) + 0.15, length.out = 30)
mu <- link(m5.5, data = list(N = xseq))
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)
plot(K ~ N, data = dcc)
lines(xseq, mu_mean, lwd = 2)
shade(mu_PI, xseq)

## R code 5.38
m5.6 <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bM * M,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = dcc
)
display(precis(m5.6))

## R code 5.39
m5.7 <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bN * N + bM * M,
    a ~ dnorm(0, 0.2),
    bN ~ dnorm(0, 0.5),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = dcc
)
display(precis(m5.7))

iplot(function() pairs(~ K + M + N, dcc))

## R code 5.40
plot(coeftab(m5.5, m5.6, m5.7), pars = c("bM", "bN"))

## R code 5.41
xseq <- seq(from = min(dcc$M) - 0.15, to = max(dcc$M) + 0.15, length.out = 30)
mu <- link(m5.7, data = data.frame(M = xseq, N = 0))
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)
plot(NULL, xlim = range(dcc$M), ylim = range(dcc$K))
lines(xseq, mu_mean, lwd = 2)
shade(mu_PI, xseq)

## R code 5.42
# M -> K <- N
# M -> N
n <- 100
M <- rnorm(n)
N <- rnorm(n, M)
K <- rnorm(n, N - M)
d_sim <- data.frame(K = K, N = N, M = M)

## R code 5.43
# M -> K <- N
# N -> M
n <- 100
N <- rnorm(n)
M <- rnorm(n, N)
K <- rnorm(n, N - M)
d_sim2 <- data.frame(K = K, N = N, M = M)

# M -> K <- N
# M <- U -> N
n <- 100
U <- rnorm(n)
N <- rnorm(n, U)
M <- rnorm(n, U)
K <- rnorm(n, N - M)
d_sim3 <- data.frame(K = K, N = N, M = M)

## R code 5.44
dag5.7 <- dagitty("dag{
    M -> K <- N
    M -> N }")
coordinates(dag5.7) <- list(x = c(M = 0, K = 1, N = 2), y = c(M = 0.5, K = 1, N = 0.5))
MElist <- equivalentDAGs(dag5.7)

## R code 5.45
data(Howell1)
d <- Howell1
str(d)

## R code 5.46
mu_female <- rnorm(1e4, 178, 20)
mu_male <- rnorm(1e4, 178, 20) + rnorm(1e4, 0, 10)
precis(data.frame(mu_female, mu_male))

## R code 5.47
d$sex <- ifelse(d$male == 1, 2, 1)
str(d$sex)

## R code 5.48
m5.8 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a[sex],
    a[sex] ~ dnorm(178, 20),
    sigma ~ dunif(0, 50)
  ),
  data = d
)
precis(m5.8, depth = 2)

## R code 5.49
post <- extract.samples(m5.8)
post$diff_fm <- post$a[, 1] - post$a[, 2]
precis(post, depth = 2)

## R code 5.50
data(milk)
d <- milk
levels(d$clade)

## R code 5.51
d$clade_id <- as.integer(d$clade)

## R code 5.52
d$K <- standardize(d$kcal.per.g)
m5.9 <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a[clade_id],
    a[clade_id] ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)
labels <- paste("a[", 1:4, "]:", levels(d$clade), sep = "")
plot(precis(m5.9, depth = 2, pars = "a"),
  labels = labels,
  xlab = "expected kcal (std)"
)

## R code 5.53
set.seed(63)
d$house <- sample(rep(1:4, each = 8), size = nrow(d))

## R code 5.54
m5.10 <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a[clade_id] + h[house],
    a[clade_id] ~ dnorm(0, 0.5),
    h[house] ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)

## R code 6.1
set.seed(1914)
N <- 200 # num grant proposals
p <- 0.1 # proportion to select
# uncorrelated newsworthiness and trustworthiness
nw <- rnorm(N)
tw <- rnorm(N)
# select top 10% of combined scores
s <- nw + tw # total score
q <- quantile(s, 1 - p) # top 10% threshold
selected <- ifelse(s >= q, TRUE, FALSE)
cor(tw[selected], nw[selected])

source('simulate-height-leg-lengths.R')

## R code 6.3
m6.1 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bl * leg_left + br * leg_right,
    a ~ dnorm(10, 100),
    bl ~ dnorm(2, 10),
    br ~ dnorm(2, 10),
    sigma ~ dexp(1)
  ),
  data = d
)
precis(m6.1)

## R code 6.4
plot(precis(m6.1))

## R code 6.5
post <- extract.samples(m6.1)
plot(bl ~ br, post, col = col.alpha(rangi2, 0.1), pch = 16)

## R code 6.6
sum_blbr <- post$bl + post$br
dens(sum_blbr, col = rangi2, lwd = 2, xlab = "sum of bl and br")

## R code 6.7
m6.2 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bl * leg_left,
    a ~ dnorm(10, 100),
    bl ~ dnorm(2, 10),
    sigma ~ dexp(1)
  ),
  data = d
)
precis(m6.2)

## R code 6.8
library(rethinking)
data(milk)
d <- milk
d$K <- standardize(d$kcal.per.g)
d$F <- standardize(d$perc.fat)
d$L <- standardize(d$perc.lactose)

## R code 6.9
# kcal.per.g regressed on perc.fat
m6.3 <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bF * F,
    a ~ dnorm(0, 0.2),
    bF ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)

# kcal.per.g regressed on perc.lactose
m6.4 <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bL * L,
    a ~ dnorm(0, 0.2),
    bL ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)

precis(m6.3)
precis(m6.4)

## R code 6.10
m6.5 <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bF * F + bL * L,
    a ~ dnorm(0, 0.2),
    bF ~ dnorm(0, 0.5),
    bL ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)
precis(m6.5)

## R code 6.11
pairs(~ kcal.per.g + perc.fat + perc.lactose, data = d, col = rangi2)

## R code 6.12
library(rethinking)
data(milk)
d <- milk
sim.coll <- function(r = 0.9) {
  d$x <- rnorm(nrow(d),
    mean = r * d$perc.fat,
    sd = sqrt((1 - r^2) * var(d$perc.fat))
  )
  m <- lm(kcal.per.g ~ perc.fat + x, data = d)
  sqrt(diag(vcov(m)))[2] # stddev of parameter
}
rep.sim.coll <- function(r = 0.9, n = 100) {
  stddev <- replicate(n, sim.coll(r))
  mean(stddev)
}
r.seq <- seq(from = 0, to = 0.99, by = 0.01)
stddev <- sapply(r.seq, function(z) rep.sim.coll(r = z, n = 100))
plot(stddev ~ r.seq, type = "l", col = rangi2, lwd = 2, xlab = "correlation")

source("deconfound-post-treatment-bias.R")

source("deconfound-collider-bias.R")

## R code 6.29
library(dagitty)
dag_6.1 <- dagitty("dag {
    U [unobserved]
    X -> Y
    X <- U <- A -> C -> Y
    U -> B <- C
}")
adjustmentSets(dag_6.1, exposure = "X", outcome = "Y")

## R code 6.30
library(dagitty)
dag_6.2 <- dagitty("dag {
    A -> D
    A -> M -> D
    A <- S -> M
    S -> W -> D
}")
adjustmentSets(dag_6.2, exposure = "W", outcome = "D")

## R code 6.31
impliedConditionalIndependencies(dag_6.2)
