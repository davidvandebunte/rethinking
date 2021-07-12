## R code 0.1
print("All models are wrong, but some are useful.")

## R code 0.2
x <- 1:2
x <- x * 10
x <- log(x)
x <- sum(x)
x <- exp(x)
x

## R code 0.3
(log(0.01^200))
(200 * log(0.01))

## R code 0.4
# Load the data:
# car braking distances in feet paired with speeds in km/h
# see ?cars for details
data(cars)

# fit a linear regression of distance on speed
m <- lm(dist ~ speed, data = cars)

# estimated coefficients from the model
coef(m)

# plot residuals against speed
plot(resid(m) ~ speed, data = cars)

## R code 2.1
ways <- c(0, 3, 8, 9, 0)
ways / sum(ways)

## R code 2.2
dbinom(6, size = 9, prob = 0.5)

## R code 2.3
# define grid
p_grid <- seq(from = 0, to = 1, length.out = 20)

# define prior
prior <- rep(1, 20)

# compute likelihood at each value in grid
likelihood <- dbinom(6, size = 9, prob = p_grid)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

## R code 2.4
plot(p_grid, posterior,
  type = "b",
  xlab = "probability of water", ylab = "posterior probability"
)
mtext("20 points")

## R code 2.5
prior <- ifelse(p_grid < 0.5, 0, 1)
prior <- exp(-5 * abs(p_grid - 0.5))

## R code 2.6
library(rethinking)
globe.qa <- quap(
  alist(
    W ~ dbinom(W + L, p), # binomial likelihood
    p ~ dunif(0, 1) # uniform prior
  ),
  data = list(W = 6, L = 3)
)

# display summary of quadratic approximation
precis(globe.qa)

## R code 2.7
# analytical calculation
W <- 6
L <- 3
curve(dbeta(x, W + 1, L + 1), from = 0, to = 1)
# quadratic approximation
curve(dnorm(x, 0.67, 0.16), lty = 2, add = TRUE)

## R code 2.8
n_samples <- 1000
p <- rep(NA, n_samples)
p[1] <- 0.5
W <- 6
L <- 3
for (i in 2:n_samples) {
  p_new <- rnorm(1, p[i - 1], 0.1)
  if (p_new < 0) p_new <- abs(p_new)
  if (p_new > 1) p_new <- 2 - p_new
  q0 <- dbinom(W, W + L, p[i - 1])
  q1 <- dbinom(W, W + L, p_new)
  p[i] <- ifelse(runif(1) < q1 / q0, p_new, p[i - 1])
}

## R code 2.9
dens(p, xlim = c(0, 1))
curve(dbeta(x, W + 1, L + 1), lty = 2, add = TRUE)

## R code 3.1
Pr_Positive_Vampire <- 0.95
Pr_Positive_Mortal <- 0.01
Pr_Vampire <- 0.001
Pr_Positive <- Pr_Positive_Vampire * Pr_Vampire +
  Pr_Positive_Mortal * (1 - Pr_Vampire)
(Pr_Vampire_Positive <- Pr_Positive_Vampire * Pr_Vampire / Pr_Positive)

## R code 3.2
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prob_p <- rep(1, 1000)
prob_data <- dbinom(6, size = 9, prob = p_grid)
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)

## R code 3.3
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)

## R code 3.4
plot(samples)

## R code 3.5
library(rethinking)
dens(samples)

## R code 3.6
# add up posterior probability where p < 0.5
sum(posterior[p_grid < 0.5])

## R code 3.7
sum(samples < 0.5) / 1e4

## R code 3.8
sum(samples > 0.5 & samples < 0.75) / 1e4

## R code 3.9
quantile(samples, 0.8)

## R code 3.10
quantile(samples, c(0.1, 0.9))

## R code 3.11
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(3, size = 3, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample(p_grid, size = 1e4, replace = TRUE, prob = posterior)

## R code 3.12
PI(samples, prob = 0.5)

## R code 3.13
HPDI(samples, prob = 0.5)

## R code 3.14
p_grid[which.max(posterior)]

## R code 3.15
chainmode(samples, adj = 0.01)

## R code 3.16
mean(samples)
median(samples)

## R code 3.17
sum(posterior * abs(0.5 - p_grid))

## R code 3.18
loss <- sapply(p_grid, function(d) sum(posterior * abs(d - p_grid)))

## R code 3.19
p_grid[which.min(loss)]

## R code 3.20
dbinom(0:2, size = 2, prob = 0.7)

## R code 3.21
rbinom(1, size = 2, prob = 0.7)

## R code 3.22
rbinom(10, size = 2, prob = 0.7)

## R code 3.23
dummy_w <- rbinom(1e5, size = 2, prob = 0.7)
table(dummy_w) / 1e5

## R code 3.24
dummy_w <- rbinom(1e5, size = 9, prob = 0.7)
simplehist(dummy_w, xlab = "dummy water count")

## R code 3.25
w <- rbinom(1e4, size = 9, prob = 0.6)

## R code 3.26
w <- rbinom(1e4, size = 9, prob = samples)

## R code 3.27
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, size = 9, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)

## R code 3.28
birth1 <- c(
  1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0,
  0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0,
  1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 0,
  1, 0, 1, 1, 1, 0, 1, 1, 1, 1
)
birth2 <- c(
  0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0,
  1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1,
  0, 0, 0, 1, 1, 1, 0, 0, 0, 0
)

## R code 3.29
library(rethinking)
data(homeworkch3)

## R code 3.30
sum(birth1) + sum(birth2)

## R code 4.1
pos <- replicate(1000, sum(runif(16, -1, 1)))

## R code 4.2
prod(1 + runif(12, 0, 0.1))

## R code 4.3
growth <- replicate(10000, prod(1 + runif(12, 0, 0.1)))
dens(growth, norm.comp = TRUE)

## R code 4.4
big <- replicate(10000, prod(1 + runif(12, 0, 0.5)))
small <- replicate(10000, prod(1 + runif(12, 0, 0.01)))

## R code 4.5
log.big <- replicate(10000, log(prod(1 + runif(12, 0, 0.5))))

## R code 4.6
w <- 6
n <- 9
p_grid <- seq(from = 0, to = 1, length.out = 100)
posterior <- dbinom(w, n, p_grid) * dunif(p_grid, 0, 1)
posterior <- posterior / sum(posterior)

## R code 4.7
library(rethinking)
data(Howell1)
d <- Howell1

## R code 4.8
str(d)

## R code 4.9
precis(d)

## R code 4.10
d$height

## R code 4.11
d2 <- d[d$age >= 18, ]

## R code 4.12
curve(dnorm(x, 178, 20), from = 100, to = 250)

## R code 4.13
curve(dunif(x, 0, 50), from = -10, to = 60)

## R code 4.14
sample_mu <- rnorm(1e4, 178, 20)
sample_sigma <- runif(1e4, 0, 50)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)

## R code 4.15
sample_mu <- rnorm(1e4, 178, 100)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)

## R code 4.16
mu.list <- seq(from = 150, to = 160, length.out = 100)
sigma.list <- seq(from = 7, to = 9, length.out = 100)
post <- expand.grid(mu = mu.list, sigma = sigma.list)
post$LL <- sapply(1:nrow(post), function(i) {
  sum(
    dnorm(d2$height, post$mu[i], post$sigma[i], log = TRUE)
  )
})
post$prod <- post$LL + dnorm(post$mu, 178, 20, TRUE) +
  dunif(post$sigma, 0, 50, TRUE)
post$prob <- exp(post$prod - max(post$prod))

## R code 4.17
contour_xyz(post$mu, post$sigma, post$prob)

## R code 4.18
image_xyz(post$mu, post$sigma, post$prob)

## R code 4.19
sample.rows <- sample(1:nrow(post),
  size = 1e4, replace = TRUE,
  prob = post$prob
)
sample.mu <- post$mu[sample.rows]
sample.sigma <- post$sigma[sample.rows]

## R code 4.20
plot(sample.mu, sample.sigma, cex = 0.5, pch = 16, col = col.alpha(rangi2, 0.1))

## R code 4.21
dens(sample.mu)
dens(sample.sigma)

## R code 4.22
PI(sample.mu)
PI(sample.sigma)

## R code 4.23
d3 <- sample(d2$height, size = 20)

## R code 4.24
mu.list <- seq(from = 150, to = 170, length.out = 200)
sigma.list <- seq(from = 4, to = 20, length.out = 200)
post2 <- expand.grid(mu = mu.list, sigma = sigma.list)
post2$LL <- sapply(1:nrow(post2), function(i) {
  sum(dnorm(d3,
    mean = post2$mu[i], sd = post2$sigma[i],
    log = TRUE
  ))
})
post2$prod <- post2$LL + dnorm(post2$mu, 178, 20, TRUE) +
  dunif(post2$sigma, 0, 50, TRUE)
post2$prob <- exp(post2$prod - max(post2$prod))
sample2.rows <- sample(1:nrow(post2),
  size = 1e4, replace = TRUE,
  prob = post2$prob
)
sample2.mu <- post2$mu[sample2.rows]
sample2.sigma <- post2$sigma[sample2.rows]
plot(sample2.mu, sample2.sigma,
  cex = 0.5,
  col = col.alpha(rangi2, 0.1),
  xlab = "mu", ylab = "sigma", pch = 16
)

## R code 4.25
dens(sample2.sigma, norm.comp = TRUE)

## R code 4.26
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18, ]

## R code 4.27
flist <- alist(
  height ~ dnorm(mu, sigma),
  mu ~ dnorm(178, 20),
  sigma ~ dunif(0, 50)
)

## R code 4.28
m4.1 <- quap(flist, data = d2)

## R code 4.29
precis(m4.1)

## R code 4.30
start <- list(
  mu = mean(d2$height),
  sigma = sd(d2$height)
)
m4.1 <- quap(flist, data = d2, start = start)

## R code 4.31
m4.2 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu ~ dnorm(178, 0.1),
    sigma ~ dunif(0, 50)
  ),
  data = d2
)
precis(m4.2)

## R code 4.32
vcov(m4.1)

## R code 4.33
diag(vcov(m4.1))
cov2cor(vcov(m4.1))

## R code 4.34
library(rethinking)
post <- extract.samples(m4.1, n = 1e4)
head(post)

## R code 4.35
precis(post)

## R code 4.36
library(MASS)
post <- mvrnorm(n = 1e4, mu = coef(m4.1), Sigma = vcov(m4.1))

## R code 4.37
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18, ]
plot(d2$height ~ d2$weight)

## R code 4.38
set.seed(2971)
N <- 100 # 100 lines
a <- rnorm(N, 178, 20)
b <- rnorm(N, 0, 10)

## R code 4.39
plot(NULL,
  xlim = range(d2$weight), ylim = c(-100, 400),
  xlab = "weight", ylab = "height"
)
abline(h = 0, lty = 2)
abline(h = 272, lty = 1, lwd = 0.5)
mtext("b ~ dnorm(0,10)")
xbar <- mean(d2$weight)
for (i in 1:N) {
  curve(a[i] + b[i] * (x - xbar),
    from = min(d2$weight), to = max(d2$weight), add = TRUE,
    col = col.alpha("black", 0.2)
  )
}

## R code 4.40
b <- rlnorm(1e4, 0, 1)
dens(b, xlim = c(0, 5), adj = 0.1)

## R code 4.41
set.seed(2971)
N <- 100 # 100 lines
a <- rnorm(N, 178, 20)
b <- rlnorm(N, 0, 1)

## R code 4.42
# load data again, since it's a long way back
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18, ]

# define the average weight, x-bar
xbar <- mean(d2$weight)

# fit model
m4.3 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * (weight - xbar),
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ),
  data = d2
)

## R code 4.43
m4.3b <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + exp(log_b) * (weight - xbar),
    a ~ dnorm(178, 20),
    log_b ~ dnorm(0, 1),
    sigma ~ dunif(0, 50)
  ),
  data = d2
)

## R code 4.44
precis(m4.3)

## R code 4.45
round(vcov(m4.3), 3)

## R code 4.46
plot(height ~ weight, data = d2, col = rangi2)
post <- extract.samples(m4.3)
a_map <- mean(post$a)
b_map <- mean(post$b)
curve(a_map + b_map * (x - xbar), add = TRUE)

## R code 4.47
post <- extract.samples(m4.3)
post[1:5, ]

## R code 4.48
N <- 10
dN <- d2[1:N, ]
mN <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * (weight - mean(weight)),
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ),
  data = dN
)

## R code 4.49
# extract 20 samples from the posterior
post <- extract.samples(mN, n = 20)

# display raw data and sample size
plot(dN$weight, dN$height,
  xlim = range(d2$weight), ylim = range(d2$height),
  col = rangi2, xlab = "weight", ylab = "height"
)
mtext(concat("N = ", N))

# plot the lines, with transparency
for (i in 1:20) {
  curve(post$a[i] + post$b[i] * (x - mean(dN$weight)),
    col = col.alpha("black", 0.3), add = TRUE
  )
}

## R code 4.50
post <- extract.samples(m4.3)
mu_at_50 <- post$a + post$b * (50 - xbar)

## R code 4.51
dens(mu_at_50, col = rangi2, lwd = 2, xlab = "mu|weight=50")

## R code 4.52
PI(mu_at_50, prob = 0.89)

## R code 4.53
mu <- link(m4.3)
str(mu)

## R code 4.54
# define sequence of weights to compute predictions for
# these values will be on the horizontal axis
weight.seq <- seq(from = 25, to = 70, by = 1)

# use link to compute mu
# for each sample from posterior
# and for each weight in weight.seq
mu <- link(m4.3, data = data.frame(weight = weight.seq))
str(mu)

## R code 4.55
# use type="n" to hide raw data
plot(height ~ weight, d2, type = "n")

# loop over samples and plot each mu value
for (i in 1:100) {
  points(weight.seq, mu[i, ], pch = 16, col = col.alpha(rangi2, 0.1))
}

## R code 4.56
# summarize the distribution of mu
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.89)

## R code 4.57
# plot raw data
# fading out points to make line and interval more visible
plot(height ~ weight, data = d2, col = col.alpha(rangi2, 0.5))

# plot the MAP line, aka the mean mu for each weight
lines(weight.seq, mu.mean)

# plot a shaded region for 89% PI
shade(mu.PI, weight.seq)

## R code 4.58
post <- extract.samples(m4.3)
mu.link <- function(weight) post$a + post$b * (weight - xbar)
weight.seq <- seq(from = 25, to = 70, by = 1)
mu <- sapply(weight.seq, mu.link)
mu.mean <- apply(mu, 2, mean)
mu.CI <- apply(mu, 2, PI, prob = 0.89)

## R code 4.59
sim.height <- sim(m4.3, data = list(weight = weight.seq))
str(sim.height)

## R code 4.60
height.PI <- apply(sim.height, 2, PI, prob = 0.89)

## R code 4.61
# plot raw data
plot(height ~ weight, d2, col = col.alpha(rangi2, 0.5))

# draw MAP line
lines(weight.seq, mu.mean)

# draw HPDI region for line
shade(mu.PI, weight.seq)

# draw PI region for simulated heights
shade(height.PI, weight.seq)

## R code 4.62
sim.height <- sim(m4.3, data = list(weight = weight.seq), n = 1e4)
height.PI <- apply(sim.height, 2, PI, prob = 0.89)

## R code 4.63
post <- extract.samples(m4.3)
weight.seq <- 25:70
sim.height <- sapply(weight.seq, function(weight) {
  rnorm(
    n = nrow(post),
    mean = post$a + post$b * (weight - xbar),
    sd = post$sigma
  )
})
height.PI <- apply(sim.height, 2, PI, prob = 0.89)

## R code 4.64
library(rethinking)
data(Howell1)
d <- Howell1

## R code 4.65
d$weight_s <- (d$weight - mean(d$weight)) / sd(d$weight)
d$weight_s2 <- d$weight_s^2
m4.5 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1 * weight_s + b2 * weight_s2,
    a ~ dnorm(178, 20),
    b1 ~ dlnorm(0, 1),
    b2 ~ dnorm(0, 1),
    sigma ~ dunif(0, 50)
  ),
  data = d
)

## R code 4.66
precis(m4.5)

## R code 4.67
weight.seq <- seq(from = -2.2, to = 2, length.out = 30)
pred_dat <- list(weight_s = weight.seq, weight_s2 = weight.seq^2)
mu <- link(m4.5, data = pred_dat)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.89)
sim.height <- sim(m4.5, data = pred_dat)
height.PI <- apply(sim.height, 2, PI, prob = 0.89)

## R code 4.68
plot(height ~ weight_s, d, col = col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)

## R code 4.69
d$weight_s3 <- d$weight_s^3
m4.6 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1 * weight_s + b2 * weight_s2 + b3 * weight_s3,
    a ~ dnorm(178, 20),
    b1 ~ dlnorm(0, 1),
    b2 ~ dnorm(0, 10),
    b3 ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ),
  data = d
)

## R code 4.70
plot(height ~ weight_s, d, col = col.alpha(rangi2, 0.5), xaxt = "n")

## R code 4.71
at <- c(-2, -1, 0, 1, 2)
labels <- at * sd(d$weight) + mean(d$weight)
axis(side = 1, at = at, labels = round(labels, 1))

## R code 4.72
library(rethinking)
data(cherry_blossoms)
d <- cherry_blossoms
precis(d)

## R code 4.73
d2 <- d[complete.cases(d$doy), ] # complete cases on doy
num_knots <- 15
knot_list <- quantile(d2$year, probs = seq(0, 1, length.out = num_knots))

## R code 4.74
library(splines)
B <- bs(d2$year,
  knots = knot_list[-c(1, num_knots)],
  degree = 3, intercept = TRUE
)

## R code 4.75
plot(NULL, xlim = range(d2$year), ylim = c(0, 1), xlab = "year", ylab = "basis")
for (i in 1:ncol(B)) lines(d2$year, B[, i])

## R code 4.76
m4.7 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + B %*% w,
    a ~ dnorm(100, 10),
    w ~ dnorm(0, 10),
    sigma ~ dexp(1)
  ),
  data = list(D = d2$doy, B = B),
  start = list(w = rep(0, ncol(B)))
)

## R code 4.77
post <- extract.samples(m4.7)
w <- apply(post$w, 2, mean)
plot(NULL,
  xlim = range(d2$year), ylim = c(-6, 6),
  xlab = "year", ylab = "basis * weight"
)
for (i in 1:ncol(B)) lines(d2$year, w[i] * B[, i])

## R code 4.78
mu <- link(m4.7)
mu_PI <- apply(mu, 2, PI, 0.97)
plot(d2$year, d2$doy, col = col.alpha(rangi2, 0.3), pch = 16)
shade(mu_PI, d2$year, col = col.alpha("black", 0.5))

## R code 4.79
m4.7alt <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + sapply(1:827, function(i) sum(B[i, ] * w)),
    a ~ dnorm(100, 1),
    w ~ dnorm(0, 10),
    sigma ~ dexp(1)
  ),
  data = list(D = d2$doy, B = B),
  start = list(w = rep(0, ncol(B)))
)

source("deconfound-model.R")
source("balance-overfitting-underfitting.R")
source("model-interactions.R")
source("debug-mcmc-inference.R")
source("introduce-glms.R")
source("model-counts.R")

## R code 12.1
pbar <- 0.5
theta <- 5
curve(dbeta2(x, pbar, theta),
  from = 0, to = 1,
  xlab = "probability", ylab = "Density"
)

## R code 12.2
library(rethinking)
data(UCBadmit)
d <- UCBadmit
d$gid <- ifelse(d$applicant.gender == "male", 1L, 2L)
dat <- list(A = d$admit, N = d$applications, gid = d$gid)
m12.1 <- ulam(
  alist(
    A ~ dbetabinom(N, pbar, theta),
    logit(pbar) <- a[gid],
    a[gid] ~ dnorm(0, 1.5),
    transpars > theta <<- phi + 2.0,
    phi ~ dexp(1)
  ),
  data = dat, chains = 4
)

## R code 12.3
post <- extract.samples(m12.1)
post$da <- post$a[, 1] - post$a[, 2]
precis(post, depth = 2)

## R code 12.4
gid <- 2
# draw posterior mean beta distribution
curve(dbeta2(x, mean(logistic(post$a[, gid])), mean(post$theta)),
  from = 0, to = 1,
  ylab = "Density", xlab = "probability admit", ylim = c(0, 3), lwd = 2
)

# draw 50 beta distributions sampled from posterior
for (i in 1:50) {
  p <- logistic(post$a[i, gid])
  theta <- post$theta[i]
  curve(dbeta2(x, p, theta), add = TRUE, col = col.alpha("black", 0.2))
}
mtext("distribution of female admission rates")

## R code 12.5
postcheck(m12.1)

## R code 12.6
library(rethinking)
data(Kline)
d <- Kline
d$P <- standardize(log(d$population))
d$contact_id <- ifelse(d$contact == "high", 2L, 1L)

dat2 <- list(
  T = d$total_tools,
  P = d$population,
  cid = d$contact_id
)

m12.2 <- ulam(
  alist(
    T ~ dgampois(lambda, phi),
    lambda <- exp(a[cid]) * P^b[cid] / g,
    a[cid] ~ dnorm(1, 1),
    b[cid] ~ dexp(1),
    g ~ dexp(1),
    phi ~ dexp(1)
  ),
  data = dat2, chains = 4, log_lik = TRUE
)

## R code 12.7
# define parameters
prob_drink <- 0.2 # 20% of days
rate_work <- 1 # average 1 manuscript per day

# sample one year of production
N <- 365

# simulate days monks drink
set.seed(365)
drink <- rbinom(N, 1, prob_drink)

# simulate manuscripts completed
y <- (1 - drink) * rpois(N, rate_work)

## R code 12.8
simplehist(y, xlab = "manuscripts completed", lwd = 4)
zeros_drink <- sum(drink)
zeros_work <- sum(y == 0 & drink == 0)
zeros_total <- sum(y == 0)
lines(c(0, 0), c(zeros_work, zeros_total), lwd = 4, col = rangi2)

## R code 12.9
m12.3 <- ulam(
  alist(
    y ~ dzipois(p, lambda),
    logit(p) <- ap,
    log(lambda) <- al,
    ap ~ dnorm(-1.5, 1),
    al ~ dnorm(1, 0.5)
  ),
  data = list(y = y), chains = 4
)
precis(m12.3)

## R code 12.10
post <- extract.samples(m12.3)
mean(inv_logit(post$ap)) # probability drink
mean(exp(post$al)) # rate finish manuscripts, when not drinking

## R code 12.11
m12.3_alt <- ulam(
  alist(
    y | y > 0 ~ custom(log1m(p) + poisson_lpmf(y | lambda)),
    y | y == 0 ~ custom(log_mix(p, 0, poisson_lpmf(0 | lambda))),
    logit(p) <- ap,
    log(lambda) <- al,
    ap ~ dnorm(-1.5, 1),
    al ~ dnorm(1, 0.5)
  ),
  data = list(y = as.integer(y)), chains = 4
)

## R code 12.12
library(rethinking)
data(Trolley)
d <- Trolley

## R code 12.13
simplehist(d$response, xlim = c(1, 7), xlab = "response")

## R code 12.14
# discrete proportion of each response value
pr_k <- table(d$response) / nrow(d)

# cumsum converts to cumulative proportions
cum_pr_k <- cumsum(pr_k)

# plot
plot(1:7, cum_pr_k,
  type = "b", xlab = "response",
  ylab = "cumulative proportion", ylim = c(0, 1)
)

## R code 12.15
logit <- function(x) log(x / (1 - x)) # convenience function
round(lco <- logit(cum_pr_k), 2)

## R code 12.16
m12.4 <- ulam(
  alist(
    R ~ dordlogit(0, cutpoints),
    cutpoints ~ dnorm(0, 1.5)
  ),
  data = list(R = d$response), chains = 4, cores = 4
)

## R code 12.17
m12.4q <- quap(
  alist(
    response ~ dordlogit(0, c(a1, a2, a3, a4, a5, a6)),
    c(a1, a2, a3, a4, a5, a6) ~ dnorm(0, 1.5)
  ),
  data = d, start = list(a1 = -2, a2 = -1, a3 = 0, a4 = 1, a5 = 2, a6 = 2.5)
)

## R code 12.18
precis(m12.4, depth = 2)

## R code 12.19
round(inv_logit(coef(m12.4)), 3)

## R code 12.20
round(pk <- dordlogit(1:7, 0, coef(m12.4)), 2)

## R code 12.21
sum(pk * (1:7))

## R code 12.22
round(pk <- dordlogit(1:7, 0, coef(m12.4) - 0.5), 2)

## R code 12.23
sum(pk * (1:7))

## R code 12.24
dat <- list(
  R = d$response,
  A = d$action,
  I = d$intention,
  C = d$contact
)
m12.5 <- ulam(
  alist(
    R ~ dordlogit(phi, cutpoints),
    phi <- bA * A + bC * C + BI * I,
    BI <- bI + bIA * A + bIC * C,
    c(bA, bI, bC, bIA, bIC) ~ dnorm(0, 0.5),
    cutpoints ~ dnorm(0, 1.5)
  ),
  data = dat, chains = 4, cores = 4
)
precis(m12.5)

## R code 12.25
plot(precis(m12.5), xlim = c(-1.4, 0))

## R code 12.26
plot(NULL,
  type = "n", xlab = "intention", ylab = "probability",
  xlim = c(0, 1), ylim = c(0, 1), xaxp = c(0, 1, 1), yaxp = c(0, 1, 2)
)

## R code 12.27
kA <- 0 # value for action
kC <- 0 # value for contact
kI <- 0:1 # values of intention to calculate over
pdat <- data.frame(A = kA, C = kC, I = kI)
phi <- link(m12.5, data = pdat)$phi

## R code 12.28
post <- extract.samples(m12.5)
for (s in 1:50) {
  pk <- pordlogit(1:6, phi[s, ], post$cutpoints[s, ])
  for (i in 1:6) lines(kI, pk[, i], col = grau(0.1))
}

## R code 12.29
kA <- 0 # value for action
kC <- 1 # value for contact
kI <- 0:1 # values of intention to calculate over
pdat <- data.frame(A = kA, C = kC, I = kI)
s <- sim(m12.5, data = pdat)
simplehist(s, xlab = "response")

## R code 12.30
library(rethinking)
data(Trolley)
d <- Trolley
levels(d$edu)

## R code 12.31
edu_levels <- c(6, 1, 8, 4, 7, 2, 5, 3)
d$edu_new <- edu_levels[d$edu]

## R code 12.32
library(gtools)
set.seed(1805)
delta <- rdirichlet(10, alpha = rep(2, 7))
str(delta)

## R code 12.33
h <- 3
plot(NULL, xlim = c(1, 7), ylim = c(0, 0.4), xlab = "index", ylab = "probability")
for (i in 1:nrow(delta)) {
  lines(1:7, delta[i, ],
    type = "b",
    pch = ifelse(i == h, 16, 1), lwd = ifelse(i == h, 4, 1.5),
    col = ifelse(i == h, "black", col.alpha("black", 0.7))
  )
}

## R code 12.34
dat <- list(
  R = d$response,
  action = d$action,
  intention = d$intention,
  contact = d$contact,
  E = as.integer(d$edu_new), # edu_new as an index
  alpha = rep(2, 7)
) # delta prior

m12.6 <- ulam(
  alist(
    R ~ ordered_logistic(phi, kappa),
    phi <- bE * sum(delta_j[1:E]) + bA * action + bI * intention + bC * contact,
    kappa ~ normal(0, 1.5),
    c(bA, bI, bC, bE) ~ normal(0, 1),
    vector[8]:delta_j <<- append_row(0, delta),
    simplex[7]:delta ~ dirichlet(alpha)
  ),
  data = dat, chains = 4, cores = 4
)

## R code 12.35
precis(m12.6, depth = 2, omit = "kappa")

## R code 12.36
delta_labels <- c("Elem", "MidSch", "SHS", "HSG", "SCol", "Bach", "Mast", "Grad")
pairs(m12.6, pars = "delta", labels = delta_labels)

## R code 12.37
dat$edu_norm <- normalize(d$edu_new)
m12.7 <- ulam(
  alist(
    R ~ ordered_logistic(mu, cutpoints),
    mu <- bE * edu_norm + bA * action + bI * intention + bC * contact,
    c(bA, bI, bC, bE) ~ normal(0, 1),
    cutpoints ~ normal(0, 1.5)
  ),
  data = dat, chains = 4, cores = 4
)
precis(m12.7)

## R code 12.38
library(rethinking)
data(Hurricanes)

## R code 13.1
library(rethinking)
data(reedfrogs)
d <- reedfrogs
str(d)

## R code 13.2
# make the tank cluster variable
d$tank <- 1:nrow(d)

dat <- list(
  S = d$surv,
  N = d$density,
  tank = d$tank
)

# approximate posterior
m13.1 <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a[tank],
    a[tank] ~ dnorm(0, 1.5)
  ),
  data = dat, chains = 4, log_lik = TRUE
)

## R code 13.3
m13.2 <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a[tank],
    a[tank] ~ dnorm(a_bar, sigma),
    a_bar ~ dnorm(0, 1.5),
    sigma ~ dexp(1)
  ),
  data = dat, chains = 4, log_lik = TRUE
)

## R code 13.4
compare(m13.1, m13.2)

## R code 13.5
# extract Stan samples
post <- extract.samples(m13.2)

# compute mean intercept for each tank
# also transform to probability with logistic
d$propsurv.est <- logistic(apply(post$a, 2, mean))

# display raw proportions surviving in each tank
plot(d$propsurv,
  ylim = c(0, 1), pch = 16, xaxt = "n",
  xlab = "tank", ylab = "proportion survival", col = rangi2
)
axis(1, at = c(1, 16, 32, 48), labels = c(1, 16, 32, 48))

# overlay posterior means
points(d$propsurv.est)

# mark posterior mean probability across tanks
abline(h = mean(inv_logit(post$a_bar)), lty = 2)

# draw vertical dividers between tank densities
abline(v = 16.5, lwd = 0.5)
abline(v = 32.5, lwd = 0.5)
text(8, 0, "small tanks")
text(16 + 8, 0, "medium tanks")
text(32 + 8, 0, "large tanks")

## R code 13.6
# show first 100 populations in the posterior
plot(NULL,
  xlim = c(-3, 4), ylim = c(0, 0.35),
  xlab = "log-odds survive", ylab = "Density"
)
for (i in 1:100) {
  curve(dnorm(x, post$a_bar[i], post$sigma[i]),
    add = TRUE,
    col = col.alpha("black", 0.2)
  )
}

# sample 8000 imaginary tanks from the posterior distribution
sim_tanks <- rnorm(8000, post$a_bar, post$sigma)

# transform to probability and visualize
dens(inv_logit(sim_tanks), lwd = 2, adj = 0.1)

## R code 13.7
a_bar <- 1.5
sigma <- 1.5
nponds <- 60
Ni <- as.integer(rep(c(5, 10, 25, 35), each = 15))

## R code 13.8
set.seed(5005)
a_pond <- rnorm(nponds, mean = a_bar, sd = sigma)

## R code 13.9
dsim <- data.frame(pond = 1:nponds, Ni = Ni, true_a = a_pond)

## R code 13.10
class(1:3)
class(c(1, 2, 3))

## R code 13.11
dsim$Si <- rbinom(nponds, prob = logistic(dsim$true_a), size = dsim$Ni)

## R code 13.12
dsim$p_nopool <- dsim$Si / dsim$Ni

## R code 13.13
dat <- list(Si = dsim$Si, Ni = dsim$Ni, pond = dsim$pond)
m13.3 <- ulam(
  alist(
    Si ~ dbinom(Ni, p),
    logit(p) <- a_pond[pond],
    a_pond[pond] ~ dnorm(a_bar, sigma),
    a_bar ~ dnorm(0, 1.5),
    sigma ~ dexp(1)
  ),
  data = dat, chains = 4
)

## R code 13.14
precis(m13.3, depth = 2)

## R code 13.15
post <- extract.samples(m13.3)
dsim$p_partpool <- apply(inv_logit(post$a_pond), 2, mean)

## R code 13.16
dsim$p_true <- inv_logit(dsim$true_a)

## R code 13.17
nopool_error <- abs(dsim$p_nopool - dsim$p_true)
partpool_error <- abs(dsim$p_partpool - dsim$p_true)

## R code 13.18
plot(1:60, nopool_error,
  xlab = "pond", ylab = "absolute error",
  col = rangi2, pch = 16
)
points(1:60, partpool_error)

## R code 13.19
nopool_avg <- aggregate(nopool_error, list(dsim$Ni), mean)
partpool_avg <- aggregate(partpool_error, list(dsim$Ni), mean)

## R code 13.20
a <- 1.5
sigma <- 1.5
nponds <- 60
Ni <- as.integer(rep(c(5, 10, 25, 35), each = 15))
a_pond <- rnorm(nponds, mean = a, sd = sigma)
dsim <- data.frame(pond = 1:nponds, Ni = Ni, true_a = a_pond)
dsim$Si <- rbinom(nponds, prob = inv_logit(dsim$true_a), size = dsim$Ni)
dsim$p_nopool <- dsim$Si / dsim$Ni
newdat <- list(Si = dsim$Si, Ni = dsim$Ni, pond = 1:nponds)
m13.3new <- stan(fit = m13.3@stanfit, data = newdat, chains = 4)

post <- extract.samples(m13.3new)
dsim$p_partpool <- apply(inv_logit(post$a_pond), 2, mean)
dsim$p_true <- inv_logit(dsim$true_a)
nopool_error <- abs(dsim$p_nopool - dsim$p_true)
partpool_error <- abs(dsim$p_partpool - dsim$p_true)
plot(1:60, nopool_error, xlab = "pond", ylab = "absolute error", col = rangi2, pch = 16)
points(1:60, partpool_error)

## R code 13.21
library(rethinking)
data(chimpanzees)
d <- chimpanzees
d$treatment <- 1 + d$prosoc_left + 2 * d$condition

dat_list <- list(
  pulled_left = d$pulled_left,
  actor = d$actor,
  block_id = d$block,
  treatment = as.integer(d$treatment)
)

set.seed(13)
m13.4 <- ulam(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + g[block_id] + b[treatment],
    b[treatment] ~ dnorm(0, 0.5),
    ## adaptive priors
    a[actor] ~ dnorm(a_bar, sigma_a),
    g[block_id] ~ dnorm(0, sigma_g),
    ## hyper-priors
    a_bar ~ dnorm(0, 1.5),
    sigma_a ~ dexp(1),
    sigma_g ~ dexp(1)
  ),
  data = dat_list, chains = 4, cores = 4, log_lik = TRUE
)

## R code 13.22
precis(m13.4, depth = 2)
plot(precis(m13.4, depth = 2)) # also plot

## R code 13.23
set.seed(14)
m13.5 <- ulam(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + b[treatment],
    b[treatment] ~ dnorm(0, 0.5),
    a[actor] ~ dnorm(a_bar, sigma_a),
    a_bar ~ dnorm(0, 1.5),
    sigma_a ~ dexp(1)
  ),
  data = dat_list, chains = 4, cores = 4, log_lik = TRUE
)

## R code 13.24
compare(m13.4, m13.5)

## R code 13.25
set.seed(15)
m13.6 <- ulam(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + g[block_id] + b[treatment],
    b[treatment] ~ dnorm(0, sigma_b),
    a[actor] ~ dnorm(a_bar, sigma_a),
    g[block_id] ~ dnorm(0, sigma_g),
    a_bar ~ dnorm(0, 1.5),
    sigma_a ~ dexp(1),
    sigma_g ~ dexp(1),
    sigma_b ~ dexp(1)
  ),
  data = dat_list, chains = 4, cores = 4, log_lik = TRUE
)
coeftab(m13.4, m13.6)

## R code 13.26
m13.7 <- ulam(
  alist(
    v ~ normal(0, 3),
    x ~ normal(0, exp(v))
  ),
  data = list(N = 1), chains = 4
)
precis(m13.7)

## R code 13.27
m13.7nc <- ulam(
  alist(
    v ~ normal(0, 3),
    z ~ normal(0, 1),
    gq > real[1]:x <<- z * exp(v)
  ),
  data = list(N = 1), chains = 4
)
precis(m13.7nc)

## R code 13.28
set.seed(13)
m13.4b <- ulam(m13.4, chains = 4, cores = 4, control = list(adapt_delta = 0.99))
divergent(m13.4b)

## R code 13.29
set.seed(13)
m13.4nc <- ulam(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a_bar + z[actor] * sigma_a + # actor intercepts
      x[block_id] * sigma_g + # block intercepts
      b[treatment],
    b[treatment] ~ dnorm(0, 0.5),
    z[actor] ~ dnorm(0, 1),
    x[block_id] ~ dnorm(0, 1),
    a_bar ~ dnorm(0, 1.5),
    sigma_a ~ dexp(1),
    sigma_g ~ dexp(1),
    gq > vector[actor]:a <<- a_bar + z * sigma_a,
    gq > vector[block_id]:g <<- x * sigma_g
  ),
  data = dat_list, chains = 4, cores = 4
)

## R code 13.30
precis_c <- precis(m13.4, depth = 2)
precis_nc <- precis(m13.4nc, depth = 2)
pars <- c(
  paste("a[", 1:7, "]", sep = ""), paste("g[", 1:6, "]", sep = ""),
  paste("b[", 1:4, "]", sep = ""), "a_bar", "sigma_a", "sigma_g"
)
neff_table <- cbind(precis_c[pars, "n_eff"], precis_nc[pars, "n_eff"])
plot(neff_table,
  xlim = range(neff_table), ylim = range(neff_table),
  xlab = "n_eff (centered)", ylab = "n_eff (non-centered)", lwd = 2
)
abline(a = 0, b = 1, lty = 2)

## R code 13.31
chimp <- 2
d_pred <- list(
  actor = rep(chimp, 4),
  treatment = 1:4,
  block_id = rep(1, 4)
)
p <- link(m13.4, data = d_pred)
p_mu <- apply(p, 2, mean)
p_ci <- apply(p, 2, PI)

## R code 13.32
post <- extract.samples(m13.4)
str(post)

## R code 13.33
dens(post$a[, 5])

## R code 13.34
p_link <- function(treatment, actor = 1, block_id = 1) {
  logodds <- with(
    post,
    a[, actor] + g[, block_id] + b[, treatment]
  )
  return(inv_logit(logodds))
}

## R code 13.35
p_raw <- sapply(1:4, function(i) p_link(i, actor = 2, block_id = 1))
p_mu <- apply(p_raw, 2, mean)
p_ci <- apply(p_raw, 2, PI)

## R code 13.36
p_link_abar <- function(treatment) {
  logodds <- with(post, a_bar + b[, treatment])
  return(inv_logit(logodds))
}

## R code 13.37
post <- extract.samples(m13.4)
p_raw <- sapply(1:4, function(i) p_link_abar(i))
p_mu <- apply(p_raw, 2, mean)
p_ci <- apply(p_raw, 2, PI)

plot(NULL,
  xlab = "treatment", ylab = "proportion pulled left",
  ylim = c(0, 1), xaxt = "n", xlim = c(1, 4)
)
axis(1, at = 1:4, labels = c("R/N", "L/N", "R/P", "L/P"))
lines(1:4, p_mu)
shade(p_ci, 1:4)

## R code 13.38
a_sim <- with(post, rnorm(length(post$a_bar), a_bar, sigma_a))
p_link_asim <- function(treatment) {
  logodds <- with(post, a_sim + b[, treatment])
  return(inv_logit(logodds))
}
p_raw_asim <- sapply(1:4, function(i) p_link_asim(i))

## R code 13.39
plot(NULL,
  xlab = "treatment", ylab = "proportion pulled left",
  ylim = c(0, 1), xaxt = "n", xlim = c(1, 4)
)
axis(1, at = 1:4, labels = c("R/N", "L/N", "R/P", "L/P"))
for (i in 1:100) lines(1:4, p_raw_asim[i, ], col = grau(0.25), lwd = 2)

## R code 13.40
## R code 13.41
## R code 14.1
a <- 3.5 # average morning wait time
b <- (-1) # average difference afternoon wait time
sigma_a <- 1 # std dev in intercepts
sigma_b <- 0.5 # std dev in slopes
rho <- (-0.7) # correlation between intercepts and slopes

## R code 14.2
Mu <- c(a, b)

## R code 14.3
cov_ab <- sigma_a * sigma_b * rho
Sigma <- matrix(c(sigma_a^2, cov_ab, cov_ab, sigma_b^2), ncol = 2)

## R code 14.4
matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)

## R code 14.5
sigmas <- c(sigma_a, sigma_b) # standard deviations
Rho <- matrix(c(1, rho, rho, 1), nrow = 2) # correlation matrix

# now matrix multiply to get covariance matrix
Sigma <- diag(sigmas) %*% Rho %*% diag(sigmas)

## R code 14.6
N_cafes <- 20

## R code 14.7
library(MASS)
set.seed(5) # used to replicate example
vary_effects <- mvrnorm(N_cafes, Mu, Sigma)

## R code 14.8
a_cafe <- vary_effects[, 1]
b_cafe <- vary_effects[, 2]

## R code 14.9
plot(a_cafe, b_cafe,
  col = rangi2,
  xlab = "intercepts (a_cafe)", ylab = "slopes (b_cafe)"
)

# overlay population distribution
library(ellipse)
for (l in c(0.1, 0.3, 0.5, 0.8, 0.99)) {
  lines(ellipse(Sigma, centre = Mu, level = l), col = col.alpha("black", 0.2))
}

## R code 14.10
set.seed(22)
N_visits <- 10
afternoon <- rep(0:1, N_visits * N_cafes / 2)
cafe_id <- rep(1:N_cafes, each = N_visits)
mu <- a_cafe[cafe_id] + b_cafe[cafe_id] * afternoon
sigma <- 0.5 # std dev within cafes
wait <- rnorm(N_visits * N_cafes, mu, sigma)
d <- data.frame(cafe = cafe_id, afternoon = afternoon, wait = wait)

## R code 14.11
R <- rlkjcorr(1e4, K = 2, eta = 2)
dens(R[, 1, 2], xlab = "correlation")

## R code 14.12
set.seed(867530)
m14.1 <- ulam(
  alist(
    wait ~ normal(mu, sigma),
    mu <- a_cafe[cafe] + b_cafe[cafe] * afternoon,
    c(a_cafe, b_cafe)[cafe] ~ multi_normal(c(a, b), Rho, sigma_cafe),
    a ~ normal(5, 2),
    b ~ normal(-1, 0.5),
    sigma_cafe ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ),
  data = d, chains = 4, cores = 4
)

## R code 14.13
post <- extract.samples(m14.1)
dens(post$Rho[, 1, 2], xlim = c(-1, 1)) # posterior
R <- rlkjcorr(1e4, K = 2, eta = 2) # prior
dens(R[, 1, 2], add = TRUE, lty = 2)

## R code 14.14
# compute unpooled estimates directly from data
a1 <- sapply(
  1:N_cafes,
  function(i) mean(wait[cafe_id == i & afternoon == 0])
)
b1 <- sapply(
  1:N_cafes,
  function(i) mean(wait[cafe_id == i & afternoon == 1])
) - a1

# extract posterior means of partially pooled estimates
post <- extract.samples(m14.1)
a2 <- apply(post$a_cafe, 2, mean)
b2 <- apply(post$b_cafe, 2, mean)

# plot both and connect with lines
plot(a1, b1,
  xlab = "intercept", ylab = "slope",
  pch = 16, col = rangi2, ylim = c(min(b1) - 0.1, max(b1) + 0.1),
  xlim = c(min(a1) - 0.1, max(a1) + 0.1)
)
points(a2, b2, pch = 1)
for (i in 1:N_cafes) lines(c(a1[i], a2[i]), c(b1[i], b2[i]))

## R code 14.15
# compute posterior mean bivariate Gaussian
Mu_est <- c(mean(post$a), mean(post$b))
rho_est <- mean(post$Rho[, 1, 2])
sa_est <- mean(post$sigma_cafe[, 1])
sb_est <- mean(post$sigma_cafe[, 2])
cov_ab <- sa_est * sb_est * rho_est
Sigma_est <- matrix(c(sa_est^2, cov_ab, cov_ab, sb_est^2), ncol = 2)

# draw contours
library(ellipse)
for (l in c(0.1, 0.3, 0.5, 0.8, 0.99)) {
  lines(ellipse(Sigma_est, centre = Mu_est, level = l),
    col = col.alpha("black", 0.2)
  )
}

## R code 14.16
# convert varying effects to waiting times
wait_morning_1 <- (a1)
wait_afternoon_1 <- (a1 + b1)
wait_morning_2 <- (a2)
wait_afternoon_2 <- (a2 + b2)

# plot both and connect with lines
plot(wait_morning_1, wait_afternoon_1,
  xlab = "morning wait",
  ylab = "afternoon wait", pch = 16, col = rangi2,
  ylim = c(min(wait_afternoon_1) - 0.1, max(wait_afternoon_1) + 0.1),
  xlim = c(min(wait_morning_1) - 0.1, max(wait_morning_1) + 0.1)
)
points(wait_morning_2, wait_afternoon_2, pch = 1)
for (i in 1:N_cafes) {
  lines(
    c(wait_morning_1[i], wait_morning_2[i]),
    c(wait_afternoon_1[i], wait_afternoon_2[i])
  )
}
abline(a = 0, b = 1, lty = 2)

## R code 14.17
# now shrinkage distribution by simulation
v <- mvrnorm(1e4, Mu_est, Sigma_est)
v[, 2] <- v[, 1] + v[, 2] # calculate afternoon wait
Sigma_est2 <- cov(v)
Mu_est2 <- Mu_est
Mu_est2[2] <- Mu_est[1] + Mu_est[2]

# draw contours
library(ellipse)
for (l in c(0.1, 0.3, 0.5, 0.8, 0.99)) {
  lines(ellipse(Sigma_est2, centre = Mu_est2, level = l),
    col = col.alpha("black", 0.5)
  )
}

## R code 14.18
library(rethinking)
data(chimpanzees)
d <- chimpanzees
d$block_id <- d$block
d$treatment <- 1L + d$prosoc_left + 2L * d$condition

dat <- list(
  L = d$pulled_left,
  tid = d$treatment,
  actor = d$actor,
  block_id = as.integer(d$block_id)
)

set.seed(4387510)
m14.2 <- ulam(
  alist(
    L ~ dbinom(1, p),
    logit(p) <- g[tid] + alpha[actor, tid] + beta[block_id, tid],

    # adaptive priors
    vector[4]:alpha[actor] ~ multi_normal(0, Rho_actor, sigma_actor),
    vector[4]:beta[block_id] ~ multi_normal(0, Rho_block, sigma_block),

    # fixed priors
    g[tid] ~ dnorm(0, 1),
    sigma_actor ~ dexp(1),
    Rho_actor ~ dlkjcorr(4),
    sigma_block ~ dexp(1),
    Rho_block ~ dlkjcorr(4)
  ),
  data = dat, chains = 4, cores = 4
)

## R code 14.19
set.seed(4387510)
m14.3 <- ulam(
  alist(
    L ~ binomial(1, p),
    logit(p) <- g[tid] + alpha[actor, tid] + beta[block_id, tid],

    # adaptive priors - non-centered
    transpars > matrix[actor, 4]:alpha <-
      compose_noncentered(sigma_actor, L_Rho_actor, z_actor),
    transpars > matrix[block_id, 4]:beta <-
      compose_noncentered(sigma_block, L_Rho_block, z_block),
    matrix[4, actor]:z_actor ~ normal(0, 1),
    matrix[4, block_id]:z_block ~ normal(0, 1),

    # fixed priors
    g[tid] ~ normal(0, 1),
    vector[4]:sigma_actor ~ dexp(1),
    cholesky_factor_corr[4]:L_Rho_actor ~ lkj_corr_cholesky(2),
    vector[4]:sigma_block ~ dexp(1),
    cholesky_factor_corr[4]:L_Rho_block ~ lkj_corr_cholesky(2),

    # compute ordinary correlation matrixes from Cholesky factors
    gq > matrix[4, 4]:Rho_actor <<- Chol_to_Corr(L_Rho_actor),
    gq > matrix[4, 4]:Rho_block <<- Chol_to_Corr(L_Rho_block)
  ),
  data = dat, chains = 4, cores = 4, log_lik = TRUE
)

## R code 14.20
# extract n_eff values for each model
neff_nc <- precis(m14.3, 3, pars = c("alpha", "beta"))$n_eff
neff_c <- precis(m14.2, 3, pars = c("alpha", "beta"))$n_eff
plot(neff_c, neff_nc,
  xlab = "centered (default)",
  ylab = "non-centered (cholesky)", lwd = 1.5
)
abline(a = 0, b = 1, lty = 2)

## R code 14.21
precis(m14.3, depth = 2, pars = c("sigma_actor", "sigma_block"))

## R code 14.22
# compute mean for each actor in each treatment
pl <- by(d$pulled_left, list(d$actor, d$treatment), mean)

# generate posterior predictions using link
datp <- list(
  actor = rep(1:7, each = 4),
  tid = rep(1:4, times = 7),
  block_id = rep(5, times = 4 * 7)
)
p_post <- link(m14.3, data = datp)
p_mu <- apply(p_post, 2, mean)
p_ci <- apply(p_post, 2, PI)

# set up plot
plot(NULL,
  xlim = c(1, 28), ylim = c(0, 1), xlab = "",
  ylab = "proportion left lever", xaxt = "n", yaxt = "n"
)
axis(2, at = c(0, 0.5, 1), labels = c(0, 0.5, 1))
abline(h = 0.5, lty = 2)
for (j in 1:7) abline(v = (j - 1) * 4 + 4.5, lwd = 0.5)
for (j in 1:7) text((j - 1) * 4 + 2.5, 1.1, concat("actor ", j), xpd = TRUE)

xo <- 0.1 # offset distance to stagger raw data and predictions
# raw data
for (j in (1:7)[-2]) {
  lines((j - 1) * 4 + c(1, 3) - xo, pl[j, c(1, 3)], lwd = 2, col = rangi2)
  lines((j - 1) * 4 + c(2, 4) - xo, pl[j, c(2, 4)], lwd = 2, col = rangi2)
}
points(1:28 - xo, t(pl), pch = 16, col = "white", cex = 1.7)
points(1:28 - xo, t(pl), pch = c(1, 1, 16, 16), col = rangi2, lwd = 2)

yoff <- 0.175
text(1 - xo, pl[1, 1] - yoff, "R/N", pos = 1, cex = 0.8)
text(2 - xo, pl[1, 2] + yoff, "L/N", pos = 3, cex = 0.8)
text(3 - xo, pl[1, 3] - yoff, "R/P", pos = 1, cex = 0.8)
text(4 - xo, pl[1, 4] + yoff, "L/P", pos = 3, cex = 0.8)

# posterior predictions
for (j in (1:7)[-2]) {
  lines((j - 1) * 4 + c(1, 3) + xo, p_mu[(j - 1) * 4 + c(1, 3)], lwd = 2)
  lines((j - 1) * 4 + c(2, 4) + xo, p_mu[(j - 1) * 4 + c(2, 4)], lwd = 2)
}
for (i in 1:28) lines(c(i, i) + xo, p_ci[, i], lwd = 1)
points(1:28 + xo, p_mu, pch = 16, col = "white", cex = 1.3)
points(1:28 + xo, p_mu, pch = c(1, 1, 16, 16))

## R code 14.23
set.seed(73)
N <- 500
U_sim <- rnorm(N)
Q_sim <- sample(1:4, size = N, replace = TRUE)
E_sim <- rnorm(N, U_sim + Q_sim)
W_sim <- rnorm(N, U_sim + 0 * E_sim)
dat_sim <- list(
  W = standardize(W_sim),
  E = standardize(E_sim),
  Q = standardize(Q_sim)
)

## R code 14.24
#m14.4 <- ulam(
#  alist(
#    W ~ dnorm(mu, sigma),
#    mu <- aW + bEW * E,
#    aW ~ dnorm(0, 0.2),
#    bEW ~ dnorm(0, 0.5),
#    sigma ~ dexp(1)
#  ),
#  data = dat_sim, chains = 4, cores = 4
#)
#precis(m14.4)

## R code 14.25
m14.5 <- ulam(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- aW + bEW * E + bQW * Q,
    aW ~ dnorm(0, 0.2),
    bEW ~ dnorm(0, 0.5),
    bQW ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = dat_sim, chains = 4, cores = 4
)
precis(m14.5)

## R code 14.26
m14.6 <- ulam(
  alist(
    c(W, E) ~ multi_normal(c(muW, muE), Rho, Sigma),
    muW <- aW + bEW * E,
    muE <- aE + bQE * Q,
    c(aW, aE) ~ normal(0, 0.2),
    c(bEW, bQE) ~ normal(0, 0.5),
    Rho ~ lkj_corr(2),
    Sigma ~ exponential(1)
  ),
  data = dat_sim, chains = 4, cores = 4
)
precis(m14.6, depth = 3)

## R code 14.27
#m14.4x <- ulam(m14.4, data = dat_sim, chains = 4, cores = 4)
m14.6x <- ulam(m14.6, data = dat_sim, chains = 4, cores = 4)

## R code 14.28
set.seed(73)
N <- 500
U_sim <- rnorm(N)
Q_sim <- sample(1:4, size = N, replace = TRUE)
E_sim <- rnorm(N, U_sim + Q_sim)
W_sim <- rnorm(N, -U_sim + 0.2 * E_sim)
dat_sim <- list(
  W = standardize(W_sim),
  E = standardize(E_sim),
  Q = standardize(Q_sim)
)

## R code 14.29
library(dagitty)
dagIV <- dagitty("dag{ Q -> E <- U -> W <- E }")
instrumentalVariables(dagIV, exposure = "E", outcome = "W")

## R code 14.30
library(rethinking)
data(KosterLeckie)

## R code 14.31
kl_data <- list(
  N = nrow(kl_dyads),
  N_households = max(kl_dyads$hidB),
  did = kl_dyads$did,
  hidA = kl_dyads$hidA,
  hidB = kl_dyads$hidB,
  giftsAB = kl_dyads$giftsAB,
  giftsBA = kl_dyads$giftsBA
)

m14.7 <- ulam(
  alist(
    giftsAB ~ poisson(lambdaAB),
    giftsBA ~ poisson(lambdaBA),
    log(lambdaAB) <- a + gr[hidA, 1] + gr[hidB, 2] + d[did, 1],
    log(lambdaBA) <- a + gr[hidB, 1] + gr[hidA, 2] + d[did, 2],
    a ~ normal(0, 1),

    ## gr matrix of varying effects
    vector[2]:gr[N_households] ~ multi_normal(0, Rho_gr, sigma_gr),
    Rho_gr ~ lkj_corr(4),
    sigma_gr ~ exponential(1),

    ## dyad effects
    transpars > matrix[N, 2]:d <-
      compose_noncentered(rep_vector(sigma_d, 2), L_Rho_d, z),
    matrix[2, N]:z ~ normal(0, 1),
    cholesky_factor_corr[2]:L_Rho_d ~ lkj_corr_cholesky(8),
    sigma_d ~ exponential(1),

    ## compute correlation matrix for dyads
    gq > matrix[2, 2]:Rho_d <<- Chol_to_Corr(L_Rho_d)
  ),
  data = kl_data, chains = 4, cores = 4, iter = 2000
)

## R code 14.32
precis(m14.7, depth = 3, pars = c("Rho_gr", "sigma_gr"))

## R code 14.33
post <- extract.samples(m14.7)
g <- sapply(1:25, function(i) post$a + post$gr[, i, 1])
r <- sapply(1:25, function(i) post$a + post$gr[, i, 2])
Eg_mu <- apply(exp(g), 2, mean)
Er_mu <- apply(exp(r), 2, mean)

## R code 14.34
plot(NULL,
  xlim = c(0, 8.6), ylim = c(0, 8.6), xlab = "generalized giving",
  ylab = "generalized receiving", lwd = 1.5
)
abline(a = 0, b = 1, lty = 2)

# ellipses
library(ellipse)
for (i in 1:25) {
  Sigma <- cov(cbind(g[, i], r[, i]))
  Mu <- c(mean(g[, i]), mean(r[, i]))
  for (l in c(0.5)) {
    el <- ellipse(Sigma, centre = Mu, level = l)
    lines(exp(el), col = col.alpha("black", 0.5))
  }
}
# household means
points(Eg_mu, Er_mu, pch = 21, bg = "white", lwd = 1.5)

## R code 14.35
precis(m14.7, depth = 3, pars = c("Rho_d", "sigma_d"))

## R code 14.36
dy1 <- apply(post$d[, , 1], 2, mean)
dy2 <- apply(post$d[, , 2], 2, mean)
plot(dy1, dy2)

## R code 14.37
# load the distance matrix
library(rethinking)
data(islandsDistMatrix)

# display (measured in thousands of km)
Dmat <- islandsDistMatrix
colnames(Dmat) <- c("Ml", "Ti", "SC", "Ya", "Fi", "Tr", "Ch", "Mn", "To", "Ha")
round(Dmat, 1)

## R code 14.38
# linear
curve(exp(-1 * x), from = 0, to = 4, lty = 2)
# squared
curve(exp(-1 * x^2), add = TRUE)

## R code 14.39
data(Kline2) # load the ordinary data, now with coordinates
d <- Kline2
d$society <- 1:10 # index observations

dat_list <- list(
  T = d$total_tools,
  P = d$population,
  society = d$society,
  Dmat = islandsDistMatrix
)

m14.8 <- ulam(
  alist(
    T ~ dpois(lambda),
    lambda <- (a * P^b / g) * exp(k[society]),
    vector[10]:k ~ multi_normal(0, SIGMA),
    matrix[10, 10]:SIGMA <- cov_GPL2(Dmat, etasq, rhosq, 0.01),
    c(a, b, g) ~ dexp(1),
    etasq ~ dexp(2),
    rhosq ~ dexp(0.5)
  ),
  data = dat_list, chains = 4, cores = 4, iter = 2000
)

## R code 14.40
precis(m14.8, depth = 3)

## R code 14.41
post <- extract.samples(m14.8)

# plot the posterior median covariance function
plot(NULL,
  xlab = "distance (thousand km)", ylab = "covariance",
  xlim = c(0, 10), ylim = c(0, 2)
)

# compute posterior mean covariance
x_seq <- seq(from = 0, to = 10, length.out = 100)
pmcov <- sapply(x_seq, function(x) post$etasq * exp(-post$rhosq * x^2))
pmcov_mu <- apply(pmcov, 2, mean)
lines(x_seq, pmcov_mu, lwd = 2)

# plot 60 functions sampled from posterior
for (i in 1:50) {
  curve(post$etasq[i] * exp(-post$rhosq[i] * x^2),
    add = TRUE,
    col = col.alpha("black", 0.3)
  )
}

## R code 14.42
# compute posterior median covariance among societies
K <- matrix(0, nrow = 10, ncol = 10)
for (i in 1:10) {
  for (j in 1:10) {
    K[i, j] <- median(post$etasq) *
      exp(-median(post$rhosq) * islandsDistMatrix[i, j]^2)
  }
}
diag(K) <- median(post$etasq) + 0.01

## R code 14.43
# convert to correlation matrix
Rho <- round(cov2cor(K), 2)
# add row/col names for convenience
colnames(Rho) <- c("Ml", "Ti", "SC", "Ya", "Fi", "Tr", "Ch", "Mn", "To", "Ha")
rownames(Rho) <- colnames(Rho)
Rho

## R code 14.44
# scale point size to logpop
psize <- d$logpop / max(d$logpop)
psize <- exp(psize * 1.5) - 2

# plot raw data and labels
plot(d$lon2, d$lat,
  xlab = "longitude", ylab = "latitude",
  col = rangi2, cex = psize, pch = 16, xlim = c(-50, 30)
)
labels <- as.character(d$culture)
text(d$lon2, d$lat, labels = labels, cex = 0.7, pos = c(2, 4, 3, 3, 4, 1, 3, 2, 4, 2))

# overlay lines shaded by Rho
for (i in 1:10) {
  for (j in 1:10) {
    if (i < j) {
      lines(c(d$lon2[i], d$lon2[j]), c(d$lat[i], d$lat[j]),
        lwd = 2, col = col.alpha("black", Rho[i, j]^2)
      )
    }
  }
}

## R code 14.45
# compute posterior median relationship, ignoring distance
logpop.seq <- seq(from = 6, to = 14, length.out = 30)
# TODO: Is it as simple as changing this to `b`? He mentions `bp` in the text but it doesn't seem to
# exist in the model. Otherwise comment this out.
lambda <- sapply(logpop.seq, function(lp) exp(post$a + post$b * lp))
lambda.median <- apply(lambda, 2, median)
lambda.PI80 <- apply(lambda, 2, PI, prob = 0.8)

# plot raw data and labels
plot(d$logpop, d$total_tools,
  col = rangi2, cex = psize, pch = 16,
  xlab = "log population", ylab = "total tools"
)
text(d$logpop, d$total_tools,
  labels = labels, cex = 0.7,
  pos = c(4, 3, 4, 2, 2, 1, 4, 4, 4, 2)
)

# display posterior predictions
lines(logpop.seq, lambda.median, lty = 2)
lines(logpop.seq, lambda.PI80[1, ], lty = 2)
lines(logpop.seq, lambda.PI80[2, ], lty = 2)

# overlay correlations
for (i in 1:10) {
  for (j in 1:10) {
    if (i < j) {
      lines(c(d$logpop[i], d$logpop[j]),
        c(d$total_tools[i], d$total_tools[j]),
        lwd = 2, col = col.alpha("black", Rho[i, j]^2)
      )
    }
  }
}

## R code 14.46
m14.8nc <- ulam(
  alist(
    T ~ dpois(lambda),
    lambda <- (a * P^b / g) * exp(k[society]),

    # non-centered Gaussian Process prior
    transpars > vector[10]:k <<- L_SIGMA * z,
    vector[10]:z ~ normal(0, 1),
    transpars > matrix[10, 10]:L_SIGMA <<- cholesky_decompose(SIGMA),
    transpars > matrix[10, 10]:SIGMA <- cov_GPL2(Dmat, etasq, rhosq, 0.01),
    c(a, b, g) ~ dexp(1),
    etasq ~ dexp(2),
    rhosq ~ dexp(0.5)
  ),
  data = dat_list, chains = 4, cores = 4, iter = 2000
)

## R code 14.47
library(rethinking)
data(Primates301)
data(Primates301_nex)

# plot it using ape package - install.packages('ape') if needed
library(ape)
plot(ladderize(Primates301_nex),
  type = "fan", font = 1, no.margin = TRUE,
  label.offset = 1, cex = 0.5
)

## R code 14.48
d <- Primates301
d$name <- as.character(d$name)
dstan <- d[complete.cases(d$group_size, d$body, d$brain), ]
spp_obs <- dstan$name

## R code 14.49
dat_list <- list(
  N_spp = nrow(dstan),
  M = standardize(log(dstan$body)),
  B = standardize(log(dstan$brain)),
  G = standardize(log(dstan$group_size)),
  Imat = diag(nrow(dstan))
)

m14.9 <- ulam(
  alist(
    B ~ multi_normal(mu, SIGMA),
    mu <- a + bM * M + bG * G,
    matrix[N_spp, N_spp]:SIGMA <- Imat * sigma_sq,
    a ~ normal(0, 1),
    c(bM, bG) ~ normal(0, 0.5),
    sigma_sq ~ exponential(1)
  ),
  data = dat_list, chains = 4, cores = 4
)
precis(m14.9)

## R code 14.50
library(ape)
tree_trimmed <- keep.tip(Primates301_nex, spp_obs)
Rbm <- corBrownian(phy = tree_trimmed)
V <- vcv(Rbm)
Dmat <- cophenetic(tree_trimmed)
plot(Dmat, V, xlab = "phylogenetic distance", ylab = "covariance")

## R code 14.51
# put species in right order
dat_list$V <- V[spp_obs, spp_obs]
# convert to correlation matrix
dat_list$R <- dat_list$V / max(V)

# Brownian motion model
m14.10 <- ulam(
  alist(
    B ~ multi_normal(mu, SIGMA),
    mu <- a + bM * M + bG * G,
    matrix[N_spp, N_spp]:SIGMA <- R * sigma_sq,
    a ~ normal(0, 1),
    c(bM, bG) ~ normal(0, 0.5),
    sigma_sq ~ exponential(1)
  ),
  data = dat_list, chains = 4, cores = 4
)
precis(m14.10)

## R code 14.52
# add scaled and reordered distance matrix
dat_list$Dmat <- Dmat[spp_obs, spp_obs] / max(Dmat)

m14.11 <- ulam(
  alist(
    B ~ multi_normal(mu, SIGMA),
    mu <- a + bM * M + bG * G,
    matrix[N_spp, N_spp]:SIGMA <- cov_GPL1(Dmat, etasq, rhosq, 0.01),
    a ~ normal(0, 1),
    c(bM, bG) ~ normal(0, 0.5),
    etasq ~ half_normal(1, 0.25),
    rhosq ~ half_normal(3, 0.25)
  ),
  data = dat_list, chains = 4, cores = 4
)
precis(m14.11)

## R code 14.53
post <- extract.samples(m14.11)
plot(NULL,
  xlim = c(0, max(dat_list$Dmat)), ylim = c(0, 1.5),
  xlab = "phylogenetic distance", ylab = "covariance"
)

# posterior
for (i in 1:30) {
  curve(post$etasq[i] * exp(-post$rhosq[i] * x), add = TRUE, col = rangi2)
}

# prior mean and 89% interval
eta <- abs(rnorm(1e3, 1, 0.25))
rho <- abs(rnorm(1e3, 3, 0.25))
d_seq <- seq(from = 0, to = 1, length.out = 50)
K <- sapply(d_seq, function(x) eta * exp(-rho * x))
lines(d_seq, colMeans(K), lwd = 2)
shade(apply(K, 2, PI), d_seq)
text(0.5, 0.5, "prior")
text(0.2, 0.1, "posterior", col = rangi2)

## R code 14.54
# S <- matrix(c(sa^2, sa * sb * rho, sa * sb * rho, sb^2), nrow = 2)

## R code 15.1
# simulate a pancake and return randomly ordered sides
sim_pancake <- function() {
  pancake <- sample(1:3, 1)
  sides <- matrix(c(1, 1, 1, 0, 0, 0), 2, 3)[, pancake]
  sample(sides)
}

# sim 10,000 pancakes
pancakes <- replicate(1e4, sim_pancake())
up <- pancakes[1, ]
down <- pancakes[2, ]

# compute proportion 1/1 (BB) out of all 1/1 and 1/0
num_11_10 <- sum(up == 1)
num_11 <- sum(up == 1 & down == 1)
num_11 / num_11_10

## R code 15.2
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce

# points
plot(d$Divorce ~ d$MedianAgeMarriage,
  ylim = c(4, 15),
  xlab = "Median age marriage", ylab = "Divorce rate"
)

# standard errors
for (i in 1:nrow(d)) {
  ci <- d$Divorce[i] + c(-1, 1) * d$Divorce.SE[i]
  x <- d$MedianAgeMarriage[i]
  lines(c(x, x), ci)
}

## R code 15.3
dlist <- list(
  D_obs = standardize(d$Divorce),
  D_sd = d$Divorce.SE / sd(d$Divorce),
  M = standardize(d$Marriage),
  A = standardize(d$MedianAgeMarriage),
  N = nrow(d)
)

m15.1 <- ulam(
  alist(
    D_obs ~ dnorm(D_true, D_sd),
    vector[N]:D_true ~ dnorm(mu, sigma),
    mu <- a + bA * A + bM * M,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.5),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = dlist, chains = 4, cores = 4
)

## R code 15.4
precis(m15.1, depth = 2)

## R code 15.5
dlist <- list(
  D_obs = standardize(d$Divorce),
  D_sd = d$Divorce.SE / sd(d$Divorce),
  M_obs = standardize(d$Marriage),
  M_sd = d$Marriage.SE / sd(d$Marriage),
  A = standardize(d$MedianAgeMarriage),
  N = nrow(d)
)

m15.2 <- ulam(
  alist(
    D_obs ~ dnorm(D_true, D_sd),
    vector[N]:D_true ~ dnorm(mu, sigma),
    mu <- a + bA * A + bM * M_true[i],
    M_obs ~ dnorm(M_true, M_sd),
    vector[N]:M_true ~ dnorm(0, 1),
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.5),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = dlist, chains = 4, cores = 4
)

## R code 15.6
post <- extract.samples(m15.2)
D_true <- apply(post$D_true, 2, mean)
M_true <- apply(post$M_true, 2, mean)
plot(dlist$M_obs, dlist$D_obs,
  pch = 16, col = rangi2,
  xlab = "marriage rate (std)", ylab = "divorce rate (std)"
)
points(M_true, D_true)
for (i in 1:nrow(d)) {
  lines(c(dlist$M_obs[i], M_true[i]), c(dlist$D_obs[i], D_true[i]))
}

## R code 15.7
# N <- 500
# A <- rnorm(N)
# M <- rnorm(N, -A)
# D <- rnorm(N, A)
# A_obs <- rnorm(N, A)
# 
# ## R code 15.8
# N <- 100
# S <- rnorm(N)
# H <- rbinom(N, size = 10, inv_logit(S))
# 
# ## R code 15.9
# D <- rbern(N) # dogs completely random
# Hm <- H
# Hm[D == 1] <- NA
# 
# ## R code 15.10
# D <- ifelse(S > 0, 1, 0)
# Hm <- H
# Hm[D == 1] <- NA
# 
# ## R code 15.11
# set.seed(501)
# N <- 1000
# X <- rnorm(N)
# S <- rnorm(N)
# H <- rbinom(N, size = 10, inv_logit(2 + S - 2 * X))
# D <- ifelse(X > 1, 1, 0)
# Hm <- H
# Hm[D == 1] <- NA
# 
# ## R code 15.12
# dat_list <- list(
#   H = H,
#   S = S
# )
# 
# m15.3 <- ulam(
#   alist(
#     H ~ binomial(10, p),
#     logit(p) <- a + bS * S,
#     a ~ normal(0, 1),
#     bS ~ normal(0, 0.5)
#   ),
#   data = dat_list, chains = 4
# )
# precis(m15.3)
# 
# ## R code 15.13
# dat_list0 <- list(H = H[D == 0], S = S[D == 0])
# 
# m15.4 <- ulam(
#   alist(
#     H ~ binomial(10, p),
#     logit(p) <- a + bS * S,
#     a ~ normal(0, 1),
#     bS ~ normal(0, 0.5)
#   ),
#   data = dat_list0, chains = 4
# )
# precis(m15.4)
# 
# ## R code 15.14
# D <- ifelse(abs(X) < 1, 1, 0)
# 
# ## R code 15.15
# N <- 100
# S <- rnorm(N)
# H <- rbinom(N, size = 10, inv_logit(S))
# D <- ifelse(H < 5, 1, 0)
# Hm <- H
# Hm[D == 1] <- NA
# 
# ## R code 15.16
# library(rethinking)
# data(milk)
# d <- milk
# d$neocortex.prop <- d$neocortex.perc / 100
# d$logmass <- log(d$mass)
# dat_list <- list(
#   K = standardize(d$kcal.per.g),
#   B = standardize(d$neocortex.prop),
#   M = standardize(d$logmass)
# )
# 
# ## R code 15.17
# m15.5 <- ulam(
#   alist(
#     K ~ dnorm(mu, sigma),
#     mu <- a + bB * B + bM * M,
#     B ~ dnorm(nu, sigma_B),
#     c(a, nu) ~ dnorm(0, 0.5),
#     c(bB, bM) ~ dnorm(0, 0.5),
#     sigma_B ~ dexp(1),
#     sigma ~ dexp(1)
#   ),
#   data = dat_list, chains = 4, cores = 4
# )
# 
# ## R code 15.18
# precis(m15.5, depth = 2)
# 
# ## R code 15.19
# obs_idx <- which(!is.na(d$neocortex.prop))
# dat_list_obs <- list(
#   K = dat_list$K[obs_idx],
#   B = dat_list$B[obs_idx],
#   M = dat_list$M[obs_idx]
# )
# m15.6 <- ulam(
#   alist(
#     K ~ dnorm(mu, sigma),
#     mu <- a + bB * B + bM * M,
#     B ~ dnorm(nu, sigma_B),
#     c(a, nu) ~ dnorm(0, 0.5),
#     c(bB, bM) ~ dnorm(0, 0.5),
#     sigma_B ~ dexp(1),
#     sigma ~ dexp(1)
#   ),
#   data = dat_list_obs, chains = 4, cores = 4
# )
# precis(m15.6)
# 
# ## R code 15.20
# plot(coeftab(m15.5, m15.6), pars = c("bB", "bM"))
# 
# ## R code 15.21
# post <- extract.samples(m15.5)
# B_impute_mu <- apply(post$B_impute, 2, mean)
# B_impute_ci <- apply(post$B_impute, 2, PI)
# 
# # B vs K
# plot(dat_list$B, dat_list$K,
#   pch = 16, col = rangi2,
#   xlab = "neocortex percent (std)", ylab = "kcal milk (std)"
# )
# miss_idx <- which(is.na(dat_list$B))
# Ki <- dat_list$K[miss_idx]
# points(B_impute_mu, Ki)
# for (i in 1:12) lines(B_impute_ci[, i], rep(Ki[i], 2))
# 
# # M vs B
# plot(dat_list$M, dat_list$B,
#   pch = 16, col = rangi2,
#   ylab = "neocortex percent (std)", xlab = "log body mass (std)"
# )
# Mi <- dat_list$M[miss_idx]
# points(Mi, B_impute_mu)
# for (i in 1:12) lines(rep(Mi[i], 2), B_impute_ci[, i])
# 
# ## R code 15.22
# m15.7 <- ulam(
#   alist(
#     # K as function of B and M
#     K ~ dnorm(mu, sigma),
#     mu <- a + bB * B_merge + bM * M,
# 
#     # M and B correlation
#     MB ~ multi_normal(c(muM, muB), Rho_BM, Sigma_BM),
#     matrix[29, 2]:MB <<- append_col(M, B_merge),
# 
#     # define B_merge as mix of observed and imputed values
#     vector[29]:B_merge <- merge_missing(B, B_impute),
# 
#     # priors
#     c(a, muB, muM) ~ dnorm(0, 0.5),
#     c(bB, bM) ~ dnorm(0, 0.5),
#     sigma ~ dexp(1),
#     Rho_BM ~ lkj_corr(2),
#     Sigma_BM ~ dexp(1)
#   ),
#   data = dat_list, chains = 4, cores = 4
# )
# precis(m15.7, depth = 3, pars = c("bM", "bB", "Rho_BM"))
# 
# ## R code 15.23
# B_missidx <- which(is.na(dat_list$B))
# 
# ## R code 15.24
# data(Moralizing_gods)
# str(Moralizing_gods)
# 
# ## R code 15.25
# table(Moralizing_gods$moralizing_gods, useNA = "always")
# 
# ## R code 15.26
# symbol <- ifelse(Moralizing_gods$moralizing_gods == 1, 16, 1)
# symbol <- ifelse(is.na(Moralizing_gods$moralizing_gods), 4, symbol)
# color <- ifelse(is.na(Moralizing_gods$moralizing_gods), "black", rangi2)
# plot(Moralizing_gods$year, Moralizing_gods$population,
#   pch = symbol,
#   col = color, xlab = "Time (year)", ylab = "Population size", lwd = 1.5
# )
# 
# ## R code 15.27
# with(
#   Moralizing_gods,
#   table(gods = moralizing_gods, literacy = writing, useNA = "always")
# )
# 
# ## R code 15.28
# haw <- which(Moralizing_gods$polity == "Big Island Hawaii")
# columns <- c("year", "writing", "moralizing_gods")
# t(Moralizing_gods[haw, columns])
# 
# ## R code 15.29
# set.seed(9)
# N_houses <- 100L
# alpha <- 5
# beta <- (-3)
# k <- 0.5
# r <- 0.2
# cat <- rbern(N_houses, k)
# notes <- rpois(N_houses, alpha + beta * cat)
# R_C <- rbern(N_houses, r)
# cat_obs <- cat
# cat_obs[R_C == 1] <- (-9L)
# dat <- list(
#   notes = notes,
#   cat = cat_obs,
#   RC = R_C,
#   N = as.integer(N_houses)
# )
# 
# ## R code 15.30
# m15.8 <- ulam(
#   alist(
#     # singing bird model
#     ## cat known present/absent:
#     notes | RC == 0 ~ poisson(lambda),
#     log(lambda) <- a + b * cat,
#     ## cat NA:
#     notes | RC == 1 ~ custom(log_sum_exp(
#       log(k) + poisson_lpmf(notes | exp(a + b)),
#       log(1 - k) + poisson_lpmf(notes | exp(a))
#     )),
# 
#     # priors
#     a ~ normal(0, 1),
#     b ~ normal(0, 0.5),
# 
#     # sneaking cat model
#     cat | RC == 0 ~ bernoulli(k),
#     k ~ beta(2, 2)
#   ),
#   data = dat, chains = 4, cores = 4
# )
# 
# ## R code 15.31
# m15.9 <- ulam(
#   alist(
#     # singing bird model
#     notes | RC == 0 ~ poisson(lambda),
#     notes | RC == 1 ~ custom(log_sum_exp(
#       log(k) + poisson_lpmf(notes | exp(a + b)),
#       log(1 - k) + poisson_lpmf(notes | exp(a))
#     )),
#     log(lambda) <- a + b * cat,
#     a ~ normal(0, 1),
#     b ~ normal(0, 0.5),
# 
#     # sneaking cat model
#     cat | RC == 0 ~ bernoulli(k),
#     k ~ beta(2, 2),
# 
#     # imputed values
#     gq > vector[N]:PrC1 <- exp(lpC1) / (exp(lpC1) + exp(lpC0)),
#     gq > vector[N]:lpC1 <- log(k) + poisson_lpmf(notes[i] | exp(a + b)),
#     gq > vector[N]:lpC0 <- log(1 - k) + poisson_lpmf(notes[i] | exp(a))
#   ),
#   data = dat, chains = 4, cores = 4
# )
# 
# ## R code 15.32
# set.seed(100)
# x <- c(rnorm(10), NA)
# y <- c(rnorm(10, x), 100)
# d <- list(x = x, y = y)
# 
# ## R code 15.33
# ## R code 15.34
# ## R code 15.35
# ## R code 15.36
# ## R code 15.37
# ## R code 15.38
# ## R code 15.39
# ## R code 16.1
# library(rethinking)
# data(Howell1)
# d <- Howell1
# 
# # scale observed variables
# d$w <- d$weight / mean(d$weight)
# d$h <- d$height / mean(d$height)
# 
# ## R code 16.2
# m16.1 <- ulam(
#   alist(
#     w ~ dlnorm(mu, sigma),
#     exp(mu) <- 3.141593 * k * p^2 * h^3,
#     p ~ beta(2, 18),
#     k ~ exponential(0.5),
#     sigma ~ exponential(1)
#   ),
#   data = d, chains = 4, cores = 4
# )
# 
# ## R code 16.3
# h_seq <- seq(from = 0, to = max(d$h), length.out = 30)
# w_sim <- sim(m16.1, data = list(h = h_seq))
# mu_mean <- apply(w_sim, 2, mean)
# w_CI <- apply(w_sim, 2, PI)
# plot(d$h, d$w,
#   xlim = c(0, max(d$h)), ylim = c(0, max(d$w)), col = rangi2,
#   lwd = 2, xlab = "height (scaled)", ylab = "weight (scaled)"
# )
# lines(h_seq, mu_mean)
# shade(w_CI, h_seq)
# 
# ## R code 16.4
# library(rethinking)
# data(Boxes)
# precis(Boxes)
# 
# ## R code 16.5
# table(Boxes$y) / length(Boxes$y)
# 
# ## R code 16.6
# set.seed(7)
# N <- 30 # number of children
# 
# # half are random
# # sample from 1,2,3 at random for each
# y1 <- sample(1:3, size = N / 2, replace = TRUE)
# 
# # half follow majority
# y2 <- rep(2, N / 2)
# 
# # combine and shuffle y1 and y2
# y <- sample(c(y1, y2))
# 
# # count the 2s
# sum(y == 2) / N
# 
# ## R code 16.7
# data(Boxes_model)
# cat(Boxes_model)
# 
# ## R code 16.8
# # prep data
# dat_list <- list(
#   N = nrow(Boxes),
#   y = Boxes$y,
#   majority_first = Boxes$majority_first
# )
# 
# # run the sampler
# m16.2 <- stan(model_code = Boxes_model, data = dat_list, chains = 3, cores = 3)
# 
# # show marginal posterior for p
# p_labels <- c(
#   "1 Majority", "2 Minority", "3 Maverick", "4 Random",
#   "5 Follow First"
# )
# plot(precis(m16.2, 2), labels = p_labels)
# 
# ## R code 16.9
# library(rethinking)
# data(Panda_nuts)
# 
# ## R code 16.10
# N <- 1e4
# phi <- rlnorm(N, log(1), 0.1)
# k <- rlnorm(N, log(2), 0.25)
# theta <- rlnorm(N, log(5), 0.25)
# 
# # relative grow curve
# plot(NULL,
#   xlim = c(0, 1.5), ylim = c(0, 1), xaxt = "n", xlab = "age",
#   ylab = "body mass"
# )
# at <- c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5)
# axis(1, at = at, labels = round(at * max(Panda_nuts$age)))
# for (i in 1:20) curve((1 - exp(-k[i] * x)), add = TRUE, col = grau(), lwd = 1.5)
# 
# # implied rate of nut opening curve
# plot(NULL,
#   xlim = c(0, 1.5), ylim = c(0, 1.2), xaxt = "n", xlab = "age",
#   ylab = "nuts per second"
# )
# at <- c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5)
# axis(1, at = at, labels = round(at * max(Panda_nuts$age)))
# for (i in 1:20) {
#   curve(phi[i] * (1 - exp(-k[i] * x))^theta[i],
#     add = TRUE,
#     col = grau(), lwd = 1.5
#   )
# }
# 
# ## R code 16.11
# dat_list <- list(
#   n = as.integer(Panda_nuts$nuts_opened),
#   age = Panda_nuts$age / max(Panda_nuts$age),
#   seconds = Panda_nuts$seconds
# )
# 
# m16.4 <- ulam(
#   alist(
#     n ~ poisson(lambda),
#     lambda <- seconds * phi * (1 - exp(-k * age))^theta,
#     phi ~ lognormal(log(1), 0.1),
#     k ~ lognormal(log(2), 0.25),
#     theta ~ lognormal(log(5), 0.25)
#   ),
#   data = dat_list, chains = 4
# )
# 
# ## R code 16.12
# post <- extract.samples(m16.4)
# plot(NULL,
#   xlim = c(0, 1), ylim = c(0, 1.5), xlab = "age",
#   ylab = "nuts per second", xaxt = "n"
# )
# at <- c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5)
# axis(1, at = at, labels = round(at * max(Panda_nuts$age)))
# 
# # raw data
# pts <- dat_list$n / dat_list$seconds
# point_size <- normalize(dat_list$seconds)
# points(jitter(dat_list$age), pts, col = rangi2, lwd = 2, cex = point_size * 3)
# 
# # 30 posterior curves
# for (i in 1:30) {
#   with(
#     post,
#     curve(phi[i] * (1 - exp(-k[i] * x))^theta[i], add = TRUE, col = grau())
#   )
# }
# 
# ## R code 16.13
# library(rethinking)
# data(Lynx_Hare)
# plot(1:21, Lynx_Hare[, 3],
#   ylim = c(0, 90), xlab = "year",
#   ylab = "thousands of pelts", xaxt = "n", type = "l", lwd = 1.5
# )
# at <- c(1, 11, 21)
# axis(1, at = at, labels = Lynx_Hare$Year[at])
# lines(1:21, Lynx_Hare[, 2], lwd = 1.5, col = rangi2)
# points(1:21, Lynx_Hare[, 3], bg = "black", col = "white", pch = 21, cex = 1.4)
# points(1:21, Lynx_Hare[, 2], bg = rangi2, col = "white", pch = 21, cex = 1.4)
# text(17, 80, "Lepus", pos = 2)
# text(19, 50, "Lynx", pos = 2, col = rangi2)
# 
# ## R code 16.14
# sim_lynx_hare <- function(n_steps, init, theta, dt = 0.002) {
#   L <- rep(NA, n_steps)
#   H <- rep(NA, n_steps)
#   L[1] <- init[1]
#   H[1] <- init[2]
#   for (i in 2:n_steps) {
#     H[i] <- H[i - 1] + dt * H[i - 1] * (theta[1] - theta[2] * L[i - 1])
#     L[i] <- L[i - 1] + dt * L[i - 1] * (theta[3] * H[i - 1] - theta[4])
#   }
#   return(cbind(L, H))
# }
# 
# ## R code 16.15
# theta <- c(0.5, 0.05, 0.025, 0.5)
# z <- sim_lynx_hare(1e4, as.numeric(Lynx_Hare[1, 2:3]), theta)
# 
# plot(z[, 2],
#   type = "l", ylim = c(0, max(z[, 2])), lwd = 2, xaxt = "n",
#   ylab = "number (thousands)", xlab = ""
# )
# lines(z[, 1], col = rangi2, lwd = 2)
# mtext("time", 1)
# 
# ## R code 16.16
# N <- 1e4
# Ht <- 1e4
# p <- rbeta(N, 2, 18)
# h <- rbinom(N, size = Ht, prob = p)
# h <- round(h / 1000, 2)
# dens(h, xlab = "thousand of pelts", lwd = 2)
# 
# ## R code 16.17
# data(Lynx_Hare_model)
# cat(Lynx_Hare_model)
# 
# ## R code 16.18
# dat_list <- list(
#   N = nrow(Lynx_Hare),
#   pelts = Lynx_Hare[, 2:3]
# )
# 
# m16.5 <- stan(
#   model_code = Lynx_Hare_model, data = dat_list, chains = 3,
#   cores = 3, control = list(adapt_delta = 0.95)
# )
# 
# ## R code 16.19
# post <- extract.samples(m16.5)
# pelts <- dat_list$pelts
# plot(1:21, pelts[, 2],
#   pch = 16, ylim = c(0, 120), xlab = "year",
#   ylab = "thousands of pelts", xaxt = "n"
# )
# at <- c(1, 11, 21)
# axis(1, at = at, labels = Lynx_Hare$Year[at])
# points(1:21, pelts[, 1], col = rangi2, pch = 16)
# # 21 time series from posterior
# for (s in 1:21) {
#   lines(1:21, post$pelts_pred[s, , 2], col = col.alpha("black", 0.2), lwd = 2)
#   lines(1:21, post$pelts_pred[s, , 1], col = col.alpha(rangi2, 0.3), lwd = 2)
# }
# # text labels
# text(17, 90, "Lepus", pos = 2)
# text(19, 50, "Lynx", pos = 2, col = rangi2)
# 
# ## R code 16.20
# plot(NULL,
#   pch = 16, xlim = c(1, 21), ylim = c(0, 500), xlab = "year",
#   ylab = "thousands of animals", xaxt = "n"
# )
# at <- c(1, 11, 21)
# axis(1, at = at, labels = Lynx_Hare$Year[at])
# for (s in 1:21) {
#   lines(1:21, post$pop[s, , 2], col = col.alpha("black", 0.2), lwd = 2)
#   lines(1:21, post$pop[s, , 1], col = col.alpha(rangi2, 0.4), lwd = 2)
# }
# 
# ## R code 16.21
# data(Lynx_Hare)
# dat_ar1 <- list(
#   L = Lynx_Hare$Lynx[2:21],
#   L_lag1 = Lynx_Hare$Lynx[1:20],
#   H = Lynx_Hare$Hare[2:21],
#   H_lag1 = Lynx_Hare$Hare[1:20]
# )
