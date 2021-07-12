source("model-finite-counts.R")

## R code 11.35
y <- rbinom(1e5, 1000, 1 / 1000)
c(mean(y), var(y))

source('load-basic-islands-model.R')

## R code 11.38
curve(dlnorm(x, 0, 10), from = 0, to = 100, n = 200)

## R code 11.39
a <- rnorm(1e4, 0, 10)
lambda <- exp(a)
mean(lambda)

## R code 11.40
curve(dlnorm(x, 3, 0.5), from = 0, to = 100, n = 200)

## R code 11.41
N <- 100
a <- rnorm(N, 3, 0.5)
b <- rnorm(N, 0, 10)
plot(NULL, xlim = c(-2, 2), ylim = c(0, 100))
for (i in 1:N) curve(exp(a[i] + b[i] * x), add = TRUE, col = grau())

## R code 11.42
set.seed(10)
N <- 100
a <- rnorm(N, 3, 0.5)
b <- rnorm(N, 0, 0.2)
plot(NULL, xlim = c(-2, 2), ylim = c(0, 100))
for (i in 1:N) curve(exp(a[i] + b[i] * x), add = TRUE, col = grau())

## R code 11.43
x_seq <- seq(from = log(100), to = log(200000), length.out = 100)
lambda <- sapply(x_seq, function(x) exp(a + b * x))
plot(NULL,
  xlim = range(x_seq), ylim = c(0, 500), xlab = "log population",
  ylab = "total tools"
)
for (i in 1:N) lines(x_seq, lambda[i, ], col = grau(), lwd = 1.5)

## R code 11.44
plot(NULL,
  xlim = range(exp(x_seq)), ylim = c(0, 500), xlab = "population",
  ylab = "total tools"
)
for (i in 1:N) lines(exp(x_seq), lambda[i, ], col = grau(), lwd = 1.5)

# intercept only
m11.9 <- ulam(
  alist(
    T ~ dpois(lambda),
    log(lambda) <- a,
    a ~ dnorm(3, 0.5)
  ),
  data = dat, chains = 4, log_lik = TRUE
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

## R code 11.46
compare(m11.9, m11.10, func = PSIS)

## R code 11.49
dat2 <- list(T = d$total_tools, P = d$population, cid = d$contact_id)
m11.11 <- ulam(
  alist(
    T ~ dpois(lambda),
    lambda <- exp(a[cid]) * P^b[cid] / g,
    a[cid] ~ dnorm(1, 1),
    b[cid] ~ dexp(1),
    g ~ dexp(1)
  ),
  data = dat2, chains = 4, log_lik = TRUE
)

## R code 11.50
num_days <- 30
y <- rpois(num_days, 1.5)

## R code 11.51
num_weeks <- 4
y_new <- rpois(num_weeks, 0.5 * 7)

## R code 11.52
y_all <- c(y, y_new)
exposure <- c(rep(1, 30), rep(7, 4))
monastery <- c(rep(0, 30), rep(1, 4))
d <- data.frame(y = y_all, days = exposure, monastery = monastery)

## R code 11.53
# compute the offset
d$log_days <- log(d$days)

# fit the model
m11.12 <- quap(
  alist(
    y ~ dpois(lambda),
    log(lambda) <- log_days + a + b * monastery,
    a ~ dnorm(0, 1),
    b ~ dnorm(0, 1)
  ),
  data = d
)

## R code 11.54
post <- extract.samples(m11.12)
lambda_old <- exp(post$a)
lambda_new <- exp(post$a + post$b)
precis(data.frame(lambda_old, lambda_new))

## R code 11.55
# simulate career choices among 500 individuals
N <- 500 # number of individuals
income <- c(1, 2, 5) # expected income of each career
score <- 0.5 * income # scores for each career, based on income
# next line converts scores to probabilities
p <- softmax(score[1], score[2], score[3])

# now simulate choice
# outcome career holds event type values, not counts
career <- rep(NA, N) # empty vector of choices for each individual
# sample chosen career for each individual
set.seed(34302)
for (i in 1:N) career[i] <- sample(1:3, size = 1, prob = p)

## R code 11.56
code_m11.13 <- "
data{
    int N; // number of individuals
    int K; // number of possible careers
    int career[N]; // outcome
    vector[K] career_income;
}
parameters{
    vector[K-1] a; // intercepts
    real<lower=0> b; // association of income with choice
}
model{
    vector[K] p;
    vector[K] s;
    a ~ normal( 0 , 1 );
    b ~ normal( 0 , 0.5 );
    s[1] = a[1] + b*career_income[1];
    s[2] = a[2] + b*career_income[2];
    s[3] = 0; // pivot
    p = softmax( s );
    career ~ categorical( p );
}
"

## R code 11.57
dat_list <- list(N = N, K = 3, career = career, career_income = income)
m11.13 <- stan(model_code = code_m11.13, data = dat_list, chains = 4)
precis(m11.13, 2)

## R code 11.58
post <- extract.samples(m11.13)

# set up logit scores
s1 <- with(post, a[, 1] + b * income[1])
s2_orig <- with(post, a[, 2] + b * income[2])
s2_new <- with(post, a[, 2] + b * income[2] * 2)

# compute probabilities for original and counterfactual
p_orig <- sapply(1:length(post$b), function(i) {
  softmax(c(s1[i], s2_orig[i], 0))
})
p_new <- sapply(1:length(post$b), function(i) {
  softmax(c(s1[i], s2_new[i], 0))
})

# summarize
p_diff <- p_new[2, ] - p_orig[2, ]
precis(p_diff)

## R code 11.59
N <- 500
# simulate family incomes for each individual
family_income <- runif(N)
# assign a unique coefficient for each type of event
b <- c(-2, 0, 2)
career <- rep(NA, N) # empty vector of choices for each individual
for (i in 1:N) {
  score <- 0.5 * (1:3) + b * family_income[i]
  p <- softmax(score[1], score[2], score[3])
  career[i] <- sample(1:3, size = 1, prob = p)
}

code_m11.14 <- "
data{
    int N; // number of observations
    int K; // number of outcome values
    int career[N]; // outcome
    real family_income[N];
}
parameters{
    vector[K-1] a; // intercepts
    vector[K-1] b; // coefficients on family income
}
model{
    vector[K] p;
    vector[K] s;
    a ~ normal(0,1.5);
    b ~ normal(0,1);
    for ( i in 1:N ) {
        for ( j in 1:(K-1) ) s[j] = a[j] + b[j]*family_income[i];
        s[K] = 0; // the pivot
        p = softmax( s );
        career[i] ~ categorical( p );
    }
}
"

dat_list <- list(N = N, K = 3, career = career, family_income = family_income)
m11.14 <- stan(model_code = code_m11.14, data = dat_list, chains = 4)
precis(m11.14, 2)

## R code 11.60
library(rethinking)
data(UCBadmit)
d <- UCBadmit

## R code 11.61
# binomial model of overall admission probability
m_binom <- quap(
  alist(
    admit ~ dbinom(applications, p),
    logit(p) <- a,
    a ~ dnorm(0, 1.5)
  ),
  data = d
)

# Poisson model of overall admission rate and rejection rate
# 'reject' is a reserved word in Stan, cannot use as variable name
dat <- list(admit = d$admit, rej = d$reject)
m_pois <- ulam(
  alist(
    admit ~ dpois(lambda1),
    rej ~ dpois(lambda2),
    log(lambda1) <- a1,
    log(lambda2) <- a2,
    c(a1, a2) ~ dnorm(0, 1.5)
  ),
  data = dat, chains = 3, cores = 3
)

## R code 11.62
inv_logit(coef(m_binom))

## R code 11.63
k <- coef(m_pois)
a1 <- k["a1"]
a2 <- k["a2"]
exp(a1) / (exp(a1) + exp(a2))
