library(rethinking)

data(reedfrogs)
rf_df <- reedfrogs
rf_df$tank <- 1:nrow(rf_df)

rf_dat <- list(
  S = rf_df$surv,
  N = rf_df$density,
  tank = rf_df$tank
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
  data = rf_dat, chains = 4, cores = 4, log_lik = TRUE
)
