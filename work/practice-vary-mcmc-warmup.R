library(rethinking)
source("iplot.R")

display_markdown("
**9M3.** Re-estimate one of the Stan models from the chapter, but at different numbers of `warmup`
iterations. Be sure to use the same number of sampling iterations in each case. Compare the `n_eff`
values. How much warmup is enough?

**Answer.** For the rugged models, we can get down to 200 warmup iterations and still get nearly as
many effective samples as requested samples.
")

source("load-slim-rugged.R")

likelihood <- alist(
  log_gdp_std ~ dnorm(mu, sigma),
  mu <- a[cid] + b[cid] * (rugged_std - 0.215),
  a[cid] ~ dnorm(1, 0.1),
  b[cid] ~ dnorm(0, 0.3),
  sigma ~ dexp(1)
)

samples <- 1000
warmup <- 1000
m_rugged_warmup_A <- ulam(
  likelihood,
  data = dat_slim,
  iter = warmup + samples,
  warmup = warmup,
  chains = 4,
  cores = parallel::detectCores()
)

display_markdown(sprintf("<br/> Warmup: %d, Samples: %d", warmup, samples))
display(m_rugged_warmup_A)
display_markdown("<br/> Effective samples:")
display(precis(m_rugged_warmup_A, depth = 2), mimetypes="text/plain")

samples <- 1000
warmup <- 500
m_rugged_warmup_B <- ulam(
  likelihood,
  data = dat_slim,
  iter = warmup + samples,
  warmup = warmup,
  chains = 4,
  cores = parallel::detectCores()
)

display_markdown(sprintf("<br/> Warmup: %d, Samples: %d", warmup, samples))
display(m_rugged_warmup_B)
display_markdown("<br/> Effective samples:")
display(precis(m_rugged_warmup_B, depth = 2), mimetypes="text/plain")

samples <- 1000
warmup <- 200
m_rugged_warmup_C <- ulam(
  likelihood,
  data = dat_slim,
  iter = warmup + samples,
  warmup = warmup,
  chains = 4,
  cores = parallel::detectCores()
)

display_markdown(sprintf("<br/> Warmup: %d, Samples: %d", warmup, samples))
display(m_rugged_warmup_C)
display_markdown("<br/> Effective samples:")
display(precis(m_rugged_warmup_C, depth = 2), mimetypes="text/plain")
