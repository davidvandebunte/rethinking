library(rethinking)
library(IRdisplay)
source("iplot.R")

display_markdown(r"(
**13M5.** Modify the cross-classified chimpanzees model `m13.4` so that the adaptive prior for
blocks contains a parameter $\gamma$ for its mean:

$$
\begin{align}
\gamma_j & \sim Normal(\bar{\gamma}, \sigma_{\gamma}) \\
\bar{\gamma} & \sim Normal(0, 1.5)
\end{align}
$$

Compare this model to `m13.4`. What has including $\gamma$ done?

**Answer.** First, let's reproduce the relevant plots from the chapter:
)")

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

display(precis(m13.4, depth = 2), mimetypes="text/plain")
iplot(function() {
  plot(precis(m13.4, depth=2), main='m13.4')
}, ar=2.0)

display_markdown(r"(
When we add the new parameter, the number of divergent transitions increase:
)")

m13.4_extra_param <- ulam(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + g[block_id] + b[treatment],
    b[treatment] ~ dnorm(0, 0.5),
    ## adaptive priors
    a[actor] ~ dnorm(a_bar, sigma_a),
    g[block_id] ~ dnorm(g_bar, sigma_g),
    ## hyper-priors
    a_bar ~ dnorm(0, 1.5),
    g_bar ~ dnorm(0, 1.5),
    sigma_a ~ dexp(1),
    sigma_g ~ dexp(1)
  ),
  data = dat_list, chains = 4, cores = 4, log_lik = TRUE
)

display_markdown(r"(
<br/>
We also see many HDPI increase:
)")

display(precis(m13.4_extra_param, depth = 2), mimetypes="text/plain")
iplot(function() {
  plot(precis(m13.4_extra_param, depth=2), main='m13.4_extra_param')
}, ar=2.0)

display_markdown(r"(
Notice the previously specified prior for the new parameter $\bar{\gamma}$ did not change
significantly in the posterior. Let's change the prior to:
$$
\bar{\gamma} \sim Normal(5, 4)
$$

With this change, we can come up with significantly different inferences:
)")

m13.4_extra_param_new_prior <- ulam(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + g[block_id] + b[treatment],
    b[treatment] ~ dnorm(0, 0.5),
    ## adaptive priors
    a[actor] ~ dnorm(a_bar, sigma_a),
    g[block_id] ~ dnorm(g_bar, sigma_g),
    ## hyper-priors
    a_bar ~ dnorm(0, 1.5),
    g_bar ~ dnorm(5, 1.5),
    sigma_a ~ dexp(1),
    sigma_g ~ dexp(1)
  ),
  data = dat_list, chains = 4, cores = 4, log_lik = TRUE
)

display(precis(m13.4_extra_param_new_prior, depth = 2), mimetypes="text/plain")
iplot(function() {
  plot(precis(m13.4_extra_param_new_prior, depth=2), main='m13.4_extra_param_new_prior')
}, ar=2.0)

display_markdown(r"(
All these symptoms are indicative of a non-identifiable parameter, as discussed in the chapter at
the start of section **13.3.1**. We encountered the same issue in question **13M1**.
)")
