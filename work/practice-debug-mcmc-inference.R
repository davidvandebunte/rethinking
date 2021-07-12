source('iplot.R')
library(rethinking)

options(mc.cores = parallel::detectCores())

display_markdown("
**9E1.** Which of the following is a requirement of the simple Metropolis algorithm?
1. The parameters must be discrete.
2. The likelihood function must be Gaussian.
3. The proposal distribution must be symmetric.

**Answer.** The parameters do not need to be discrete; see the top of **9.2. Metropolis algorithms** and a
specific example of a non-discrete parameter in section **2.4.5. Markov chain Monte Carlo**.

The likelihood function does not need to be Gaussian, see the example in section **2.4.5. Markov
chain Monte Carlo**.

[mha]: https://en.wikipedia.org/wiki/Metropolis%E2%80%93Hastings_algorithm

The proposal distribution needs to be symmetric. See [Metropolis-Hastings algorithm][mha] and the
top of **9.2. Metropolis algorithms**.
")

display_markdown("
**9E2.** Gibbs sampling is more efficient than the Metropolis algorithm. How does it achieve this
extra efficiency? Are there any limitations to the Gibbs sampling strategy?

**Answer.** Gibbs sampling allows asymmetric proposals, which it takes advantage of by introducing
adaptive proposals. An adaptive proposal is one in which the distribution of proposed parameter
values adjusts intelligently. To achieve adaptive proposals it relies on conjugate priors.

One limitation to this approach is that some conjugate priors are pathological in shape. A second
limitation is that these conjugate-prior based adaptive proposals don't perform as well as HMC
adaptive proposals when there are a large number of dimensions.
")

display_markdown("
**9E3.** Which sort of parameters can Hamiltonian Monte Carlo not handle? Can you explain why?

**Answer.** HMC cannot handle discrete parameters. I'd guess this is because these parameters would
not be differentiable. If you're running a physics simulation, you need to be able to stop at any
point.
")

display_markdown("
**9E4.** Explain the difference between the effective number of samples, n_eff as calculated by
Stan, and the actual number of samples.

[mcmc]: https://en.wikipedia.org/wiki/Markov_chain_Monte_Carlo

**Answer.** All MCMC algorithms (Metropolis, Gibbs, HMC) naturally produce autocorrelated samples;
see [Markov chain Monte Carlo][mcmc]. The 'Leapfrog steps' and 'Step size' settings as described in
the text help avoid this autocorrelation. The effective number of parameters reported by Stan is an
estimate of the length of a Markov chain with no autocorrelation that would provide the same quality
of estimate as your chain.
")

display_markdown("
**9E5.** Which value should Rhat approach, when a chain is sampling the posterior distribution
correctly?

**Answer.** 1.00 from above.
")

display_markdown("
**9E6.** Sketch a good trace plot for a Markov chain, one that is effectively sampling from the
posterior distribution. What is good about its shape? Then sketch a trace plot for a malfunctioning
Markov chain. What about its shape indicates malfunction?

[sta]: https://en.wikipedia.org/wiki/Stationary_process

**Answer.** The good features of the traces in the upper plot are [Stationarity][sta], good mixing
to e.g. avoid autocorrelated samples, and that all three chains stick to the same regions.

The bad features of the lower plot are just the opposite.
")

display_svg(file="good_trace_plot.svg", width=20, height=20)

display_markdown("
**9E7.** Repeat the problem above, but now for a trace rank plot.

**Answer.** The good feature of the top plot is that all three chains have about the same rank
histogram; they are exploring the same space.

The bad feature of the lower plot is that different chains are spending different amounts of time on
the same sample values.
")

display_svg(file="good_trank_plot.svg", width=20, height=20)

display_markdown("
**9M1.** Re-estimate the terrain ruggedness model from the chapter, but now using a uniform prior
for the standard deviation, `sigma`. The uniform prior should be `dunif(0,1)`. Use `ulam` to
estimate the posterior. Does the different prior have any detectible influence on the posterior
distribution of `sigma`? Why or why not?

**Answer.** This change could have potentially have had an effect on the inference for `sigma`
because it assigns zero prior probability to parameter values above `1.0`. Because `sigma` is less
than one, it doesn't make a difference.
")

source('load-slim-rugged.R')

m_rugged_unif_sigma <- ulam(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid] * (rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dunif(0,1)
  ),
  data = dat_slim, chains = 4, cores = parallel::detectCores()
)

iplot(function() {
  plot(precis(m_rugged_unif_sigma, depth=2), main="m_rugged_unif_sigma")
}, ar=4.0)

display_markdown("
**9M2.** Modify the terrain ruggedness model again. This time, change the prior for `b[cid]` to
`dexp(0.3)`. What does this do to the posterior distribution? Can you explain it?

**Answer.** Notice `b[2]` is now positive, when previously it was inferred to be negative. Because
we assigned zero prior probability to negative values, we're forcing this new inference.

You may also see the warning message:

```
Warning message:
“Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
Running the chains for more iterations may help. See
http://mc-stan.org/misc/warnings.html#tail-ess”
```
")

m_rugged_exp_b <- ulam(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid] * (rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dexp(0.3),
    sigma ~ dexp(1)
  ),
  data = dat_slim, chains = 4, cores = parallel::detectCores()
)

iplot(function() {
  plot(precis(m_rugged_exp_b, depth=2), main="m_rugged_exp_b")
}, ar=4.0)

source('practice-vary-mcmc-warmup.R')
source('practice-cauchy-prior.R')
source('practice-ulam-divorce-rate.R')
source('practice-leg-prior.R')
source('practice-metropolis-islands.R')
source('practice-metropolis-globe-tossing.R')
source('practice-hmc-globe-tossing.R')
