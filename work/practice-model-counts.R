source("iplot.R")

display_markdown(r"(

[odds]: https://en.wikipedia.org/wiki/Odds

**11E1.** If an event has probability 0.35, what are the log-odds of this event?

**Answer.** The odds are $0.35 / (1 - 0.35) = 0.35/0.65$ or about a half. The
odds ([Odds][odds]) in favor of the event are about one to two. On the `log2`
scale we should expect the log-odds to be about negative one:

$$
log2\left(\frac{p}{1-p}\right) = log2(0.35 / (1 - 0.35)) = -0.893
$$

For a logarithmic base of `e` contract the log scale by `log(2)` or about 0.7:

$$
log\left(\frac{p}{1-p}\right) = log(0.35 / (1 - 0.35)) = -0.619
$$
)")

display_markdown(r"(
**11E2.** If an event has log-odds 3.2, what is the probability of the event?

**Answer.** Expect a log2-odds of about 3.2/0.7 or around 4.6. That is, the
odds in favor of the event should be about 2**4.6 to one, perhaps around 25:1.

$$
\frac{1}{1+e^{-x}} = \frac{1}{1+e^{-3.2}} = 0.961
$$
)")

display_markdown(r"(
**11E3.** Suppose that a coefficient in a logistic regression has value 1.7.
What does this imply about the proportional change in odds of the outcome?

**Answer.** The 'proportional odds' is about five:

$$
q = e^{\beta} = e^{1.7} = 5.47
$$

The 'proportional odds' is the change in odds for a unit increase in the predictor variable. So when
the predictor variable increases by one, we can expect the odds $p/(1-p)$ to increase by about a
factor of five.
)")

display_markdown("
**11E4.** Why do Poisson regressions sometimes require the use of an *offset*? Provide an example.

**Answer.** We use an offset in a Poisson regression (see section **11.2.3**) when we have counts
that were collected at varying densities. For example, counts collected over varying:
- Lengths of observation
- Area of sampling
- Intensity of sampling

The offset lets us include all the data in the same model while still accounting for the differences
in measurement.

A specific example would be collecting the number of visitors to a restaurant. Some counts may be
collected on a daily basis, and others on a weekly basis.
")

display_markdown("
**11M1.** As explained in the chapter, binomial data can be organized in aggregated and
disaggregated forms, without any impact on inference. But the likelihood of the data does change
when the data are converted between the two formats. Can you explain why?

**Answer.** The likelihood changes when you move from the disaggregated to aggregated format because
now there is no longer exactly one trial associated with every event.
")

display_markdown(r"(
**11M2.** If a coefficient in a Poisson regression has value 1.7, what does this imply about the
change in outcome?

**Answer.** Assuming the Poisson regression uses a log link, for a unit increase in the predictor
variable we should see an increase in the rate (or mean) $\lambda$ of about five:

$$
q = \frac{exp(\alpha + \beta (x + 1))}{exp(\alpha + \beta x)} = exp(\beta) = exp(1.7) \approx 5.47
$$
)")

display_markdown(r"(
**11M3.** Explain why the logit link is appropriate for a binomial generalized linear model.

**Answer.** The binomial distribution takes a probability parameter (between zero and one) and the
logit link constrains a parameter to this range.
)")

display_markdown(r"(
**11M4.** Explain why the log link is appropriate for a Poisson generalized linear model.

**Answer.** The Poisson distribution takes a positive real-valued parameter and the log link
constrains a parameter to this range.
)")

display_markdown(r"(
**11M5.** What would it imply to use a logit link for the mean of a Poisson generalized linear model?
Can you think of a real research problem for which this would make sense?

**Answer.** It would imply the mean $\lambda$ is between zero and one, rather than simply positive
and real-valued. If the researcher knows the maximum number of times (e.g. one) an event can occur,
it seems like it would be more appropriate to use the binomial distribution in many cases and infer
`p` instead.
)")

display_markdown("
**11M6.** State the constraints for which the binomial and Poisson distributions have maximum
entropy. Are the constraints different at all for binomial and Poisson? Why or why not?

[mepd]: https://en.wikipedia.org/wiki/Maximum_entropy_probability_distribution#Other_examples
[pa]: https://en.wikipedia.org/wiki/Binomial_distribution#Poisson_approximation

**Answer.** The maximum entropy constraint for both these distributions is that the expected value
is a constant. See [Maximum entropy probability distribution][mepd].

These constraints are the same because the Poisson distribution is an approximation of the binomial
when `p` is small and `N` is large. See [Poisson approximation][pa].
")

display_markdown("
**11M7.** Use `quap` to construct a quadratic approximate posterior distribution for the chimpanzee
model that includes a unique intercept for each actor, `m11.4` (page 330). Compare the quadratic
approximation to the posterior distribution produced instead from MCMC. Can you explain both the
differences and the similarities between the approximate and the MCMC distributions? Relax the prior
on the actor intercepts to Normal(0, 10). Re-estimate the posterior using both `ulam` and `quap`. Do
the differences increase or decrease? Why?

**Answer.** The quadratic approximation assumes a symmetric Gaussian posterior, so it may produce
posterior inferences that are more symmetric (unlike MCMC). In this case, the algorithms produce
marginally different results.

When we move to the flat prior, `quap` struggles to produce a decent result because it is starting
with a prior that is completely non-symmetric. It may pick either the left or the right peak of the
prior to optimize from.
")

source('load-chimp-models.R')
flush.console()

m11.4_flat_prior <- ulam(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + b[treatment],
    a[actor] ~ dnorm(0, 10),
    b[treatment] ~ dnorm(0, 0.5)
  ),
  data = dat_list, chains = 4, log_lik = TRUE
)

m11.4_quap_flat_prior <- quap(
  m11.4_flat_prior@formula,
  data = dat_list
)
flush.console()

iplot(function() {
  plot(compare(m11.4, m11.4_quap, m11.4_flat_prior, m11.4_quap_flat_prior))
}, ar=3)

source('practice-kline-islands.R')

display_markdown("
**11H1.** Use WAIC to compare the chimpanzee model that includes a unique intercept for each actor,
m11.4 (page 330), to the simpler models fit in the same section. Interpret the results.

**Answer.** The last model (`m11.4`) does the best by far because it includes the `actor` predictor.
The `actor` variable is highly predictive of the output; the big story of this experiment is
handedness.
")

iplot(function() {
  plot(compare(m11.1, m11.2, m11.3, m11.4))
}, ar=3)

source('practice-count-salmon-pirating-attempts.R')
source('practice-count-salamanders.R')
source('practice-nwo-grants.R')
source('practice-count-social-learning.R')
