library(rethinking)
source('iplot.R')

display_markdown("
**9H1.** Run the model below and then inspect the posterior distribution and explain what it is
accomplishing.

R code 9.28

```R
mp <- ulam(
 alist(
  a ~ dnorm(0,1),
  b ~ dcauchy(0,1)
 ), data=list(y=1) , chains=1 )
```

<br/>

Compare the samples for the parameters a and b. Can you explain the different trace plots? If you
are unfamiliar with the Cauchy distribution, you should look it up. The key feature to attend to is
that it has no expected value. Can you connect this fact to the trace plot?
")

## R code 9.28
mp <- ulam(
  alist(
    a ~ dnorm(0, 1),
    b ~ dcauchy(0, 1)
  ),
  data = list(y = 1), chains = 4, cores = 4
)

## R code 9.20
iplot(function() traceplot(mp), ar=2.8)

display_markdown("
**Answer.** We start with two priors unconnected to the data so we're essentially sampling from the
prior rather than any posterior. Said another way, the posterior equals the prior.

[sta]: https://en.wikipedia.org/wiki/Stationary_process
[cd]: https://en.wikipedia.org/wiki/Cauchy_distribution

The `a` prior is [Stationary][sta] in the common sense, with a stable mean indicated by balance
between the top and bottom of the figure. The [Cauchy process][cd] is stationary in the sense the
process statistics do not change (strictly stationary) but doesn't have a stable mean (expected
value) so doesn't have the property sometimes referred to as wide-sense stationarity.

[htd]: https://en.wikipedia.org/wiki/Heavy-tailed_distribution

Without a stable mean, there is no guarantee that the Cauchy process will be symmetric vertically;
large spikes towards low or high values are common in repeated simulations. The [Heavy tails][htd]
that define the Cauchy distribution may sometimes lead to this warning (among others):
```
Warning message:
“Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
Running the chains for more iterations may help. See
http://mc-stan.org/misc/warnings.html#tail-ess”
```

<br/>

[loln]: https://en.wikipedia.org/wiki/Law_of_large_numbers

Samples from the Cauchy process sometimes to appear to display bad 'mixing'. That is, the samples
don't appear to explore the same space as the process continues (because of the spikes). In general,
the Cauchy distribution will not obey the [Law of Large Numbers][loln], that is, there's no
guarantee of convergence of the mean. Said another way, these spikes will not go away regardless of
how many iterations we allow.

Because the median and mode are still defined, multiple chains may still appear to 'converge' to
e.g. zero in this case.
")
