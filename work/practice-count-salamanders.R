source("iplot.R")
library(rethinking)

display_markdown("
[gc]: https://en.wikipedia.org/wiki/Groundcover

**11H4.** The data contained in `data(salamanders)` are counts of salamanders (*Plethodon
elongatus*) from 47 different 49-m² plots in northern California. The column `SALAMAN` is the count in
each plot, and the columns `PCTCOVER` and `FORESTAGE` are percent of [ground cover][gc] and age of trees
in the plot, respectively. You will model `SALAMAN` as a Poisson variable.

(a) Model the relationship between density and percent cover, using a log-link (same as the example
in the book and lecture). Use weakly informative priors of your choosing. Check the quadratic
approximation again, by comparing `quap` to `ulam`. Then plot the expected counts and their 89%
interval against percent cover. In which ways does the model do a good job? In which ways does it do
a bad job?

**Answer.** The `salamanders` data frame:
")

data(salamanders)
display(salamanders)
sal <- salamanders
sal$StdPctCover <- scale(sal$PCTCOVER)

sal_dat <- list(
  S = sal$SALAMAN,
  StdPctCover = sal$StdPctCover
)

mq_sal <- quap(
  alist(
    S ~ dpois(lambda),
    log(lambda) <- a + b*StdPctCover,
    a ~ dnorm(3, 0.5),
    b ~ dnorm(0, 0.2)
  ), data = sal_dat
)

mu_sal <- ulam(
  mq_sal@formula,
  data = sal_dat, chains=4, cores=4, log_lik=TRUE
)

display_markdown("
There doesn't seem to be a difference between `quap` and `ulam` in inference:
")

iplot(function() {
  plot(compare(mu_sal, mq_sal))
}, ar=4.5)

pctc <- list(StdPctCover=scale(seq(0, 100)))
sims <- sim(mq_sal, data=pctc)

expectedSalCount.mu <- apply(sims, 2, mean)
expectedSalCount.PI <- apply(sims, 2, PI)
display_markdown("The raw data:")
display(expectedSalCount.mu)
display(expectedSalCount.PI)

iplot(function() {
  plot(pctc$StdPctCover, expectedSalCount.mu, ylim=c(0,12))
  shade(expectedSalCount.PI, pctc$StdPctCover)
  title("Expected salamander count")
})

display_markdown("
The model does well (is more certain) when it needs to guess lower numbers; when there are fewer
salamanders when the ground cover is low. In general, though, it is uncertain about most of its
predictions.
")

display_markdown("
(b) Can you improve the model by using the other predictor, `FORESTAGE`? Try any models you think
useful. Can you explain why `FORESTAGE` helps or does not help with prediction?

**Answer.** It doesn't seem forest age helps much with prediction; it may be that forest age only
predicts ground cover and so adds no extra information.
")

sal$StdForestAge <- scale(sal$FORESTAGE)
sal_dat <- list(
  S = sal$SALAMAN,
  StdPctCover = sal$StdPctCover,
  StdForestAge = sal$StdForestAge
)

mq_sal_forest_age <- quap(
  alist(
    S ~ dpois(lambda),
    log(lambda) <- a + b_c*StdPctCover + b_f*StdForestAge,
    a ~ dnorm(3, 0.5),
    b_c ~ dnorm(0, 0.2),
    b_f ~ dnorm(0, 0.2)
  ), data = sal_dat
)

iplot(function() {
  plot(compare(mq_sal, mq_sal_forest_age))
}, ar=4.5)
