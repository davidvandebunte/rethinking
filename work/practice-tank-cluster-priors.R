library(rethinking)
source("iplot.R")

display_markdown(r"(
**13M3.** Re-estimate the basic Reed frog varying intercept model, but now using a Cauchy
distribution in place of the Gaussian distribution for the varying intercepts. That is, fit this
model:

$$
\begin{align}
S_i & \sim Binomial(N_i,p_i) \\
logit(p_i) & = \alpha_{tank[i]} \\
\alpha_{tank} & \sim Cauchy(\alpha, \sigma), tank = 1..48 \\
\alpha & \sim Normal(0, 1) \\
\sigma & \sim Exponential(1) \\
\end{align}
$$

(You are likely to see many divergent transitions for this model. Can you figure out why? Can you
fix them?) Compare the posterior means of the intercepts, $\alpha_{tank}$, to the posterior means
produced in the chapter, using the customary Gaussian prior. Can you explain the pattern of
differences? Take note of any change in the mean $\alpha$ as well.

**Answer.** First, let's reproduce some of the plots from the chapter. Similar to the approach in
the `R code 13.22` box and elsewhere, we'll print the raw output of `precis` for a model before its
plots:
)")

source('load-reed-frog-model.R')

plot_means <- function(post, plot_main) {
  # compute mean intercept for each tank
  # also transform to probability with logistic
  rf_df$propsurv.est <- logistic(apply(post$a, 2, mean))

  iplot(function() {
    # display raw proportions surviving in each tank
    plot(rf_df$propsurv,
      ylim = c(0, 1), pch = 16, xaxt = "n",
      xlab = "tank", ylab = "proportion survival", col = rangi2,
      main=plot_main
    )
    axis(1, at = c(1, 16, 32, 48), labels = c(1, 16, 32, 48))

    # overlay posterior means
    points(rf_df$propsurv.est)

    # mark posterior mean probability across tanks
    abline(h = mean(inv_logit(post$a_bar)), lty = 2)

    # draw vertical dividers between tank densities
    abline(v = 16.5, lwd = 0.5)
    abline(v = 32.5, lwd = 0.5)
    text(8, 0, "small tanks")
    text(16 + 8, 0, "medium tanks")
    text(32 + 8, 0, "large tanks")
  })
}

display(precis(m13.2, depth = 2), mimetypes="text/plain")
iplot(function() {
  plot(precis(m13.2, depth=2), main='m13.2')
}, ar=1.0)
post <- extract.samples(m13.2)
plot_means(post, "m13.2")

display_markdown(r"(
As promised, sampling from the model produces divergent transitions:
)")

m_tank_cauchy_orig <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a[tank],
    a[tank] ~ dcauchy(a_bar, sigma),
    a_bar ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = rf_dat, chains = 4, cores = 4, log_lik = TRUE
)

display_markdown(r"(
<br/>
Adjusting `adapt_delta` does little to reduce the number of divergent transitions:
)")

m_tank_cauchy <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a[tank],
    a[tank] ~ dcauchy(a_bar, sigma),
    a_bar ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = rf_dat, chains = 4, cores = 4, log_lik = TRUE, control=list(adapt_delta=0.99)
)

display_markdown(r"(
<br/>
Let's examine the `pairs()` plot suggested in the warning message (for only a few parameters):
)")

sel_pars = c("a_bar", "sigma", "a[41]", "a[38]")
iplot(function() {
  pairs(m_tank_cauchy@stanfit, pars=sel_pars)
})
iplot(function() {
  traceplot(m_tank_cauchy, pars=sel_pars)
}, ar=2)

display_markdown(r"(
<br/>
For comparison, these are the same plots for the Devil's Funnel (i.e. Neal's Funnel):
)")

m13.7 <- ulam(
  alist(
    v ~ normal(0, 3),
    x ~ normal(0, exp(v))
  ),
  data = list(N = 1), chains = 4
)
display(precis(m13.7), mimetypes="text/plain")
iplot(function() {
  plot(precis(m13.7), main='m13.7')
}, ar=4.5)

display_markdown(r"(
This model produces warnings even producing the `pairs()` plot:
)")
iplot(function() {
  pairs(m13.7@stanfit)
})
iplot(function() {
  traceplot(m13.7)
}, ar=2)

display_markdown(r"(
<br/>
[qf]: https://en.wikipedia.org/wiki/Quantile_function
[nd]: https://en.wikipedia.org/wiki/Normal_distribution
[cd]: https://en.wikipedia.org/wiki/Cauchy_distribution

Unlike the `pairs()` plot from the Funnel, the divergent transitions produced by the Cauchy
distribution are not associated with steep contours.

Let's at least attempt to reparameterize the model to confirm whether or not it will help. One way
to think about reparameterizing a model is as factoring the quantile function. As explained in
[Quantile function][qf], a sample from a given distribution may be obtained in principle by applying
its quantile function to a sample from a uniform distribution. The quantile function for the [Normal
distribution][nd] is:

$$
Q(p, \mu, \sigma) = \mu + \sigma \sqrt{2} \cdot erf^{-1} (2p - 1)
$$

So for the standard normal:
$$
Q_s(p) = \sqrt{2} \cdot erf^{-1} (2p - 1)
$$

Imagining `p` comes from a uniform distribution in both cases, we can write:
$$
Q(p, \mu, \sigma) = \mu + \sigma Q_s(p)
$$

Starting from the quantile function for the [Cauchy distribution][cd]:
$$
Q(p, x_0, \gamma) = x_0 + \gamma \tan[\pi(p - \frac{1}{2})]
$$

Define the standard Cauchy distribution quantile function as:
$$
Q_s(p) = \tan[\pi(p - \frac{1}{2})]
$$

So that:
$$
Q(p, x_0, \gamma) = x_0 + \gamma Q_s(p)
$$

Finally, define a new non-centered model:
$$
\begin{align}
S_i & \sim Binomial(N_i,p_i) \\
logit(p_i) & = \alpha + \sigma \cdot a_{tank[i]} \\
a_{tank} & \sim Cauchy(0, 1), tank = 1..48 \\
\alpha & \sim Normal(0, 1) \\
\sigma & \sim Exponential(1) \\
\end{align}
$$

Unfortunately, this model produces the same divergent transitions:
)")

m_tank_noncen_cauchy <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a_bar + sigma * std_c[tank],
    std_c[tank] ~ dcauchy(0, 1),
    a_bar ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = rf_dat, chains = 4, cores = 4, log_lik = TRUE
)

display_markdown(r"(
What's more likely going on here is that the energy sanity checks in MCMC are too tight for the
extreme deviates produced by the Cauchy distribution. Consider the quantile function for the Cauchy
distribution above, defined based on the trigonometric `tan` function. This definition leads to
extreme negative or positive deviates when `p` is near either zero or one; notice this term
approaches negative infinity as `p` approaches zero and positive infinity as it approaches one.

Notice in the `pairs()` plot above, the trace plot, and in the number of effective samples that
larger parameters like `a[38]` are harder to sample than `a[41]`. Part of the reason for this may
be that these samples would get rejected as divergent transitions, even when we rarely happen to
sample from this part of the posterior.

In the posterior distributions parameter `a[38]` is much less certain than `a[41]`. In the original
and Cauchy model, parameter 38 is inferred to be larger than 41. All large parameters are going to
be more uncertain with the Cauchy distribution and in fact for all distributions with a thick tail;
getting a sample of a large value implies a rather large deviate from the distribution and a
specific large deviate is relatively unlikely relative to other large deviates (the long tail is
relatively flat). Additionally, a large observation could be explained by applying a larger range of
parameters to a distribution because every parameterization has large tails that allow for the
observation. The same observation could be made of the Student-t inferences in the next question.
The increase in uncertainty for larger parameters exists even in model `m13.2` but is much more
significant for thick-tailed distributions.
)")

display(precis(m_tank_cauchy, depth = 2), mimetypes="text/plain")
iplot(function() {
  plot(precis(m_tank_cauchy, depth=2), main='m_tank_cauchy')
}, ar=1.0)

display_markdown(r"(
The same trends in shrinkage exist in this model as the original model. Because of the thicker
tails, the model doesn't apply as much shrinkage to extreme observations. Because of the difficulty
in sampling, the shrinkage is more variable.
)")

post <- extract.samples(m_tank_cauchy)
plot_means(post, "m_tank_cauchy")

display_markdown(r"(
**13M4.** Now use a Student-t distribution with $\nu = 2$ for the intercepts:

$$
\alpha_{tank} \sim Student(2, \alpha, \sigma)
$$

Refer back to the Student-t example in Chapter 7 (page 234), if necessary. Compare the resulting
posterior to both the original model and the Cauchy model in 13M3. Can you explain the differences
and similarities in shrinkage in terms of the properties of these distributions?

**Answer.** This model produces some but fewer divergent transitions, likely because of deviates
coming from the thick tails:
)")

m_tank_student_t <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a[tank],
    a[tank] ~ dstudent(2, a_bar, sigma),
    a_bar ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = rf_dat, chains = 4, cores = 4, log_lik = TRUE
)

display_markdown(r"(
Although we won't address these divergent transitions, let's at least check how much mixing is
occurring:
)")

iplot(function() {
  pairs(m_tank_cauchy@stanfit, pars=sel_pars)
})
iplot(function() {
  traceplot(m_tank_cauchy, pars=sel_pars)
}, ar=2)

display(precis(m_tank_student_t, depth = 2), mimetypes="text/plain")
iplot(function() {
  plot(precis(m_tank_student_t, depth=2), main='m_tank_student_t')
}, ar=1.0)

display_markdown(r"(
Similar trends in shrinkage exist in this model. Because of the thicker tails, the model doesn't
apply as much shrinkage to extreme observations. Improved sampling relative to the Cauchy
distribution seems to have produced slightly more consistent shrinkage.
)")

post <- extract.samples(m_tank_student_t)
plot_means(post, "m_tank_student_t")
