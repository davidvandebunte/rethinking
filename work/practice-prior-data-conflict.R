library(rethinking)
library(IRdisplay)
source("iplot.R")

display_markdown(r"(
**ERROR:**
> Likewise, the tails of distributions strongly influence can (sic) outliers are shrunk or not
towards the mean.

Likewise, the tails of distributions strongly influence whether outliers are shrunk towards the
mean.

**13M6.** Sometimes the prior and the data (through the likelihood) are in conflict, because they
concentrate around different regions of parameter space. What happens in these cases depends a lot
upon the shape of the tails of the distributions. Likewise, the tails of distributions strongly
influence can outliers are shrunk or not towards the mean. I want you to consider four different
models to fit to one observation at $y = 0$. The models differ only in the distributions assigned to
the likelihood and the prior. Here are the four models:

Model NN:
$$
\begin{align}
y & \sim Normal(\mu, 1) \\
\mu & \sim Normal(10, 1)
\end{align}
$$

Model TN:
$$
\begin{align}
y & \sim Student(2, \mu, 1) \\
\mu & \sim Normal(10, 1)
\end{align}
$$

Model NT:
$$
\begin{align}
y & \sim Normal(\mu, 1) \\
\mu & \sim Student(2, 10, 1)
\end{align}
$$

Model TT:
$$
\begin{align}
y & \sim Student(2, \mu, 1) \\
\mu & \sim Student(2, 10, 1)
\end{align}
$$

Estimate the posterior distributions for these models and compare them. Can you explain the results,
using the properties of the distributions?

**Answer.** Fit model NN:
)")

m_nn <- ulam(
  alist(
    y ~ dnorm(mu, 1),
    mu ~ dnorm(10, 1)
  ),
  data = list(y = 0), chains = 4
)
flush.console()

display_markdown(r"(
<br/>
Output of `precis` for model NN:
)")
display(precis(m_nn), mimetypes="text/plain")

display_markdown(r"(
<br/>
Fit model TN:
)")
m_tn <- ulam(
  alist(
    y ~ dstudent(2, mu, 1),
    mu ~ dnorm(10, 1)
  ),
  data = list(y = 0), chains = 4
)
flush.console()

display_markdown(r"(
<br/>
Output of `precis` for model TN:
)")
display(precis(m_tn), mimetypes="text/plain")

display_markdown(r"(
<br/>
Fit model NT:
)")
m_nt <- ulam(
  alist(
    y ~ dnorm(mu, 1),
    mu ~ dstudent(2, 10, 1)
  ),
  data = list(y = 0), chains = 4
)
flush.console()

display_markdown(r"(
<br/>
Output of `precis` for model NT:
)")
display(precis(m_nt), mimetypes="text/plain")

display_markdown(r"(
<br/>
Fit model NN:
)")
m_tt <- ulam(
  alist(
    y ~ dstudent(2, mu, 1),
    mu ~ dstudent(2, 10, 1)
  ),
  data = list(y = 0), chains = 4
)
flush.console()

display_markdown(r"(
<br/>
Output of `precis` for model TT:
)")
display(precis(m_tt), mimetypes="text/plain")

display_markdown(r"(
<br/>
[rr]: https://en.wikipedia.org/wiki/Robust_regression
[rp]: https://en.wikipedia.org/wiki/Regularization_(mathematics)

These four models are best understood in terms of [Robust regression][rr] (section **7.5.2**) and
[Regularizing priors][rp] (section **7.3**).

Try not to assume a right answer in these inferences. The $y = 0$ data point is definitely an
outlier with respect to the four priors centered at $\mu = 10$, but are the priors or the data point
correct? It may be that most observations are at $y = 10$ and this sample is truly an outlier, or
that most observations are at $y = 0$ and the human building the model made a mistake in stating a
prior centered on $\mu = 10$.

Let's consider regularizing priors. The normal priors on $\mu$ in the first and second models are
much more regularizing than the Student-t priors. A regularizing prior in this and any other model
means a prior that prevents the model from getting overly excited by the data. Said another way, a
regularizing prior is a strong statement about prior beliefs that should override the data to some
extent. The term *regularizing* is generally a positive one, but a regularizing prior is more
generally a neutral concept of preference for the prior to the data. In these first two models we
see the inference for $\mu$ is generally closer to the prior than to the data. That is, $\mu \sim 5$
and $\mu \sim 10$ are closer to the prior of $\mu = 10$ than the models with less regularizing
Student-t distributions, with inferences of $\mu \sim 0$ and $\mu \sim 5$.

Equally as important to the final inferences are the likelihood functions. The Student-t likelihoods
in the second and fourth models are much more robust to outliers than the noormal likelihoods. We
say we are doing robust regression when we pick a likelihood function that is not easily surprised
by the data such as a Student-t distribution. Again, the term *robust* is generally a positive one,
but a robust regression is more generally a neutral concept of preference against getting excited by
any single data point. With one data point, this translates to preference for the prior. In the second
and fourth models we see the inference for $\mu$ is generally closer to the prior than to the data.
That is, $\mu \sim 10$ and $\mu \sim 5$ are closer to the prior of $\mu = 10$ than the models with
less robust normal likelihoods, with inferences of $\mu \sim 5$ and $\mu \sim 0$.

Interestingly, the flat prior on model TT has led to some divergent transitions. See section
**9.5.3** for how a weakly informative prior such as the normal prior in model TN could address this
issue. If you look at the `pairs()` plot and traceplot below, you'll see the sampling process can't
decide whether the data or the likelihood is correct. That is, the histogram for `mu` produces a
large number of samples at both $\mu = 0$ and $\mu = 10$. The Student-t likelihood doesn't strongly
rule out the possibility that the Student-t prior is correct, and the Student-t prior doesn't
strongly rule out the possibility that the data, through the Student-t likelihood, is correct.
)")

iplot(function() {
  pairs(m_tt@stanfit)
})
iplot(function() {
  traceplot(m_tt)
}, ar=2)
