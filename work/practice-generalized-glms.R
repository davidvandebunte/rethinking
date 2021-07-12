source('iplot.R')
library(rethinking)

display_markdown("
[bt]: https://en.wikipedia.org/wiki/Blood_type
[cv]: https://en.wikipedia.org/wiki/Categorical_variable
[ls]: https://en.wikipedia.org/wiki/Likert_scale
[od]: https://en.wikipedia.org/wiki/Ordinal_data

**12E1.** What is the difference between an ordered categorical variable and an unordered one?
Define and then give an example of each.

**Answer.** An ordered categorical variable is one in which the categories are ranked, but the
distance between the categories is not necessarily constant. An example of an ordered categorical
variable is the [Likert scale][ls]; an example of an unordered categorical variable is the [Blood
type][bt] of a person.

See also [Categorical variable][cv] and [Ordinal data][od].

**12E2.** What kind of link function does an ordered logistic regression employ? How does it differ
from an ordinary logit link?

**Answer.** It employs a cumulative link, which maps the probability of a category or any category
of lower rank (the cumulative probability) to the log-cumulative-odds scale. The major difference
from the ordinary logit link is the type of probabilties being mapped; the cumulative link maps
cumulative probabilities to log-cumulative-odds and the ordinary logit link maps absolute
probabilities to log-odds.

**12E3.** When count data are zero-inflated, using a model that ignores zero-inflation will tend to
induce which kind of inferential error?

**Answer.** A 'zero' means that nothing happened; nothing can happen either because the rate of
events is low because the process that generates events failed to get started. We may incorrectly
infer the rate of events is lower than it is in reality, when in fact it never got started.
")

display_markdown("
[ovd]: https://en.wikipedia.org/wiki/Overdispersion

**12E4.** Over-dispersion is common in count data. Give an example of a natural process that might
produce over-dispersed counts. Can you also give an example of a process that might produce
under-dispersed counts?

**Answer.** The sex ratios of families actually skew toward either all boys or girls; see [Overdispersion][ovd].

In contexts where the waiting time for a service is bounded (e.g. a store is only open a short time)
but the model still uses a Poisson distribution you may see underdispersed counts because large
values are simply not possible. If one queue feeds into another, and the first queue has a limited
number of workers, the waiting times for the second queue may be underdispersed because of the
homogeneity enforced by the bottleneck in the first queue.
")

display_markdown("
**12M1.** At a certain university, employees are annually rated from 1 to 4 on their productivity,
with 1 being least productive and 4 most productive. In a certain department at this certain
university in a certain year, the numbers of employees receiving each rating were (from 1 to 4): 12,
36, 7, 41. Compute the log cumulative odds of each rating.

**Answer.** As a list:
")

freq_prod <- c(12, 36, 7, 41)
pr_k <- freq_prod / sum(freq_prod)
cum_pr_k <- cumsum(pr_k)
lco = list(log_cum_odds = logit(cum_pr_k))
display(lco)

display_markdown("
**12M2.** Make a version of Figure 12.5 for the employee ratings data given just above.
")

iplot(function() {
  off <- 0.04
  plot(
    1:4, cum_pr_k, type="b",
    main="Version of Figure 12.5 for Employee Ratings Data", xlab="response", ylab="cumulative proportion",
    ylim=c(0,1)
  )
  # See `simplehist`
  for (j in 1:4) lines(c(j,j), c(0,cum_pr_k[j]), lwd=3, col="gray")
  for (j in 1:4) lines(c(j,j)+off, c(if (j==1) 0 else cum_pr_k[j-1],cum_pr_k[j]), lwd=3, col=rangi2)
})

display_markdown(r"(
**12M3.** Can you modify the derivation of the zero-inflated Poisson distribution (ZIPoisson) from
the chapter to construct a zero-inflated binomial distribution?

**Answer.** Call $p_z$ the additional probability of a zero, and $p_b$ the probability of success
associated with the binomial distribution. The likelihood of observing a zero is:

$$
\begin{align}
Pr(0 | p_z,n,p_b) & = p_z + (1-p_z)Pr(0|n,p_b) \\
& = p_z + (1-p_z)\binom{n}{0}p_b^0(1-p_b)^{n-0} \\
& = p_z + (1-p_z)(1-p_b)^n
\end{align}
$$

The likelihood of observing a non-zero value $k$ is:
$$
\begin{align}
Pr(k | k>0,p_z,n,p_b) & = (1-p_z)Pr(k|n,p_b) \\
& = (1-p_z)\binom{n}{k}p_b^k(1-p_b)^{n-k}
\end{align}
$$

To construct a GLM, use a logit link with $p_z$ and $p_b$ (in the typical case where $n$ is known).
)")

source('practice-hurricanes-poisson.R')
source('practice-hurricanes-gamma-poisson.R')
source('practice-hurricanes-interaction.R')
source('practice-hurricanes-log-damage.R')
source('practice-women-care-contact.R')
source('practice-fish.R')
source('practice-trolley-education.R')
source('practice-trolley-gender.R')
