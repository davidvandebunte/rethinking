library(rethinking)
library(IRdisplay)

source("iplot.R")

display_markdown("## 7.7. Practice")
display_markdown(r"(
**7E1.** State the three motivating criteria that define information entropy. Try to express each in
your own words.

**Answer.**

1. The change in an uncertainty metric (entropy) should be continuous as the distribution
probabilities change continuously.
2. An uncertainty metric should increase as the number of events increases. For example, we
intuitively expect more uncertainty from a roll of a die than from flipping a coin.
3. Our uncertainty metric (or information metric) should increase additively when two distributions
are combined.

[eit]: https://en.wikipedia.org/w/index.php?title=Entropy_(information_theory)&oldid=1017918048#Alternate_characterization
[ic]: https://en.wikipedia.org/wiki/Information_content

See also [Entropy (information theory) - Alternate characterization][eit] and [Information content][ic].
)")

display_markdown("
**7E2.** Suppose a coin is weighted such that, when it is tossed and lands on a table, it comes up heads
70% of the time. What is the entropy of this coin?")
display_markdown("**Answer.** In nats:")
p <- c(0.7, 0.3)
display(-sum(p * log(p)))
display_markdown("")

display_markdown("
**7E3.** Suppose a four-sided die is loaded such that, when tossed onto a table, it shows “1” 20%, “2”
25%, ”3” 25%, and ”4” 30% of the time. What is the entropy of this die?")
display_markdown("**Answer.** In nats:")
p <- c(0.2, 0.25, 0.25, 0.30)
display(-sum(p * log(p)))
display_markdown("")

display_markdown("
**7E4.** Suppose another four-sided die is loaded such that it never shows “4”. The other three sides
show equally often. What is the entropy of this die?")
display_markdown("**Answer.** In nats:")
p <- c(1 / 3, 1 / 3, 1 / 3)
display(-sum(p * log(p)))
display_markdown("")

display_markdown(r"(
**ERROR**: The medium questions are marked as easy (e.g. 7M1 below is marked 7E1) in VitalSource.

**7M1.** Write down and compare the definitions of AIC and WAIC. Which of these criteria is most
general? Which assumptions are required to transform the more general criterion into a less general
one?

**Answer.** The formulas:

$$
AIC = D_{train} + 2p = -2lppd + 2p = -2(lppd - p) \\
WAIC(y, \Theta) = -2\bigl(lppd - \sum_i var_\theta (log (p(y_i|\theta)))\bigr)
$$

WAIC is more general; we can transform it into AIC if we assume the penalty term i.e. effective
number of parameters is equal p.

)")
display_markdown("
**7M2.** Explain the difference between model selection and model comparison. What information is
lost under model selection?

**Answer.** In model selection, we lose information about the relative confidence we have in a model
to other models we've evaluated. The model with the best PSIS or WAIC score (best predictive score)
is not necessarily the one we want; we may be able to give up predictive accuracy for a model we
believe is causally correct if the difference in predictive accuracy is negligible.

A related consideration is that as we consider more models, we'll likely discover one (see the
section on the 'The Curse of Tippecanoe') that will fit the data well only because we've tried so
many models.
")
display_markdown("
**7M3.** When comparing models with an information criterion, why must all models be fit to exactly
the same observations? What would happen to the information criterion values, if the models were
fit to different numbers of observations? Perform some experiments, if you are not sure.

**Answer.** WAIC and AIC are based on `lppd` (log-pointwise-predictive-density) which is based on
the idea of a log-probability score. Both `lppd` and the log-probability score are based on the
number of observations; they will scale with the number of observations. The log-probability score
is an estimate of $E[log(q_i)]$ but:
> without the final step of dividing by the number of observations

The first of these experiments is fit to 50 observations; the second is fit to 40 observations:
")

set.seed(214)
data(cars)

cm1 <- quap(
  alist(
    dist ~ dnorm(mu, sigma),
    mu <- a + b * speed,
    a ~ dnorm(0, 100),
    b ~ dnorm(0, 10),
    sigma ~ dexp(1)
  ),
  data = cars
)

fewer_obs <- cars[1:40, ]
cm2 <- quap(
  alist(
    dist ~ dnorm(mu, sigma),
    mu <- a + b * speed,
    a ~ dnorm(0, 100),
    b ~ dnorm(0, 10),
    sigma ~ dexp(1)
  ),
  data = fewer_obs
)
display(WAIC(cm1))
display(WAIC(cm2))

display_markdown("
**7M4.** What happens to the effective number of parameters, as measured by PSIS or WAIC, as a prior
becomes more concentrated? Why? Perform some experiments, if you are not sure.

**ERROR**: There is no 'effective number of parameters' associated with PSIS. Also, section 7.4.2
recommends using the term 'overfitting penalty' rather than 'effective number of parameters', which
exists for historical reasons.

**Answer.** An informative prior will reduce the effect of the sample on the posterior, but will in
general lead to a posterior that is more concentrated (like the prior). A more concentrated
posterior will lead to lower variance, decreasing the penalty term.

The first of these experiments has vague priors; the second has informative priors:
")

data(cars)

cm1 <- quap(
  alist(
    dist ~ dnorm(mu, sigma),
    mu <- a + b * speed,
    a ~ dnorm(0, 100),
    b ~ dnorm(0, 10),
    sigma ~ dexp(1)
  ),
  data = cars
)

cm2 <- quap(
  alist(
    dist ~ dnorm(mu, sigma),
    mu <- a + b * speed,
    a ~ dnorm(-20, 5),
    b ~ dnorm(4, 1),
    sigma ~ dexp(1)
  ),
  data = cars
)
display(WAIC(cm1))
display(WAIC(cm2))

display_markdown("
**7M5.** Provide an informal explanation of why informative priors reduce overfitting.

[pcr]: https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations

**Answer.** An informative prior reduces the influence of the sample on the posterior; the influence
of the sample is what causes overfitting.

See also [Prior Choice Recommendations][pcr].
")
display_markdown("
**7M6.** Provide an informal explanation of why overly informative priors result in underfitting.

**Answer.** An overly informative prior is overly skeptical; it claims to know more about the
problem at hand than the data does.
")

display_markdown("
**7H1.** In 2007, The Wall Street Journal published an editorial (“We’re Number One, Alas”) with a
graph of corporate tax rates in 29 countries plotted against tax revenue. A badly fit curve was
drawn in (reconstructed at right), seemingly by hand, to make the argument that the relationship
between tax rate and tax revenue increases and then declines, such that higher tax rates can
actually produce less tax revenue. I want you to actually fit a curve to these data, found in
`data(Laffer)`. Consider models that use tax rate to predict tax revenue. Compare, using WAIC or
PSIS, a straight-line model to any curved models you like. What do you conclude about the
relationship between tax rate and tax revenue?

**Answer.**

[wnoa]: https://www.wsj.com/articles/SB118428874152665452
[lc]: https://en.wikipedia.org/wiki/Laffer_curve

Related articles:
- [We're Number One, Alas (WSJ)][wnoa]
- [Laffer curve (Wikipedia)][lc]

The raw data “unbiased” by a line or curve:
")

data(Laffer)
iplot(function() plot(tax_revenue ~ tax_rate, Laffer))

display_markdown("A straight line model:")

lf1 <- quap(
  alist(
    tax_revenue ~ dnorm(mu, sigma),
    mu <- a + b * tax_rate,
    a ~ dnorm(0, 5),
    b ~ dnorm(0.1, 0.5),
    sigma ~ dexp(1)
  ),
  data = Laffer
)
post <- extract.samples(lf1)
tax_rate_seq <- seq(from = min(Laffer$tax_rate), to = max(Laffer$tax_rate), length.out = 100)
l <- link(lf1, data = list(tax_rate = tax_rate_seq))
mu <- apply(l, 2, mean)
ci <- apply(l, 2, PI)
iplot(function() {
  plot(tax_revenue ~ tax_rate, data = Laffer)
  lines(tax_rate_seq, mu)
  shade(ci, tax_rate_seq)
})
display(WAIC(lf1))
display(PSIS(lf1))

display_markdown("A curved (second order polynomial) model:")

lf2 <- quap(
  alist(
    tax_revenue ~ dnorm(mu, sigma),
    mu <- a + b[1] * tax_rate + b[2] * tax_rate^2,
    a ~ dnorm(0, 5),
    b ~ dnorm(0, 5),
    sigma ~ dexp(1)
  ),
  data = Laffer, start = list(b = rep(0, 2))
)
post <- extract.samples(lf2)
tax_rate_seq <- seq(from = min(Laffer$tax_rate), to = max(Laffer$tax_rate), length.out = 100)
l <- link(lf2, data = list(tax_rate = tax_rate_seq))
mu <- apply(l, 2, mean)
ci <- apply(l, 2, PI)
iplot(function() {
  plot(tax_revenue ~ tax_rate, data = Laffer)
  lines(tax_rate_seq, mu)
  shade(ci, tax_rate_seq)
})
display(WAIC(lf2))
display(PSIS(lf2))

iplot(function() plot(compare(lf1, lf2)), ar = 4)

display_markdown("
The curved model appears to be a better fit. The difference in WAIC between the two models is
significant but not definitive.
")

display_markdown("
**7H2.** In the Laffer data, there is one country with a high tax revenue that is an outlier. Use
PSIS and WAIC to measure the importance of this outlier in the models you fit in the previous
problem. Then use robust regression with a Student’s t distribution to revisit the curve fitting
problem. How much does a curved relationship depend upon the outlier point?

**Answer.** There are two major outliers:
")

PSIS_lf2 <- PSIS(lf2, pointwise = TRUE)
WAIC_lf2 <- WAIC(lf2, pointwise = TRUE)
iplot(function() {
  plot(PSIS_lf2$k, WAIC_lf2$penalty,
    xlab = "PSIS Pareto k",
    ylab = "WAIC penalty", col = rangi2, lwd = 2
  )
})

display_markdown(
  "The curved relationship flattens when this prior is adjusted:"
)

lf3 <- quap(
  alist(
    tax_revenue ~ dstudent(5, mu, sigma),
    mu <- a + b[1] * tax_rate + b[2] * tax_rate^2,
    a ~ dnorm(0, 5),
    b ~ dnorm(0, 5),
    sigma ~ dexp(1)
  ),
  data = Laffer, start = list(b = rep(0, 2))
)
post <- extract.samples(lf3)
tax_rate_seq <- seq(from = min(Laffer$tax_rate), to = max(Laffer$tax_rate), length.out = 100)
l <- link(lf3, data = list(tax_rate = tax_rate_seq))
mu <- apply(l, 2, mean)
ci <- apply(l, 2, PI)
iplot(function() {
  plot(tax_revenue ~ tax_rate, data = Laffer)
  lines(tax_rate_seq, mu)
  shade(ci, tax_rate_seq)
})

display_markdown("
**7H3.** Consider three fictional Polynesian islands. On each there is a Royal Ornithologist charged by
the king with surveying the bird population. They have each found the following proportions of 5
important bird species:

|          | Species A | Species B | Species C | Species D | Species E |
| ---      | ---       | ---       | ---       | ---       | ---       |
| Island 1 | 0.2       | 0.2       | 0.2       | 0.2       | 0.2       |
| Island 2 | 0.8       | 0.1       | 0.05      | 0.025     | 0.025     |
| Island 3 | 0.05      | 0.15      | 0.7       | 0.05      | 0.05      |

Notice that each row sums to 1, all the birds. This problem has two parts. It is not computationally
complicated. But it is conceptually tricky. First, compute the entropy of each island’s bird distribution.
Interpret these entropy values. Second, use each island’s bird distribution to predict the other two.
This means to compute the K-L Divergence of each island from the others, treating each island as if
it were a statistical model of the other islands. You should end up with 6 different K-L Divergence
values. Which island predicts the others best? Why?

**Answer.** Entropies in nats:
")

i1 <- c(0.2, 0.2, 0.2, 0.2, 0.2)
i2 <- c(0.8, 0.1, 0.05, 0.025, 0.025)
i3 <- c(0.05, 0.15, 0.7, 0.05, 0.05)
entropy <- function(pdf) {
  return(-sum(pdf * log(pdf)))
}
islands <- list(i1, i2, i3)
entropies <- lapply(islands, FUN = entropy)
names(entropies) <- c("island1", "island2", "island3")
display(entropies)

display_markdown("
If you're on island 1, it's hard to guess what a random bird is going to be because they are all
nearly equally likely (there is a lot of uncertainty). If you're on island 2, on the other hand, if
you guess species A you're likely to be right.
")
display_markdown("K-L Divergences in nats, including how each species contributes to the sum:")

nat_div <- function(pdf_p, pdf_q) {
  return(pdf_p * log(pdf_p / pdf_q))
}
# Partial permutations of islands
perm <- data.frame(island_p = c(1, 1, 2, 2, 3, 3), island_q = c(2, 3, 1, 3, 1, 2))
div <- t(mapply(function(p, q) nat_div(islands[[p]], islands[[q]]), perm$island_p, perm$island_q))
colnames(div) <- paste("div_species_", LETTERS[1:5], sep = "")
nat_kl_div <- rowSums(div)
display(cbind(perm, div, nat_kl_div))

display_markdown("K-L Divergences in bits, including how each species contributes to the sum:")
bit_div <- div / log(2)
bit_kl_div <- rowSums(bit_div)
display(cbind(perm, bit_div, bit_kl_div))

display_markdown("
Let's interpret the first row. If you're from island 2 (where species A is common) and you go to
island 1 but continue to guess bird species based on priors from your home island, you're going to
be surprised quite a bit. The biggest contributors to the additional surprisal you'll experience
will be species D and E, which are now 8x more common than what you're used to. Because log2(8) is
three, this additional surprisal (in bits) is $0.2·3 = 0.6$.

In the second row, you're from island 3 and visiting island 1. Your priors aren't as extreme as
island 2's so your overall KL divergence is lower.

In the third row, you're from island 1 and visiting island 2. You're from a high entropy island so
in general nothing surprises you much, but species A is common on your new island so you're
constantly surprised by seeing them since they're somewhat rare on your island.

In the fourth row, you're from island 3 and visiting island 2. You're incredibly surprised by seeing
species A everywhere, because they're quite rare on your island. The sixth row is the reversed
situation, but with slightly less surprisal because species C is less common on island 3 than
species A is on island 2.

The least surprisal occurs in the fifth row, when you're from island 1 and visiting island 3. You're
from a high entropy island so you aren't shocked by anything, and the surprisal you experience by
seeing species C everywhere is slightly less than if you were visiting island 2 coming from island
1.
")

display_markdown("
**7H4.** Recall the marriage, age, and happiness collider bias example from Chapter 6. Run models
m6.9 and m6.10 again. Compare these two models using WAIC (or LOO, they will produce identical
results). Which model is expected to make better predictions? Which model provides the correct
causal inference about the influence of age on happiness? Can you explain why the answers to these
two questions disagree?
")

source("load-marriage-collider-models.R")

display_markdown("
**Answer.** The first model performs better on WAIC and PSIS (better predictive performance),
despite the fact the second provides the correct causal inference:
")
iplot(function() plot(compare(m6.9, m6.10)), ar = 4.5)

display_markdown("
The answer to these two questions disagree because the predictive ability of a model is not
necessarily related to whether it is confounded. In this case being confounded helps the model make
a better prediction because understanding whether someone is married provides more direct
information about whether they are happy than how old they are:
")
library(dagitty)
dag_hma <- dagitty('
dag {
    bb="0,0,1,1"
    Happiness [pos="0.1,0.2"]
    Married [pos="0.3,0.2"]
    Age [pos="0.5,0.2"]
    Age -> Married
    Happiness -> Married
}')
iplot(function() plot(dag_hma), scale=5)

display_markdown("
**7H5.** Revisit the urban fox data, `data(foxes)`, from the previous chapter’s practice problems.
Use WAIC or PSIS based model comparison on five different models, each using weight as the outcome,
and containing these sets of predictor variables:
1. avgfood + groupsize + area
2. avgfood + groupsize
3. groupsize + area
4. avgfood
5. area

Can you explain the relative differences in WAIC scores, using the fox DAG from last week’s
homework? Be sure to pay attention to the standard error of the score differences (`dSE`).
")
source("load-fox-models.R")
iplot(function() {
  plot(compare(
    mfox_Weight_AvgfoodGroupsizeArea,
    mfox_Weight_AvgfoodGroupsize,
    mfox_Weight_GroupsizeArea,
    mfox_Weight_Avgfood,
    mfox_Weight_Area
  ))
}, ar = 3)

display_markdown("
**Answer.** Recall the fox DAG:
")
iplot(function() plot(dag_foxes), scale=10)

display_markdown("
Both avgfood and groupsize directly affect weight, so they are the most important predictors to have
in the model. The first three models have both of these predictors, the first two directly, and the
third indirectly by getting avgfood through area. These three models perform the best, and the
standard error of their score differences (`dSE`) indicate it is difficult to distinguish their
performance.

The fourth and fifth models do not perform as well because they can only predict the impact of
groupsize on weight indirectly through avgfood.
")
