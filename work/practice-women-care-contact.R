library(rethinking)
source('iplot.R')

display_markdown("
**12H5.** One hypothesis from developmental psychology, usually attributed to Carol Gilligan,
proposes that women and men have different average tendencies in moral reasoning. Like most
hypotheses in social psychology, it is merely descriptive, not causal. The notion is that women are
more concerned with care (avoiding harm), while men are more concerned with justice and rights.
Evaluate this hypothesis, using the `Trolley` data, supposing that `contact` provides a proxy for
physical harm. Are women more or less bothered by contact than are men, in these data? Figure out
the model(s) that is needed to address this question.

**Answer.** The `head` of the `Trolley` data.frame:
")

data(Trolley)
d <- Trolley
display(head(Trolley))

display_markdown("
<br/>
We'll start from model `m12.5` in the text, converting `bC` and `bIC` to index variables based on
gender. Before doing the conversion, lets reproduce the `precis` summary for `m12.5` in the text:
")

dat <- list(
  R = d$response,
  A = d$action,
  I = d$intention,
  C = d$contact
)
m12.5 <- ulam(
  alist(
    R ~ dordlogit(phi, cutpoints),
    phi <- bA * A + bC * C + BI * I,
    BI <- bI + bIA * A + bIC * C,
    c(bA, bI, bC, bIA, bIC) ~ dnorm(0, 0.5),
    cutpoints ~ dnorm(0, 1.5)
  ),
  data = dat, chains = 4, cores = min(detectCores(), 4), log_lik=TRUE
)

iplot(function() {
  plot(precis(m12.5), main="m12.5")
}, ar=4)

dat <- list(
  R = d$response,
  A = d$action,
  I = d$intention,
  C = d$contact,
  gid = ifelse(d$male == 1, 2, 1)
)
m_trolley_care <- ulam(
  alist(
    R ~ dordlogit(phi, cutpoints),
    phi <- bA * A + bC[gid] * C + BI * I,
    BI <- bI + bIA * A + bIC[gid] * C,
    c(bA, bI, bIA) ~ dnorm(0, 0.5),
    bC[gid] ~ dnorm(0, 0.5),
    bIC[gid] ~ dnorm(0, 0.5),
    cutpoints ~ dnorm(0, 1.5)
  ),
  data = dat, chains = 4, cores = min(detectCores(), 4), log_lik=TRUE
)
flush.console()

display_markdown("
<br/>
The following plot shows the same `precis` summary after converting `bC` and `bIC` to index
variables based on gender. It appears women disapprove of contact more than men from this data and
model, lending some support to the theory:
")

iplot(function() {
  plot(precis(m_trolley_care, depth=2), main="m_trolley_care")
}, ar=2.5)

display_markdown("
Comparing the models using PSIS, to check for overfitting or outliers:
")

iplot(function() {
  plot(compare(m12.5, m_trolley_care, func=PSIS))
}, ar=4.5)
