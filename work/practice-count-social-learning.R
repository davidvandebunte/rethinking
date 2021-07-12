source("iplot.R")
library(rethinking)
library(dagitty)

display_markdown("
**11H6.** The data in `data(Primates301)` are 301 primate species and associated measures. In this
problem, you will consider how brain size is associated with social learning. There are three parts.

(a) Model the number of observations of `social_learning` for each species as a function of the log
`brain` size. Use a Poisson distribution for the `social_learning` outcome variable. Interpret the
resulting posterior.

**Answer.** The `head` of the `Primates301` data.frame:
")

data(Primates301)
display(head(Primates301, n = 20L))
prim_inc <- Primates301
prim <- prim_inc[complete.cases(prim_inc$social_learning, prim_inc$brain, prim_inc$research_effort), ]

display_markdown("
<br/>
After filtering for complete cases:
")
display(head(prim, n = 20L))

brain_dat <- list(
  SocialLearning = prim$social_learning,
  LogBrain = log(prim$brain)
)

mq_brain_size <- quap(
  alist(
    SocialLearning ~ dpois(lambda),
    log(lambda) <- a + b * LogBrain,
    a ~ dnorm(3, 0.5),
    b ~ dnorm(0, 0.2)
  ),
  data = brain_dat
)

mu_brain_size <- ulam(
  mq_brain_size@formula,
  data = brain_dat, chains = 4, cores = 4, log_lik = TRUE
)

display_markdown("
<br/>
Use `func=PSIS` to generate warnings for high Pareto k values.

If you look at the data, you'll see some classes (e.g. Gorillas and Chimpanzees) have orders of
magnitude more study and more social learning examples than other species.
")

iplot(function()
  plot(compare(mq_brain_size, mu_brain_size, func=PSIS))
, ar=4.5)

iplot(function() {
  display_markdown("<br/>Raw data:")
  x <- precis(mq_brain_size)
  display(x)
  plot(x, main="mq_brain_size")
}, ar=4.5)

display_markdown(r"(
Although the model is confident in the `b` coefficient, the wide error bars on the WAIC scores
indicate the out-of-sample performance may be much worse.

Still, in the context of this model it appears there is an association between brain size and social
learning. For every unit increase in `LogBrain` i.e. for every order of magnitude increase in brain
size we should see an exponential increase in examples of social learning. If we let $b = 1.5$ then
for every order of magnitude we'll see about $exp(1.5) \approx 4.5$ more examples of social
learning.
)")

display_markdown("
(b) Some species are studied much more than others. So the number of reported instances of
`social_learning` could be a product of research effort. Use the `research_effort` variable,
specifically its logarithm, as an additional predictor variable. Interpret the coefficient for log
`research_effort`. How does this model differ from the previous one?
")

brain_dat$LogResearchEffort <- log(prim$research_effort)

mq_brain_research <- quap(
  alist(
    SocialLearning ~ dpois(lambda),
    log(lambda) <- a + b * LogBrain + d * LogResearchEffort,
    a ~ dnorm(3, 0.5),
    b ~ dnorm(0, 0.2),
    d ~ dnorm(0, 0.2)
  ),
  data = brain_dat
)

mu_brain_research <- ulam(
  mq_brain_research@formula,
  data = brain_dat, chains = 4, cores = 4, log_lik = TRUE
)

display_markdown("
Again, use `func=PSIS` to generate warnings for high Pareto k values:
")

iplot(function()
  plot(compare(mq_brain_research, mu_brain_research, func=PSIS))
, ar=4.5)

iplot(function() {
  display_markdown("<br/>Raw data:")
  x <- precis(mq_brain_research)
  display(x)
  plot(x, main="mq_brain_research")
}, ar=4.5)

display_markdown("
**Answer.** When we start to condition on research effort, the influence of brain size drops dramatically
(to about 0.2 from 1.5).
")

display_markdown("
(c) Draw a DAG to represent how you think the variables `social_learning`, `brain`, and
`research_effort` interact. Justify the DAG with the measured associations in the two models above
(and any other models you used).

**Answer.** It seems likely we were looking at the total causal effect of LogBrain in the first
model, and the direct causal effect in the second.
")
brain_social_learning_dag <- dagitty('
dag {
    bb="0,0,1,1"
    LogBrain [pos="0.100,0.400"]
    LogResearchEffort [pos="0.300,0.300"]
    SocialLearning [pos="0.500,0.400"]
    LogBrain -> SocialLearning
    LogBrain -> LogResearchEffort
    LogResearchEffort -> SocialLearning
}')
iplot(function() plot(brain_social_learning_dag), scale=10)
