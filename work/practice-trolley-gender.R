source('iplot.R')
library(rethinking)
library(dagitty)

display_markdown("
**12H8.** Consider one more variable in the trolley data: Gender. Suppose that gender might influence
education as well as response directly. Draw the DAG now that includes response, education, age, and
gender. Using only the DAG, is it possible that the inferences from 12H7 above are confounded by
gender? If so, define any additional models you need to infer the causal influence of education on
response. What do you conclude?

**Answer.** First, lets consider a DAG that doesn't work. In the following DAG we assume
`Education -> Age` rather than that `Age -> Gender`:
")

incorrect_dag <- dagitty('
dag {
    bb="0,0,1,1"
    Education [pos="0.4,0.2"]
    Age [pos="0.6,0.2"]
    Response [pos="0.5,0.3"]
    Gender [pos="0.3,0.3"]
    Education -> Age
    Education -> Response
    Age -> Response
    Gender -> Education
    Gender -> Response
}')
iplot(function() plot(incorrect_dag), scale=10)
display(impliedConditionalIndependencies(incorrect_dag))

display_markdown("
<br/>
With the additions implied by this question, this DAG implies that `Gender` will be able to predict
`Age` without conditioning, which is not correct.

Instead, we'll add this question's covariate to the second model from the last question:
")

gender_dag <- dagitty('
dag {
    bb="0,0,1,1"
    Age [pos="0.4,0.2"]
    Education [pos="0.6,0.2"]
    Response [pos="0.5,0.3"]
    Gender [pos="0.7,0.3"]
    Age -> Education
    Education -> Response
    Age -> Response
    Gender -> Education
    Gender -> Response
}')
iplot(function() plot(gender_dag), scale=10)
display(impliedConditionalIndependencies(gender_dag))

display_markdown("
<br/>
This DAG implies `Age` will always be independent of `Gender`, which seems reasonable in this
context, ignoring that females live slightly longer than males. To infer either the direct or total
causal effect of `Education` on `Response` the required adjustment is to condition on both `Age` and
`Gender`:
")

display(adjustmentSets(gender_dag, exposure="Education", outcome="Response", effect="total"))

display_markdown("
<br/>
We can do so by fitting a model that includes all three variables as predictors of `Response`:
")

data(Trolley)
d <- Trolley

edu_levels <- c(6, 1, 8, 4, 7, 2, 5, 3)
d$edu_new <- edu_levels[d$edu]

dat <- list(
  R = d$response,
  action = d$action,
  intention = d$intention,
  contact = d$contact,
  E = as.integer(d$edu_new),
  StdAge = standardize(d$age),
  gid = ifelse(d$male == 1, 2, 1),
  alpha = rep(2, 7)
)

# This fit takes more than three hours with 4 chains/cores.
m_trolley_gender <- ulam(
  alist(
    R ~ ordered_logistic(phi, kappa),
    phi <- bE * sum(delta_j[1:E]) + bStdAge * StdAge + bGender[gid]
        + bA * action + bI * intention + bC * contact,
    kappa ~ normal(0, 1.5),
    bGender[gid] ~ normal(0, 1),
    c(bA, bI, bC, bE, bStdAge) ~ normal(0, 1),
    vector[8]:delta_j <<- append_row(0, delta),
    simplex[7]:delta ~ dirichlet(alpha)
  ),
  data = dat, chains = 2, cores = min(detectCores(), 2), iter=1600
)
flush.console()

iplot(function() {
  plot(precis(m_trolley_gender, depth=2), main="m_trolley_gender")
}, ar=1.8)
display_markdown("Raw data (preceding plot):")
display(precis(m_trolley_gender, depth = 2), mimetypes="text/plain")

display_markdown("
<br/>
At this point, education appears to have no causal effect on response. That is, the mean and HPDI of
`bE` are near zero.
")
