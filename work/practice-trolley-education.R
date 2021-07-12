source('iplot.R')
library(rethinking)
library(dagitty)

display_markdown("
[cd]: https://en.wikipedia.org/wiki/Causality#Counterfactual_theories
[bh]: https://en.wikipedia.org/wiki/Bradford_Hill_criteria
[ci]: https://en.wikipedia.org/wiki/Causal_inference#Definition

**12H7.** In the trolley data — `data(Trolley)` — we saw how education level (modeled as an ordered
category) is associated with responses. But is this association causal? One plausible confound is
that education is also associated with age, through a causal process: People are older when they
finish school than when they begin it. Reconsider the Trolley data in this light. Draw a DAG that
represents hypothetical causal relationships among response, education, and age. Which statical
model or models do you need to evaluate the causal influence of education on responses? Fit these
models to the trolley data. What do you conclude about the causal relationships among these three
variables?

**ERROR:**
> Which statical [sic] model or models do you need

**Answer.** Consider the following DAGs. Implicit in this list of DAGs is that the response would
never influence age or education. Because we already know that education is associated with the
response, we also only include DAGs where `Education` and `Response` are d-connected.

A more difficult question is what arrows to allow between education and age. Getting older does not
necessarily lead to more education, but education always leads to greater age. That is, someone
could stop schooling at any point in their lives. This perspective implies `Education` should cause
`Age`, because the choice to get a particular education is also a choice to get older before
continuing with 'real' work.

The issue with allowing this arrow is it contradicts the following definition of [Causal
dependence][cd] from David Lewis:

> An event E causally depends on C if, and only if, (i) if C had occurred, then E would have
> occurred, and (ii) if C had not occurred, then E would not have occurred.

Said another way, causality is almost always associated with the advancement of time. This is
inherent in the definition of causality above, and in lists (see #4) such as the [Bradford-Hill
criteria][bh]. See also the definition of [Causal inference][ci]. The `Age` covariate is confusingly
a measurement of time. It is perhaps best to think of `Age` as a measurement of the potential to get
education; greater `Age` indicates an individual is 'richer' in terms of time opportunity to get an
education.

Said yet another way, age is not controllable. We will get older regardless of whether we get an
education. Education on the other hand is clearly within our control. The issue seems to be that we
require two things to get a particular educational degree: a personal commitment to do so and an
investment of our time. We talk about investing our time when in the end we are always forced to
invest it in something; we can't choose to stay in a frozen present.

This perspective leads to other questions. Age does not necessarily indicate the level of education
we will eventually achieve. Are people who had a healthy upbringing, e.g. with more material
resources, more or less likely to provide disapproving responses, and also more or less likely to
get an education? There are plenty of possible confounds that could influence both eventual
education level and response, such as temperament. Would a 14 year old who will eventually get a PhD
answer the same way to these questions now and after they finish their PhD?

Practially speaking, based on the wording in the question and the last paragraph of section 12.4, it
seems the author wanted readers to only consider that `Age` influences `Education` (not vice versa).

See also the next question, which adds a few more comments on this issue.

Conditional independencies, if any, are listed under each DAG.

In this first model, education is a mediator between age and response:
")

age_dag <- dagitty('
dag {
    bb="0,0,1,1"
    Age [pos="0.4,0.2"]
    Education [pos="0.6,0.2"]
    Response [pos="0.5,0.3"]
    Age -> Education
    Education -> Response
}')
iplot(function() plot(age_dag), scale=10)
display(impliedConditionalIndependencies(age_dag))

display_markdown("
<br/>
In this model, all variables are associated regardless of conditioning:
")

mediator_dag <- dagitty('
dag {
    bb="0,0,1,1"
    Age [pos="0.4,0.2"]
    Education [pos="0.6,0.2"]
    Response [pos="0.5,0.3"]
    Age -> Education
    Age -> Response
    Education -> Response
}')
iplot(function() plot(mediator_dag), scale=10)
display(impliedConditionalIndependencies(mediator_dag))

display_markdown("
<br/>
In this model, education has no direct causal effect on the response:
")

independent_dag <- dagitty('
dag {
    bb="0,0,1,1"
    Age [pos="0.4,0.2"]
    Education [pos="0.6,0.2"]
    Response [pos="0.5,0.3"]
    Age -> Education
    Age -> Response
}')
iplot(function() plot(independent_dag), scale=10)
display(impliedConditionalIndependencies(independent_dag))

display_markdown("
<br/>
This answer will not consider this model because, as stated above, it seems more reasonable to
assume that age leads to education than vice versa. Notice it has the same conditional
independencies as the last model, and we could construct similar models where `Education -> Age` and
with the same conditional independencies as the first and second models. Although that approach
would work for this question, see comments in the next question.
")

independent_dag <- dagitty('
dag {
    bb="0,0,1,1"
    Age [pos="0.4,0.2"]
    Education [pos="0.6,0.2"]
    Response [pos="0.5,0.3"]
    Education -> Age
    Age -> Response
}')
iplot(function() plot(independent_dag), scale=10)
display(impliedConditionalIndependencies(independent_dag))
display_markdown("
<br/>
To test which model is correct, we only need one more model beyond what we learned from `m12.6` in
the text: a model that includes age as a predictor. If we find that the education coefficient `bE`
is near zero in this model, the third causal model is correct. If we find that the age coefficient
is near zero, the first causal model is correct. If both coefficients are non-zero, we can infer the
second causal model is correct. Of course, all causal inferences are limited by the variables we are
considering.
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
  alpha = rep(2, 7)
)

m_trolley_age <- ulam(
  alist(
    R ~ ordered_logistic(phi, kappa),
    phi <- bE * sum(delta_j[1:E]) + bStdAge * StdAge + bA * action + bI * intention + bC * contact,
    kappa ~ normal(0, 1.5),
    c(bA, bI, bC, bE, bStdAge) ~ normal(0, 1),
    vector[8]:delta_j <<- append_row(0, delta),
    simplex[7]:delta ~ dirichlet(alpha)
  ),
  data = dat, chains = 4, cores = min(detectCores(), 4), iter=2000
)

iplot(function() {
  plot(precis(m_trolley_age, depth=2), main="m_trolley_age")
}, ar=1.8)
display_markdown("Raw data (preceding plot):")
display(precis(m_trolley_age, depth = 2), mimetypes="text/plain")

display_markdown("
<br/>
As implied in the text, it is not correct to causally infer the associated of education `bE` is
negative based on `m12.6`. In this model, the association of education with response has reversed,
becoming positive. Since the error bars on neither `bStdAge` or `bE` cross zero, we'll infer for now
that the second causal model above is correct.
")
