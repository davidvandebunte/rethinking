library(rethinking)
library(IRdisplay)
source("iplot.R")

display_markdown("## 13.7. Practice")

display_markdown(r"(
**13E1.** Which of the following priors will produce more *shrinkage* in the estimates?

(a) $\alpha_{tank} \sim Normal(0,1)$

(b) $\alpha_{tank} \sim Normal(0,2)$

**Answer.** We could interpret this question in two ways. The first is that the author meant to
write:

(a) $\bar{\alpha} \sim Normal(0,1)$

(b) $\bar{\alpha} \sim Normal(0,2)$

That is, interpretion #1 is that these are priors for the hyperparameter for the average of all
tanks, with no new prior specified for the hyperparameter for the variability among tanks $\sigma$.

The second interpretation is that the author is trying to express in shorthand new priors for both
the $\bar{\alpha}$ and $\sigma$ priors, and only the expected value of these two priors, ignoring
a standard deviation. To use the same terms as the chapter:

(a) $\alpha_{tank} \sim Normal(\bar{\alpha},\sigma),
     \bar{\alpha} \sim Normal(0,?),
     \sigma \sim Exponential(1)$

(b) $\alpha_{tank} \sim Normal(\bar{\alpha},\sigma),
     \bar{\alpha} \sim Normal(0,?),
     \sigma \sim Exponential(\frac{1}{2})$

Notice we decrease the $\lambda$ parameter to the exponential distribution to increase the expected
value.

Said another way, the first interpretation is that the author meant to specify this prior at the
second level of the model, and the second interpretation is that the author meant to specify this
prior at the first level. The first interpretation makes sense given this prior shows up directly in
models in the chapter already, without much reinterpretation. The second interpretation is supported
by the author's choice of variable name (e.g. see the chapter and question **13M3**). See also
question **13M5**.

In both interpretations, prior (a) will produce more shrinkage because it is more regularizing. It
has a smaller standard deviation and therefore expects less variability.
)")

display_markdown(r"(
**13E2.** Rewrite the following model as a multilevel model.

$$
\begin{align}
y_i & \sim Binomial(1,p_i) \\
logit(p_i) & = \alpha_{group[i]} + \beta x_i \\
\alpha_{group} & \sim Normal(0, 1.5) \\
\beta & \sim Normal(0, 0.5)
\end{align}
$$

**Answer.** The focus of this chapter is on varying intercepts rather than varying effects (see the next
chapter), so we'll only convert the intercept:

$$
\begin{align}
y_i & \sim Binomial(1,p_i) \\
logit(p_i) & = \alpha_{group[i]} + \beta x_i \\
\alpha_{group} & \sim Normal(\bar{\alpha}, \sigma) \\
\bar{\alpha} & \sim Normal(0, 1.5) \\
\sigma & \sim Exponential(1) \\
\beta & \sim Normal(0, 0.5)
\end{align}
$$
)")

display_markdown(r"(
**13E3.** Rewrite the following model as a multilevel model.

$$
\begin{align}
y_i & \sim Normal(\mu_i,\sigma) \\
\mu_i & = \alpha_{group[i]} + \beta x_i \\
\alpha_{group} & \sim Normal(0, 5) \\
\beta & \sim Normal(0, 1) \\
\sigma & \sim Exponential(1)
\end{align}
$$

**Answer.** As in the last question, we'll only convert the intercept:

$$
\begin{align}
y_i & \sim Normal(\mu_i,\sigma_1) \\
\mu_i & = \alpha_{group[i]} + \beta x_i \\
\alpha_{group} & \sim Normal(\bar{\alpha}, \sigma_2) \\
\bar{\alpha} & \sim Normal(0, 5) \\
\sigma_2 & \sim Exponential\left(\frac{1}{2}\right) \\
\beta & \sim Normal(0, 1) \\
\sigma_1 & \sim Exponential(1)
\end{align}
$$
)")

display_markdown(r"(
**13E4.** Write a mathematical model formula for a Poisson regression with varying intercepts.

$$
\begin{align}
y_i & \sim Poisson(\lambda) \\
log(\lambda) & = \alpha_{group[i]} \\
\alpha_{group} & \sim Normal(\bar{\alpha}, \sigma) \\
\bar{\alpha} & \sim Normal(0, 5) \\
\sigma & \sim Exponential(1)
\end{align}
$$
)")

display_markdown(r"(
**13E5.** Write a mathematical model formula for a Poisson regression with two different kinds of
varying intercepts, a cross-classified model.

$$
\begin{align}
y_i & \sim Poisson(\lambda) \\
log(\lambda) & = \alpha_{x[i]} + \alpha_{y[i]} \\
\alpha_{x} & \sim Normal(\mu_x, \sigma_x) \\
\mu_x & \sim Normal(0, 5) \\
\sigma_x & \sim Exponential(1) \\
\alpha_{y} & \sim Normal(0, \sigma_y) \\
\sigma_y & \sim Exponential(1)
\end{align}
$$
)")

display_markdown(r"(
**13M1.** Revisit the Reed frog survival data, `data(reedfrogs)`, and add the `predation` and `size`
treatment variables to the varying intercept model. Consider models with either main effect alone,
both main effects, as well as a model including both and their interaction. Instead of focusing on
inferences about these two predictor variables, focus on the inferred variation across tanks.
Explain why it changes as it does across models.

**ERROR.** The `predation` predictor is actually labeled `pred`.

**ERROR.** The author duplicated this question in **13H4**, without realizing he had moved it and
primarily only changed wording.

Compare the second sentences:

> Consider models with either main effect alone, both main effects, as well as a model including both
> and their interaction.

> Consider models with either predictor alone, both predictors, as well as a model including their
> interaction.

One sentence only exists in **13H4**:

> What do you infer about the causal inference of these predictor variables?

Compare the last two sentences:

> Instead of focusing on inferences about these two predictor variables, focus on the inferred
> variation across tanks. Explain why it changes as it does across models.

> Also focus on the inferred variation across tanks (the $\sigma$ across tanks). Explain why it
> changes as it does across models with different predictors included.

We could treat **13H4** as a separate question that expands on **13M1** by adding causal inference,
but this answer will combine the two.

**Answer.** First, let's reproduce results from model `m13.2` in the chapter:
)")

source('load-reed-frog-model.R')

iplot(function() {
  plot(precis(m13.2, depth=2), main='m13.2')
}, ar=1.0)
display_markdown("Raw data (preceding plot):")
display(precis(m13.2, depth = 2), mimetypes="text/plain")

rf_df$Predator <- as.integer(as.factor(rf_df$pred))
rf_df$Size <- as.integer(as.factor(rf_df$size))
rf_df$Treatment <- 1 + ifelse(rf_df$Predator == 1, 0, 1) + 2*ifelse(rf_df$Size == 1, 0, 1)

display_markdown(r"(
<br/>
The `reedfrogs` data.frame is small enough to show in its entirety. Notice several new preprocessed
variables (columns) this solution will introduce later as they are used in models:
)")

display(rf_df)

display_markdown(r"(<br/>The `help` is also short:)")
display(help(reedfrogs))

display_markdown(r"(
Our first model will add the `pred` predictor on only the first level:

$$
\begin{align}
S_i & \sim Binomial(N_i,p_i) \\
logit(p_i) & = \alpha_{tank[i]} + \beta_{pred[i]} \\
\alpha_j & \sim Normal(\bar{\alpha}, \sigma), j = 1..48 \\
\bar{\alpha} & \sim Normal(0, 1.5) \\
\sigma & \sim Exponential(1) \\
\beta_j & \sim Normal(0, 1.5), j = 1..2
\end{align}
$$

Seeing as this is a chapter on multilevel models, and it's generally advisable to consider a
multilevel model when we have exchangeable index variables, should we add a second level of
parameters for this predictor? In other words:

$$
\begin{align}
S_i & \sim Binomial(N_i,p_i) \\
logit(p_i) & = \alpha_{tank[i]} + \beta_{pred[i]} \\
\alpha_j & \sim Normal(\bar{\alpha}, \sigma_{\alpha}), j = 1..48 \\
\bar{\alpha} & \sim Normal(0, 1.5) \\
\sigma_{\alpha} & \sim Exponential(1) \\
\beta_j & \sim Normal(0, \sigma_{\beta}), j = 1..2 \\
\sigma_{\beta} & \sim Exponential(1)
\end{align}
$$

The problem with this model is that there are only 2 (rather than 48) clusters for us to learn from,
that is, we are adding one second-level parameter to go with only two first-level parameters. Said
another way, do our clusters have anything to learn from each other? It may help to not forget about
the last tank of tadpoles when learning about a new one, or the last chimp when learning about a new
one, but in this case there is only one other pool to learn from: either all predator pools, or all
non-predator pools.

We could try to remember what we learned about one predator pool when moving to another predator
pool. In this approach, we would learn one $\bar{\alpha}$ and one $\sigma$ for the predator pools,
and one $\bar{\alpha}$ and one $\sigma$ for the non-predator pools. Presumably this would require
reindexing predator and non-predator pools from 1..24. The original model from the chapter already
has an intercept for individual pools this approach would likely not differ significantly from, so
we will avoid these complications and only add `pred` on the first level.

Now let's fit our new model, with a `pred` predictor on the first level. This model struggles to fit
as provided above. To avoid Stan warnings about RHat, Bulk ESS, and Tail ESS, we're actually going
to fit a model with a different $\bar{\alpha}$ prior than the one proposed above:

$$
\begin{align}
S_i & \sim Binomial(N_i,p_i) \\
logit(p_i) & = \alpha_{tank[i]} + \beta_{pred[i]} \\
\alpha_j & \sim Normal(\bar{\alpha}, \sigma), j = 1..48 \\
\bar{\alpha} & \sim Normal(0, 0.1) \\
\sigma & \sim Exponential(1) \\
\beta_j & \sim Normal(0, 1.5), j = 1..2
\end{align}
$$

See more comments on this tight $\bar{\alpha}$ prior below.
)")

rf_dat <- list(
  S = rf_df$surv,
  N = rf_df$density,
  tank = rf_df$tank,
  Predator = rf_df$Predator
)

m_rf_pred_orig <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a[tank] + bPredator[Predator],
    a[tank] ~ dnorm(a_bar, sigma),
    a_bar ~ dnorm(0, 0.1),
    sigma ~ dexp(1),
    bPredator[Predator] ~ dnorm(0, 1.5)
  ),
  data = rf_dat, chains = 4, cores = 4, log_lik = TRUE, iter=2000
)

iplot(function() {
  plot(precis(m_rf_pred_orig, depth=2), main='m_rf_pred_orig')
}, ar=1.0)
display_markdown("Raw data (preceding plot):")
display(precis(m_rf_pred_orig, depth = 2), mimetypes="text/plain")

display_markdown("
<br/>
As explained above, this model struggles to sample. We've managed to avoid the warnings and achieve
decent mixing by tightening a prior:
")

iplot(function() {
  traceplot(m_rf_pred_orig, pars=c("a[41]", "a[30]", "a_bar", "sigma", "bPredator[1]", "bPredator[2]"))
}, ar=2)
iplot(function() {
  trankplot(m_rf_pred_orig, pars=c("a[41]", "a[30]", "a_bar", "sigma", "bPredator[1]", "bPredator[2]"))
}, ar=2)

display_markdown(r"(
Why are we struggling to sample? To debug this, let's go back to the chapter on debugging MCMC, in
particular section **9.5.4.** on non-identifiable parameters. If non-identifiable parameters are a
cause of this symptom, what could we check for? Going even further back, to the end of section
**6.1**, the author suggests checking whether the posterior has changed relative to the prior. This
is a good question to ask regularly; which parameters are unchanged relative to their prior? The
obvious parameter is $\bar{\alpha}$, which we already had to tweak. Let's change the $\bar{\alpha}$
prior again and see if it remains unchanged in the posterior:

$$
\begin{align}
S_i & \sim Binomial(N_i,p_i) \\
logit(p_i) & = \alpha_{tank[i]} + \beta_{pred[i]} \\
\alpha_j & \sim Normal(\bar{\alpha}, \sigma), j = 1..48 \\
\bar{\alpha} & \sim Normal(1.0, 0.1) \\
\sigma & \sim Exponential(1) \\
\beta_j & \sim Normal(0, 1.5), j = 1..2
\end{align}
$$
)")

m_rf_pred_shift <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a[tank] + bPredator[Predator],
    a[tank] ~ dnorm(a_bar, sigma),
    a_bar ~ dnorm(1.0, 0.1),
    sigma ~ dexp(1),
    bPredator[Predator] ~ dnorm(0, 1.5)
  ),
  data = rf_dat, chains = 4, cores = 4, log_lik = TRUE, iter=2000
)

iplot(function() {
  plot(precis(m_rf_pred_shift, depth=2), main='m_rf_pred_shift')
}, ar=1.0)
display_markdown("Raw data (preceding plot):")
display(precis(m_rf_pred_shift, depth = 2), mimetypes="text/plain")

display_markdown(r"(
<br/>
The $\bar{\alpha}$ prior remains unchanged even after being shifted. Notice that the predator
parameters are the ones that have responded (and unsuprisingly, all the `a` parameters). Previously
the model had inferred a 'lack of predator' drastically helps survival and a predator has no effect
on survival (treating a predator as the baseline). Now it has inferred a predator hurts chances of
survival, and the lack of a predator doesn't help as much. That is, all these parameters only make
sense relative to each other.

To avoid this mess we could use an indicator variable:

$$
\begin{align}
S_i & \sim Binomial(N_i,p_i) \\
logit(p_i) & = \alpha_{tank[i]} + \beta P_i \\
\alpha_j & \sim Normal(\bar{\alpha}, \sigma), j = 1..48 \\
\bar{\alpha} & \sim Normal(0, 2) \\
\sigma & \sim Exponential(1) \\
\beta & \sim Normal(0, 1.5)
\end{align}
$$
)")

rf_dat <- list(
  S = rf_df$surv,
  N = rf_df$density,
  tank = rf_df$tank,
  Predator = ifelse(rf_df$Predator == 1, 0, 1)
)
m_rf_pred_indicator <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a[tank] + bPredator * Predator,
    a[tank] ~ dnorm(a_bar, sigma),
    a_bar ~ dnorm(0, 2),
    sigma ~ dexp(1),
    bPredator ~ dnorm(0, 3)
  ),
  data = rf_dat, chains = 4, cores = 4, log_lik = TRUE, iter=4000
)


iplot(function() {
  plot(precis(m_rf_pred_indicator, depth=2), main='m_rf_pred_indicator')
}, ar=1.0)
display_markdown("Raw data (preceding plot):")
display(precis(m_rf_pred_indicator, depth = 2), mimetypes="text/plain")

display_markdown(r"(
<br/>
Notice in the last model we've been able to widen the $\bar{\alpha}$ prior and actually learn it
from the data, that is, the prior changes in the posterior.

Still, an indicator variable isn't ideal for at least one of the reasons given in section
**5.3.1.**: We aren't more uncertain about the predator than the non-predator situation. The
indicator variable approach also makes it harder to read the `a` parameters relative to `a_bar`
since they aren't centered at zero, and makes it harder to set an `a_bar` prior.

Instead let's simply fix the `a_bar` prior's expected value to zero as suggested in section
**13.3.1**:

$$
\begin{align}
S_i & \sim Binomial(N_i,p_i) \\
logit(p_i) & = \alpha_{tank[i]} + \beta_{pred[i]} \\
\alpha_j & \sim Normal(0, \sigma), j = 1..48 \\
\sigma & \sim Exponential(1) \\
\beta_j & \sim Normal(0, 1.5), j = 1..2
\end{align}
$$
)")

rf_dat <- list(
  S = rf_df$surv,
  N = rf_df$density,
  tank = rf_df$tank,
  Predator = rf_df$Predator
)
m_rf_df_pred <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a[tank] + bPredator[Predator],
    a[tank] ~ dnorm(0, sigma),
    sigma ~ dexp(1),
    bPredator[Predator] ~ dnorm(0, 1.5)
  ),
  data = rf_dat, chains = 4, cores = 4, log_lik = TRUE, iter=2000
)

iplot(function() {
  plot(precis(m_rf_df_pred, depth=2), main='m_rf_df_pred')
}, ar=1.0)
display_markdown("Raw data (preceding plot):")
display(precis(m_rf_df_pred, depth = 2), mimetypes="text/plain")

display_markdown(r"(
<br/>
Let's fit a similar model with only the `size` predictor:

$$
\begin{align}
S_i & \sim Binomial(N_i,p_i) \\
logit(p_i) & = \alpha_{tank[i]} + \beta_{size[i]} \\
\alpha_j & \sim Normal(0, \sigma), j = 1..48 \\
\sigma & \sim Exponential(1) \\
\beta_j & \sim Normal(0, 1.5), j = 1..2
\end{align}
$$
)")

rf_dat <- list(
  S = rf_df$surv,
  N = rf_df$density,
  tank = rf_df$tank,
  Size = rf_df$Size
)
m_rf_df_size <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a[tank] + bSize[Size],
    a[tank] ~ dnorm(0, sigma),
    sigma ~ dexp(1),
    bSize[Size] ~ dnorm(0, 1.5)
  ),
  data = rf_dat, chains = 4, cores = 4, log_lik = TRUE, iter=2000
)

iplot(function() {
  plot(precis(m_rf_df_size, depth=2), main='m_rf_df_size')
}, ar=1.0)
display_markdown("Raw data (preceding plot):")
display(precis(m_rf_df_size, depth = 2), mimetypes="text/plain")

display_markdown(r"(
<br/>
Finally, lets model an interaction term. We've already added a 'treatment' index variable in
preprocessing; see the data.frame near the start of this question.

$$
\begin{align}
S_i & \sim Binomial(N_i,p_i) \\
logit(p_i) & = \alpha_{tank[i]} + \beta_{Treatment[i]} \\
\alpha_j & \sim Normal(0, \sigma), j = 1..48 \\
\sigma & \sim Exponential(1) \\
\beta_j & \sim Normal(0, 1.5), j = 1..4
\end{align}
$$
)")

rf_dat <- list(
  S = rf_df$surv,
  N = rf_df$density,
  tank = rf_df$tank,
  Treatment = rf_df$Treatment
)
m_rf_df_interaction <- ulam(
  alist(
    S ~ dbinom(N, p),
    logit(p) <- a[tank] + bTreatment[Treatment],
    a[tank] ~ dnorm(0, sigma),
    sigma ~ dexp(1),
    bTreatment[Treatment] ~ dnorm(0, 1.5)
  ),
  data = rf_dat, chains = 4, cores = 4, log_lik = TRUE, iter=2000
)

iplot(function() {
  plot(precis(m_rf_df_interaction, depth=2), main='m_rf_df_interaction')
}, ar=1.0)
display_markdown("Raw data (preceding plot):")
display(precis(m_rf_df_interaction, depth = 2), mimetypes="text/plain")

display_markdown(r"(
<br/>
Let's go back to the original question:

> Instead of focusing on inferences about these two predictor variables, focus on the inferred
> variation across tanks. Explain why it changes as it does across models.

The inferred variation across tanks ($\sigma$) decreases in these models as we add more predictors.
We see the largest $\sigma$ in `m13.2` where we haven't tried to explain variation with any
predictors. When we add the predator predictor in `m_rf_df_pred` we see `sigma` drop significantly
because there is a lot of predictive power in that variable (or at least more than `size`). The
model with an interaction term has the lowest inference for `sigma` because it indirectly has access
to all the predictors.

One question only exists in **13H4**:

> What do you infer about the causal inference of these predictor variables?

In the typical predator-prey relationship it's more appropriate to use differential equations to
model the relationship between the populations. Reading the original paper, though, it appears that
the presence of predators, tadpole sizes, and initial density were manipulated. If that is the case
then the causal diagram would be relatively simple: the predictors can only affect the outcome, not
each other.

It's tempting to read from the difference between `bTreatment[1]` and `bTreatment[3]` relative to
the difference between `bTreatment[2]` and `bTreatment[4]` that being large helps survival in the
absence of predators (to overcome the elements/nature) but in the presence of predators being large
makes a tadpole easier to catch (harder to hide) and more desirable to catch.

The support for the advantage of size in the absence of predators is limited, though, considering
the error bars on `bTreatment[1]` and `bTreatment[3]`. If we only accept the significant difference
in survivability from being smaller in the presence of predators, we would conclude being small
would primarily be an advantage in the larval/tadpole stage of the frog's development without an
explanation for why some tadpoles hatch larger. Based on the original paper, the confounding
variable seems to be survivability at metamorphosis, which could influence the timing of hatching
and therefore the ideal tadpole size. Tadpoles that are smaller at metamorphosis may be less likely
to survive (or reproduce) than those that are larger.

That is, there are two influences on the ideal tadpole size, both survivability at life stages in
nature:
```
MetaSurv -> TadpoleSize <- LarvSurv
```

The species (apparently) has to tradeoff size to survive both stages.
)")

display_markdown("
**13M2.** Compare the models you fit just above, using WAIC. Can you reconcile the differences in
WAIC with the posterior distributions of the models?

**Answer.** For comparison and to check on warnings, these results include `PSIS` deviances as well.
Notice the model with the predator predictor does well in both comparisons. This predictor
significantly improves predictive accuracy at the cost of only one parameter. Generally speaking the
model with only the size predictor does not do as well, because (alone) this is not an effective
predictor of whether the tadpole will survive to metamorphosis.

In general there are not large differences between these models, however, considering the error bars
produced by both WAIC and PSIS.
")

iplot(function() {
  plot(compare(m13.2, m_rf_df_pred, m_rf_df_size, m_rf_df_interaction))
}, ar=3.5)
display_markdown("Raw data (preceding plot):")
display(compare(m13.2, m_rf_df_pred, m_rf_df_size, m_rf_df_interaction), mimetypes="text/plain")

p_comp = compare(m13.2, m_rf_df_pred, m_rf_df_size, m_rf_df_interaction, func=PSIS)
iplot(function() {
  plot(p_comp)
}, ar=3.5)
display_markdown("Raw data (preceding plot):")
display(p_comp, mimetypes="text/plain")

source('practice-tank-cluster-priors.R')
source('practice-extra-parameter-chimpanzees.R')
source('practice-prior-data-conflict.R')
source('practice-bengali-contraception.R')
# source('practice-multilevel-trolley.R')
