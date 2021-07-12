source("iplot.R")

display_markdown(r"(
**11H2.** The data contained in `library(MASS);data(eagles)` are records of salmon pirating attempts
by Bald Eagles in Washington State. See `?eagles` for details. While one eagle feeds, sometimes
another will swoop in and try to steal the salmon from it. Call the feeding eagle the “victim” and
the thief the “pirate.” Use the available data to build a binomial GLM of successful pirating
attempts.

(a) Consider the following model:
$$
y_i ∼ Binomial(n_i, p_i) \\
logit(p_i) = \alpha + \beta_P P_i + \beta_V V_i + \beta_A A_i \\
α ∼ Normal(0, 1.5) \\
\beta_P, \beta_V, \beta_A ∼ Normal(0, 0.5)
$$

where $y$ is the number of successful attempts, $n$ is the total number of attempts, $P$ is a dummy
variable indicating whether or not the pirate had large body size, $V$ is a dummy variable
indicating whether or not the victim had large body size, and finally $A$ is a dummy variable
indicating whether or not the pirate was an adult. Fit the model above to the `eagles` data, using
both `quap` and `ulam`. Is the quadratic approximation okay?

**Answer.** The quadratic approximation is OK because this model includes pre-selected reasonable
priors.
)")

library(rethinking)
library(MASS)
data(eagles)
eag <- eagles
eag$Pi <- ifelse(eagles$P == "L", 1, 0)
eag$Vi <- ifelse(eagles$V == "L", 1, 0)
eag$Ai <- ifelse(eagles$A == "A", 1, 0)

mq_pirate_success <- quap(
  alist(
    y ~ dbinom(n, p),
    logit(p) <- a + b_P*Pi + b_V*Vi + b_A*Ai,
    a ~ dnorm(0, 1.5),
    b_P ~ dnorm(0, 0.5),
    b_V ~ dnorm(0, 0.5),
    b_A ~ dnorm(0, 0.5)
  ),
  data = eag
)

dat_eag <- list(
  n = eag$n,
  y = eag$y,
  Pi = eag$Pi,
  Vi = eag$Vi,
  Ai = eag$Ai
)

mu_pirate_success <- ulam(
  mq_pirate_success@formula,
  data = dat_eag, chains=4, cores=4, log_lik=TRUE
)

iplot(function() {
  plot(compare(mq_pirate_success, mu_pirate_success))
}, ar=4.5)

display_markdown(r"(
(b) Now interpret the estimates. If the quadratic approximation turned out okay, then it’s okay to
use the `quap` estimates. Otherwise stick to `ulam` estimates. Then plot the posterior predictions.
Compute and display both (1) the predicted **probability** of success and its 89% interval for each
row (i) in the data, as well as (2) the predicted success **count** and its 89% interval. What
different information does each type of posterior prediction provide?

**Answer.** The `eagles` data frame:
)")

display(eagles)

display_markdown("")
display_markdown(r"(
This graph is from `postcheck`, as mentioned in the text. The documentation on this function isn't
too helpful; it's easier to read the source code. In the following figure, the open circles
(**$mean** in the raw data) represent the predicted probability of success and the vertical lines
the 89% interval (**$PI**). The crosshairs represent the 89% interval of the predicted success count
(**$outPI**). The purple dots are observed outcomes from the `eagles` data frame above. Notice
outcomes are scaled to between zero and one; in case #1 the purple dot is at $17/24 \approx 0.7083$.
)")

iplot(function() {
  result <- postcheck(mu_pirate_success)
  display_markdown("The raw data:")
  display(result)
})


display_markdown(r"(
In part (1) we're visualizing model parameters (including uncertainty). In part (2) we're
visualizing implied predictions on the outcome scale (including uncertainty) which indirectly
includes the uncertainty in model parameters from the first part.
)")

display_markdown(r"(
(c) Now try to improve the model. Consider an interaction between the pirate’s size and age
(immature or adult). Compare this model to the previous one, using WAIC. Interpret.

**Answer.** The non-interaction model appears to do slightly better, but generally speaking the
performance is almost the same. The interaction model is more complex (has more parameters) and that
might slightly hurt its score.
)")

eag$PirateTreatment <- 1 + eag$Pi + eag$Ai

dat_eag_inter <- list(
  n = eag$n,
  y = eag$y,
  PirateTreatment = eag$PirateTreatment,
  Vi = eag$Vi
)

mu_pirate_success_inter <- ulam(
  alist(
    y ~ dbinom(n, p),
    logit(p) <- a + b_t[PirateTreatment] + b_V*Vi,
    a ~ dnorm(0, 1.5),
    b_t[PirateTreatment] ~ dnorm(0, 0.5),
    b_V ~ dnorm(0, 0.5)
  ),
  data = dat_eag_inter, chains=4, cores=4, log_lik=TRUE
)

iplot(function() {
  plot(compare(mu_pirate_success_inter, mu_pirate_success))
}, ar=4.5)
