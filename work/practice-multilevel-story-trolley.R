library(rethinking)
source("iplot.R")

display_markdown(r"(
**13H3.** The `Trolley` data are also clustered by story, which indicates a unique narrative for
each vignette. Define and fit a cross-classified varying intercepts model with both `id` and
`story`. Use the same ordinary terms as in the previous problem. Compare this model to the previous
models. What do you infer about the impact of different stories on responses?

**Answer.** Sampling from the suggested model:
)")

data(Trolley)
t_df <- Trolley

t_story_id_dat <- list(
  R = t_df$response,
  A = t_df$action,
  I = t_df$intention,
  id = t_df$id,
  story = as.integer(t_df$story),
  C = t_df$contact
)
m_trolley_story_id <- ulam(
  alist(
    R ~ dordlogit(phi, cutpoints),
    phi <- a[id] + b[story] + bA * A + bC * C + BI * I,
    a[id] ~ dnorm(0, sigma_a),
    sigma_a ~ dexp(1),
    b[story] ~ dnorm(0, sigma_b),
    sigma_b ~ dexp(1),
    BI <- bI + bIA * A + bIC * C,
    c(bA, bI, bC, bIA, bIC) ~ dnorm(0, 0.5),
    cutpoints ~ dnorm(0, 1.5)
  ),
  data = t_story_id_dat, chains = 4, cores = 4, log_lik = TRUE, iter=800
)
display(precis(m_trolley_story_id, depth = 2), mimetypes="text/plain")
iplot(function() {
  plot(precis(m_trolley_story_id, depth=2), xlim=c(-6,8), main='m_trolley_story_id')
}, ar=0.18)

display_markdown("
Comparing the models using WAIC:
")
iplot(function() {
  plot(compare(m12.5, m_trolley_id, m_trolley_story_id))
}, ar=4.5)

display_markdown("
Comparing the models using PSIS, to check for overfitting or outliers:
")
iplot(function() {
  plot(compare(m12.5, m_trolley_id, m_trolley_story_id, func=PSIS))
}, ar=4.5)

display_markdown(r"(
With this new predictor, our inferences about `bIC`, `bA` and similar parameters have changed
more significantly. Presumably these individual stories provide even more detail than the contact,
action, etc. indicators and are potentially redundant or non-identifiable with respect to the
parameters attached to the indicator variables.

Still, this extra detail seems to provide significant predictive power according to WAIC and PSIS,
though not to the same degree that the individual variable helped prediction.
)")
