library(rethinking)
source("iplot.R")

display_markdown(r"(
**13H2.** Return to `data(Trolley)` from Chapter 12. Define and fit a varying intercepts model for
these data. Cluster intercepts on individual participants, as indicated by the unique values in the
`id` variable. Include `action`, `intention`, and `contact` as ordinary terms. Compare the varying
intercepts model and a model that ignores individuals, using both WAIC and posterior predictions.
What is the impact of individual variation in these data?

**Answer.** The `head` of the `Trolley` data.frame:
)")

data(Trolley)
t_df <- Trolley
display(head(t_df))

display_markdown(r"(
<br/>
The `help` on the `Trolley` data.frame:
)")
display(help(Trolley))

display_markdown(r"(
<br/>
A `summary` of the `Trolley` data.frame:
)")
display(summary(Trolley))

display_markdown(r"(
<br/>
Let's reproduce results for model `m12.5` in the text:
)")
t_dat <- list(
  R = t_df$response,
  A = t_df$action,
  I = t_df$intention,
  C = t_df$contact
)
m12.5 <- ulam(
  alist(
    R ~ dordlogit(phi, cutpoints),
    phi <- bA * A + bC * C + BI * I,
    BI <- bI + bIA * A + bIC * C,
    c(bA, bI, bC, bIA, bIC) ~ dnorm(0, 0.5),
    cutpoints ~ dnorm(0, 1.5)
  ),
  data = t_dat, chains = 4, cores = 4, log_lik = TRUE
)
display(precis(m12.5, depth = 2), mimetypes="text/plain")
iplot(function() {
  plot(precis(m12.5, depth=2), xlim=c(-6,8),main='m12.5')
}, ar=1.8)

plot_cutpoints <- function(model) {
  iplot(function() {
    par(mfrow=c(1,3))
    plot_intention <- function(kA, kC, model) {
      plot(NULL,
        type = "n", xlab = "intention", ylab = "probability",
        xlim = c(0, 1), ylim = c(0, 1), xaxp = c(0, 1, 1), yaxp = c(0, 1, 2),
        main=paste0("action=", kA, ", contact=", kC)
      )

      kI <- 0:1 # values of intention to calculate over
      pdat <- data.frame(A = kA, C = kC, I = kI, id=1)
      phi <- link(model, data = pdat)$phi

      post <- extract.samples(model)
      for (s in 1:50) {
        pk <- pordlogit(1:6, phi[s, ], post$cutpoints[s, ])
        for (i in 1:6) lines(kI, pk[, i], col = grau(0.1))
      }
    }
    plot_intention(0, 0, model)
    plot_intention(1, 0, model)
    plot_intention(0, 1, model)
  }, ar=2.2)
}

plot_hist <- function(model) {
  iplot(function() {
    par(mfrow=c(1,3))
    plot_intent <- function(kA, kC, model) {
        kI <- 0:1 # values of intention to calculate over
        pdat <- data.frame(A = kA, C = kC, I = kI, id=1)
        s <- sim(model, data = pdat)
        simplehist(s, xlab = "response", main=paste0("action=", kA, ", contact=", kC))
    }
    plot_intent(0, 0, model)
    plot_intent(1, 0, model)
    plot_intent(0, 1, model)
  }, ar=2.2)
}

plot_cutpoints(m12.5)
plot_hist(m12.5)

display_markdown(r"(
We'll use the following model with `id` clusters:

$$
\begin{align}
R_i & \sim OrderedLogit(\phi_i,\kappa) \\
\phi_i & = \alpha_{id[i]} + \ldots \\
\alpha_{id} & \sim Normal(0, \sigma), id = 1..331 \\
\sigma & \sim Exponential(1) \\
\end{align}
$$

Notice we do not provide a cluster mean $\bar{\alpha}$; this variable would be non-identifiable with
respect to the intercepts in the ordered logit. If you try to add this parameter, you'll see the
divergent transitions and poor sampling characteristic of this problem.

Sampling from the model with `id` clusters:
)")
t_id_dat <- list(
  R = t_df$response,
  A = t_df$action,
  I = t_df$intention,
  id = t_df$id,
  C = t_df$contact
)
m_trolley_id <- ulam(
  alist(
    R ~ dordlogit(phi, cutpoints),
    phi <- a[id] + bA * A + bC * C + BI * I,
    a[id] ~ dnorm(0, sigma),
    sigma ~ dexp(1),
    BI <- bI + bIA * A + bIC * C,
    c(bA, bI, bC, bIA, bIC) ~ dnorm(0, 0.5),
    cutpoints ~ dnorm(0, 1.5)
  ),
  data = t_id_dat, chains = 4, cores = 4, log_lik = TRUE, iter=800
)
display(precis(m_trolley_id, depth = 2), mimetypes="text/plain")

display_markdown(r"(
<br/>
Although it may seem excessive, here are all the `id` parameter estimates on the same x-axis scale
as the `precis` plot above for model `m12.5`:
)")
iplot(function() {
  plot(precis(m_trolley_id, depth=2), xlim=c(-6,8), main='m_trolley_id')
}, ar=0.2)

display_markdown(r"(
Notice the individual intercepts are much less certain than `sigma`, `bIC`, and the other
higher-level parameters. This uncertainty leads to the more variable posterior predictive plot in
the first triptych below. These two tripytch plots for a typical individual, specifically the
individual with `id` equal 1, who did not have a strong positive or negative tendency.
)")

plot_cutpoints(m_trolley_id)
plot_hist(m_trolley_id)

display_markdown("
Comparing the models using WAIC:
")
iplot(function() {
  plot(compare(m12.5, m_trolley_id))
}, ar=4.5)

display_markdown("
Comparing the models using PSIS, to check for overfitting or outliers:
")
iplot(function() {
  plot(compare(m12.5, m_trolley_id, func=PSIS))
}, ar=4.5)

display_markdown(r"(
The new model has a much lower WAIC/PSIS score because the new predictor explains much of the
unexplained variation in the previous model (notice the large inferred mean for `sigma`).
)")

source('practice-multilevel-story-trolley.R')
