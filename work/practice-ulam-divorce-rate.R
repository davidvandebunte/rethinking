library(rethinking)
source("iplot.R")

source("load-divorce-data.R")

display_markdown("
**9H2.** Recall the divorce rate example from Chapter 5. Repeat that analysis, using ulam this time,
fitting models m5.1, m5.2, and m5.3. Use compare to compare the models on the basis of WAIC or PSIS.
To use WAIC or PSIS with ulam, you need add the argument log_log=TRUE. Explain the model comparison
results.

**ERROR**: Replace `log_log` in the question with `log_lik`
<br/>
")

m5.1u <- ulam(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bA * A,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = div_slim,
  log_lik=TRUE
)

m5.2u <- ulam(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM * M,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = div_slim,
  log_lik=TRUE
)

m5.3u <- ulam(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM * M + bA * A,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = div_slim,
  log_lik=TRUE
)

iplot(function() {
  plot(compare(m5.1, m5.2, m5.3))
}, ar=4.5)

iplot(function() {
  plot(compare(m5.1, m5.2, m5.3, func=PSIS))
}, ar=4.5)

display_markdown("Models fit using `ulam`:")

iplot(function() {
  plot(compare(m5.1u, m5.2u, m5.3u))
}, ar=4.5)

iplot(function() {
  plot(compare(m5.1u, m5.2u, m5.3u, func=PSIS))
}, ar=4.5)

display_markdown("
**Answer.** In Chapter 5, further analysis showed the DAG describing these three variables was most
likely `D <- A -> M`. This fits the poor performance of the model (`m5.2`) which is making
predictions based on only `M`; it only has indirect information (through `A`) to predict `D`.

The unnecessary additional predictor `M` is likely hurting the predictive performance of the
multiple regression model (`m5.3`). Without a lot of data it's likely we're only overfitting to the
noise in `M` rather than gaining any predictive accuracy from it (see AIC's parameter penalty).
")
