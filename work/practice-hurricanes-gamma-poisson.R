source('iplot.R')
library(rethinking)

display_markdown("
**12H2.** Counts are nearly always over-dispersed relative to Poisson. So fit a gamma-Poisson (aka
negative-binomial) model to predict `deaths` using `femininity`. Show that the over-dispersed model
no longer shows as precise a positive association between femininity and deaths, with an 89% interval
that overlaps zero. Can you explain why the association diminished in strength?
")

data(Hurricanes)
hur <- Hurricanes
hur$index = 1:92
hur$StdFem = standardize(hur$femininity)

hur_dat = list(
  Deaths = hur$deaths,
  Femininity = hur$StdFem
)

m_hur_gamma_pois <- map(
  alist(
    Deaths ~ dgampois(lambda, phi),
    log(lambda) <- a + bFemininity * Femininity,
    a ~ dnorm(3, 0.5),
    bFemininity ~ dnorm(0, 1.0),
    phi ~ dexp(1)
  ),
  data = hur_dat
)

display_markdown("
**Answer.** The model no longer infers an association of deaths with femininity:
")

iplot(function() {
  plot(precis(m_hur_gamma_pois), main="m_hur_gamma_pois")
}, ar=4.5)

display_markdown("
The association has diminished in strength because the influence of outliers are diminished. Notice
the storms with the greatest death toll are female in the original data: Diane, Camille, and Sandy.
")
