source('iplot.R')
library(rethinking)

display_markdown("
**12H3.** In the data, there are two measures of a hurricane’s potential to cause death:
`damage_norm` and `min_pressure`. Consult `?Hurricanes` for their meanings. It makes some sense to
imagine that femininity of a name matters more when the hurricane is itself deadly. This implies an
interaction between femininity and either or both of `damage_norm` and `min_pressure`. Fit a series
of models evaluating these interactions. Interpret and compare the models. In interpreting the
estimates, it may help to generate counterfactual predictions contrasting hurricanes with masculine
and feminine names. Are the effect sizes plausible?
")

data(Hurricanes)
hur <- Hurricanes
hur$index = 1:92
hur$StdFem = standardize(hur$femininity)
hur$StdDam = standardize(hur$damage_norm)
hur$StdPres = standardize(hur$min_pressure)

display_markdown("
**Answer.** To get some unreasonable answers, lets go back to the Poisson distribution rather than
the Gamma-Poisson. It's likely this is what the authors of the original paper were using.
")

hur_dat = list(
  Deaths = hur$deaths,
  StdFem = hur$StdFem,
  StdDam = hur$StdDam
)

m_fem_dam_inter <- quap(
  alist(
    Deaths ~ dpois(lambda),
    log(lambda) <- a + bFem * StdFem + bDam * StdDam + bFemDamInter * StdFem * StdDam,
    a ~ dnorm(3, 0.5),
    bFem ~ dnorm(0, 1.0),
    bDam ~ dnorm(0, 1.0),
    bFemDamInter ~ dnorm(0, 0.4)
  ),
  data = hur_dat
)

display_markdown("
In the first model, lets assume an interaction between `femininity` and `damage_norm`. Damage is
unsurprisingly associated with more deaths. The model also believes there is a minor interaction
between damage and femininity, where together they increase deaths more than either alone.
")

iplot(function() {
  plot(precis(m_fem_dam_inter), main="m_fem_dam_inter")
}, ar=4.5)

hur_dat = list(
  Deaths = hur$deaths,
  StdFem = hur$StdFem,
  StdPres = hur$StdPres
)

m_fem_pres_inter <- quap(
  alist(
    Deaths ~ dpois(lambda),
    log(lambda) <- a + bFem * StdFem + bPres * StdPres + bFemPresInter * StdFem * StdPres,
    a ~ dnorm(3, 0.5),
    bFem ~ dnorm(0, 1.0),
    bPres ~ dnorm(0, 1.0),
    bFemPresInter ~ dnorm(0, 0.4)
  ),
  data = hur_dat
)

display_markdown("
In the second model, lets assume an interaction between `femininity` and `min_pressure`. Pressure is
unsurprisingly negatively associated with more deaths; lower pressure indicates a stronger storm.
There also appears to be a minor interaction between pressure and femininity, though now the
association is in the opposite direction. That is, when both pressure and femininity are positive,
which means the storm is weak and feminine, more deaths occur.
")

iplot(function() {
  plot(precis(m_fem_pres_inter), main="m_fem_pres_inter")
}, ar=4.5)

hur_dat = list(
  Deaths = hur$deaths,
  StdFem = hur$StdFem,
  StdPres = hur$StdPres,
  StdDam = hur$StdDam
)

m_fem_dam_pres_inter <- quap(
  alist(
    Deaths ~ dpois(lambda),
    log(lambda) <- a + bFem * StdFem + bPres * StdPres + bDam * StdDam +
        bFemPresInter * StdFem * StdPres +
        bFemDamInter * StdFem * StdDam +
        bDamPresInter * StdDam * StdPres +
        bFDPInter * StdPres * StdDam * StdPres,
    a ~ dnorm(3, 0.5),
    bFem ~ dnorm(0, 1.0),
    bDam ~ dnorm(0, 1.0),
    bPres ~ dnorm(0, 1.0),
    bFemPresInter ~ dnorm(0, 0.4),
    bFemDamInter ~ dnorm(0, 0.4),
    bDamPresInter ~ dnorm(0, 0.4),
    bFDPInter ~ dnorm(0, 0.4)
  ),
  data = hur_dat
)

display_markdown("
In the third model, lets assume interactions between all three predictors. The influence of
femininity has dropped significantly. The three-way interaction term is hard to interpret on the
parameter scale. Lets try to interpret it on the outcome scale.
")

iplot(function() {
  plot(precis(m_fem_dam_pres_inter), main="m_fem_dam_pres_inter")
}, ar=3.5)

display_markdown("
[ept]: https://en.wikipedia.org/wiki/Polyptych

To some extent this is a counterfactual plot rather than just a posterior prediction plot because
we're considering extremes of at least some predictors. Still, many of the predictors vary here only
within the range of the data.

In the following [Enneaptych][ept], we hold damage and pressure at typical values and vary
femininity. We observe the effect on deaths. Many of the effect sizes are inplausible. In the
bottom-right plot, notice an implication of the non-negative three-way interaction parameter: when
pressure is high and damage is low femininity has a dramatic effect on deaths.
")

iplot(function() {
  par(mfrow=c(3,3))
  for (d in -1: 1) {
    for (p in -1:1) {
      sim_dat = data.frame(StdPres=p, StdDam=d, StdFem=seq(from=-2, to=2, length.out=30))
      s <- sim(m_fem_dam_pres_inter, data=sim_dat)
      plot(sim_dat$StdFem, colMeans(s),
        main=paste("StdPres = ", p, ",", "StdDam = ", d),
        type="l", ylab="Deaths", xlab="StdFem")
    }
  }
})

display_markdown("
Ignoring the known outliers, PSIS believes the full interaction model performs best:
")
iplot(function() {
  plot(compare(m_fem_dam_pres_inter, m_fem_pres_inter, m_fem_dam_inter, func=PSIS))
}, ar=4.5)
