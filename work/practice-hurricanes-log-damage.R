library(rethinking)
source('iplot.R')

display_markdown("
**12H4.** In the original hurricanes paper, storm damage (`damage_norm`) was used directly. This
assumption implies that mortality increases exponentially with a linear increase in storm strength,
because a Poisson regression uses a log link. So it’s worth exploring an alternative hypothesis:
that the logarithm of storm strength is what matters. Explore this by using the logarithm of
`damage_norm` as a predictor. Using the best model structure from the previous problem, compare a
model that uses `log(damage_norm)` to a model that uses `damage_norm` directly. Compare their
PSIS/WAIC values as well as their implied predictions. What do you conclude?

**Answer** We'll use the full interaction model as our baseline, though it probably doesn't matter
which model we start from.
")

data(Hurricanes)
hur <- Hurricanes
hur$index = 1:92
hur$StdFem = standardize(hur$femininity)
hur$StdDam = standardize(hur$damage_norm)
hur$StdLogDam = standardize(log(hur$damage_norm))
hur$StdPres = standardize(hur$min_pressure)

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
First we'll run `postcheck` on the baseline model. Notice there is more dispersion than in the plain
old Poisson model in **12H1**:
")

iplot(function() {
  result <- postcheck(m_fem_dam_pres_inter, window=92)
  display_markdown("The raw data:")
  display(result)
})

hur_dat = list(
  Deaths = hur$deaths,
  StdFem = hur$StdFem,
  StdPres = hur$StdPres,
  StdLogDam = hur$StdLogDam
)

m_fem_logdam_pres_inter <- quap(
  alist(
    Deaths ~ dpois(lambda),
    log(lambda) <- a + bFem * StdFem + bPres * StdPres + bDam * StdLogDam +
        bFemPresInter * StdFem * StdPres +
        bFemDamInter * StdFem * StdLogDam +
        bDamPresInter * StdLogDam * StdPres +
        bFDPInter * StdPres * StdLogDam * StdPres,
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
Ignoring the known outliers, comparing the models using PSIS, it appears the `log(damage)` model
improves performance:
")
iplot(function() {
  plot(compare(m_fem_dam_pres_inter, m_fem_logdam_pres_inter, func=PSIS))
}, ar=4.5)

display_markdown("
Run `postcheck` to visualize how much the model has changed on the outcome scale:
")

iplot(function() {
  result <- postcheck(m_fem_logdam_pres_inter, window=92)
  display_markdown("The raw data:")
  display(result)
})
