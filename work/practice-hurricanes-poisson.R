source('iplot.R')
library(rethinking)

display_markdown("
**12H1.** In 2014, a paper was published that was entitled “Female hurricanes are deadlier than male
hurricanes.” As the title suggests, the paper claimed that hurricanes with female names have caused
greater loss of life, and the explanation given is that people unconsciously rate female hurricanes
as less dangerous and so are less likely to evacuate.

Statisticians severely criticized the paper after publication. Here, you’ll explore the complete
data used in the paper and consider the hypothesis that hurricanes with female names are deadlier.
Load the data with:

R code 12.38

```R
library(rethinking)
data(Hurricanes)
```

Acquaint yourself with the columns by inspecting the help `?Hurricanes`. In this problem, you’ll
focus on predicting `deaths` using `femininity` of each hurricane’s name. Fit and interpret the
simplest possible model, a Poisson model of `deaths` using `femininity` as a predictor. You can use
`map` or `ulam`. Compare the model to an intercept-only Poisson model of deaths. How strong is the
association between femininity of name and deaths? Which storms does the model fit (retrodict) well?
Which storms does it fit poorly?

**Answer.** The `Hurricanes` data.frame with an index and a standardized femininity:
")

data(Hurricanes)
hur <- Hurricanes
hur$index = 1:92
hur$StdFem = standardize(hur$femininity)
display(hur)

hur_dat = list(
  Deaths = hur$deaths,
  Femininity = hur$StdFem
)

m_hur_pois <- map(
  alist(
    Deaths ~ dpois(lambda),
    log(lambda) <- a + bFemininity * Femininity,
    a ~ dnorm(3, 0.5),
    bFemininity ~ dnorm(0, 1.0)
  ),
  data = hur_dat
)

m_hur_intercept <- map(
  alist(
    Deaths ~ dpois(lambda),
    log(lambda) <- a,
    a ~ dnorm(3, 0.5)
  ),
  data = hur_dat
)

display_markdown("
<br/>
The model with the femininity predictor appears to have inferred a minor but significant association
of deaths with femininity:
")

iplot(function() {
  plot(precis(m_hur_pois), main="m_hur_pois")
}, ar=4.5)

display_markdown("
<br/>
The model with the femininity predictor performs only marginally better on WAIC than the
intercept-only model:
")
iplot(function() {
  plot(compare(m_hur_pois, m_hur_intercept))
}, ar=4.5)

display_markdown("
<br/>
[od]: https://en.wikipedia.org/wiki/Overdispersion

The model is not accurate when it attempts to retrodict the death toll on storms with many deaths.
The predictions it makes are less dispersed than the data, that is, the data are overdispersed
relative to the predictions ([Overdispersion][od]).
")

iplot(function() {
  result <- postcheck(m_hur_pois, window=92)
  display_markdown("The raw data:")
  display(result)
})
