library(rethinking)
source('iplot.R')

display_markdown("
**9H3.** Sometimes changing a prior for one parameter has unanticipated effects on other parameters.
This is because when a parameter is highly correlated with another parameter in the posterior, the
prior influences both parameters. Here’s an example to work and think through.

Go back to the leg length example in Chapter 6 and use the code there to simulate height and leg
lengths for 100 imagined individuals. Below is the model you fit before, resulting in a highly
correlated posterior for the two beta parameters. This time, fit the model using ulam:

R code 9.29

```R
m5.8s <- ulam(
 alist(
  height ~ dnorm( mu , sigma ) ,
  mu <- a + bl*leg_left + br*leg_right ,
  a ~ dnorm( 10 , 100 ) ,
  bl ~ dnorm( 2 , 10 ) ,
  br ~ dnorm( 2 , 10 ) ,
  sigma ~ dexp( 1 )
 ) , data=d, chains=4,
 start=list(a=10,bl=0,br=0.1,sigma=1) )
```
<br/>

Compare the posterior distribution produced by the code above to the posterior distribution produced
when you change the prior for br so that it is strictly positive:

R code 9.30

```R
m5.8s2 <- ulam(
 alist(
  height ~ dnorm( mu , sigma ) ,
  mu <- a + bl*leg_left + br*leg_right ,
  a ~ dnorm( 10 , 100 ) ,
  bl ~ dnorm( 2 , 10 ) ,
  br ~ dnorm( 2 , 10 ) ,
  sigma ~ dexp( 1 )
 ) , data=d, chains=4,
 constraints=list(br='lower=0'),
 start=list(a=10,bl=0,br=0.1,sigma=1) )
```
<br/>

Note the `constraints` list. What this does is constrain the prior distribution of br so that it has
positive probability only above zero. In other words, that prior ensures that the posterior
distribution for br will have no probability mass below zero. Compare the two posterior
distributions for m5.8s and m5.8s2. What has changed in the posterior distribution of both beta
parameters? Can you explain the change induced by the change in prior?

**ERROR**: These model names should be based on `m6.1` rather than `m5.8`.
")

source('simulate-height-leg-lengths.R')

## R code 9.29
m5.8s <- ulam(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bl * leg_left + br * leg_right,
    a ~ dnorm(10, 100),
    bl ~ dnorm(2, 10),
    br ~ dnorm(2, 10),
    sigma ~ dexp(1)
  ),
  data = d, chains = 4, cores = 4,
  log_lik = TRUE,
  start = list(a = 10, bl = 0, br = 0.1, sigma = 1)
)

## R code 9.30
m5.8s2 <- ulam(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bl * leg_left + br * leg_right,
    a ~ dnorm(10, 100),
    bl ~ dnorm(2, 10),
    br ~ dnorm(2, 10),
    sigma ~ dexp(1)
  ),
  data = d, chains = 4, cores = 4,
  log_lik = TRUE,
  constraints = list(br = "lower=0"),
  start = list(a = 10, bl = 0, br = 0.1, sigma = 1)
)

iplot(function() {
  plot(precis(m5.8s), main="m5.8s")
}, ar=4.0)

iplot(function() {
  plot(precis(m5.8s2), main="m5.8s2")
}, ar=4.0)

display_markdown(r"(
**Answer.** The $\beta_l$ parameter inference has adjusted to the $\beta_r$ prior by becoming
negative.
)")

display_markdown(r"(
**9H4.** For the two models fit in the previous problem, use WAIC or PSIS to compare the effective
numbers of parameters for each model. You will need to use `log_lik=TRUE` to instruct `ulam` to
compute the terms that both WAIC and PSIS need. Which model has more effective parameters? Why?

**Answer.** The first model (`m5.8s`) has more effective parameters. The number of effective
parameters is based on variance in the posterior. By adding a prior we've apparently limited the
overfitting to noise that happens in the first model, leading to less variance in both $\beta_l$ and
$\beta_r$ and therefore less variance in the posterior.
)")

iplot(function() {
  plot(compare(m5.8s, m5.8s2))
}, ar=4.5)

display_markdown("m5.8s:")
display(WAIC(m5.8s), mimetypes="text/plain")

display_markdown("<br/> m5.8s2:")
display(WAIC(m5.8s2), mimetypes="text/plain")

# post <- extract.samples(m5.8s2)
# n_samples <- 1000
# logprob <- sapply(1:n_samples,
#   function(s) {
#     mu <- post$a[s] + post$bl[s]*d$leg_left + post$br[s]*d$leg_right
#     dnorm(d$height, mu, post$sigma[s], log=TRUE)
#   }
# )
# n_cases <- nrow(d)
# pWAIC <- sapply(1:n_cases, function(i) var(logprob[i,]) )
# display(sum(pWAIC))
