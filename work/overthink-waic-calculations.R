library(rethinking)
library(IRdisplay)

## R code 7.19
data(cars)
m <- quap(
    alist(
        dist ~ dnorm(mu,sigma),
        mu <- a + b*speed,
        a ~ dnorm(0,100),
        b ~ dnorm(0,10),
        sigma ~ dexp(1)
    ) , data=cars )
set.seed(94)
post <- extract.samples(m,n=1000)

## R code 7.20
n_samples <- 1000
logprob <- sapply( 1:n_samples ,
    function(s) {
        mu <- post$a[s] + post$b[s]*cars$speed
        dnorm( cars$dist , mu , post$sigma[s] , log=TRUE )
    } )

## R code 7.21
n_cases <- nrow(cars)
lppd <- sapply( 1:n_cases , function(i) log_sum_exp(logprob[i,]) - log(n_samples) )

## R code 7.22
pWAIC <- sapply( 1:n_cases , function(i) var(logprob[i,]) )

## R code 7.23
-2*( sum(lppd) - sum(pWAIC) )

## R code 7.24
waic_vec <- -2*( lppd - pWAIC )
display(sqrt( n_cases*var(waic_vec) ))
