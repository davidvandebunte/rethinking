source('iplot.R')

# display_markdown("## 9.1. Good King Markov and his island kingdom")
set.seed(8)

## R code 9.1
num_weeks <- 1e5
positions <- rep(0, num_weeks)
current <- 10
for (i in 1:num_weeks) {
  ## record current position
  positions[i] <- current
  ## flip coin to generate proposal
  proposal <- current + sample(c(-1, 1), size = 1)
  ## now make sure he loops around the archipelago
  if (proposal < 1) proposal <- 10
  if (proposal > 10) proposal <- 1
  ## move?
  prob_move <- proposal / current
  current <- ifelse(runif(1) < prob_move, proposal, current)
}

#iplot(function() {
#  par(mfrow=c(1,2))
#  plot(1:100, positions[1:100], main="Different positions", xlab="week", ylab="island")
#  plot(table(positions), main="Same distribution", xlab="island", ylab="number of weeks")
#}, ar=1.8)

display_markdown("## 9.2. Metropolis algorithms

[cp]: https://en.wikipedia.org/wiki/Conjugate_prior

What the author refers to as *conjugate pairs* is more commonly known as a [Conjugate prior][cp].
")

## R code 9.4
library(rethinking)
D <- 10
T <- 1e3
Y <- rmvnorm(T, rep(0, D), diag(D))
rad_dist <- function(Y) sqrt(sum(Y^2))
Rd <- sapply(1:T, function(i) rad_dist(Y[i, ]))
# iplot(function() dens(Rd))
# source('overthink-hmc.R')
