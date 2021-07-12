library(rethinking)

source("iplot.R")

display_markdown("
Suppose the globe tossing data had turned out to be 8 water in 15 tosses. Construct the posterior
distribution, using grid approximation. Use the same flat prior as before.")

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
iplot(function() {
    plot(p_grid, posterior, type="l", xlab="probability of water", ylab="posterior probability")
})

display_markdown("
Draw 10,000 samples from the grid approximation from above. Then use the samples to calculate the
90% HPDI for p.")
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
display(HPDI( samples, prob=0.9 ))
# TODO: But "the are hardly" ever sensible on pg. 85.
# TODO: "This density, as well as the individual densities for mu and sigma, as shown in" on pg. 85
iplot(function() {
    dens(samples, show.HPDI=0.9)
})

display_markdown("
**3M3.** Construct a posterior predictive check for this model and data. This means simulate the
distribution of samples, averaging over the posterior uncertainty in p. What is the probability of
observing 8 water in 15 tosses? ")
w <- rbinom( 1e4, size=15, prob=samples )
display(sum(w == 8) / length(samples))
iplot(function() {
    simplehist(w, xlab="number of water samples", ylab="posterior prediction distribution")
})

display_markdown("
**3M4.** Using the posterior distribution constructed from the new (8/15) data, now calculate the
probability of observing 6 water in 9 tosses.")
w <- rbinom( 1e4, size=9, prob=samples )
display(sum(w == 6) / length(samples))
iplot(function() {
    simplehist(w, xlab="number of water samples", ylab="posterior prediction distribution")
})

display_markdown("
**3M5.** Start over at 3M1, but now use a prior that is zero below `p = 0.5` and a constant above `p
= 0.5`. This corresponds to prior information that a majority of the Earth’s surface is water.
Repeat each problem above and compare the inferences. What difference does the better prior make? If
it helps, compare inferences (using both priors) to the true value p = 0.7.")
prior <- ifelse(p_grid < 0.5, 0, 1)
likelihood <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
iplot(function() {
    plot(p_grid, posterior, type="l", xlab="probability of water", ylab="posterior probability")
})
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
display(HPDI( samples, prob=0.9 ))
iplot(function() {
    dens(samples, show.HPDI=0.9)
})
w <- rbinom( 1e4, size=15, prob=samples )
display(sum(w == 8) / length(samples))
iplot(function() {
    simplehist(w, xlab="number of water samples", ylab="posterior prediction distribution")
})
w <- rbinom( 1e4, size=9, prob=samples )
display(sum(w == 6) / length(samples))
iplot(function() {
    simplehist(w, xlab="number of water samples", ylab="posterior prediction distribution")
})

display_markdown("
**3M6.** Suppose you want to estimate the Earth’s proportion of water very precisely. Specifically,
you want the 99% percentile interval of the posterior distribution of p to be only 0.05 wide. This
means the distance between the upper and lower bound of the interval should be 0.05. How many times
will you have to toss the globe to do this?")
# TODO: Finish this problem.
#
# This problem asks you to take your model and determine how much data it needs to reach a certain
# level of "confidence" (or should you use the term "compatible"?). It's related to some of the more
# fundamental questions you've had in the past about e.g. how many images you need in signs before
# you start preferring quality to quantity.
#
# If you assume your model is totally correct, you can assume your data (your globe tosses) are
# coming from a generative process that matches your model. Every new sample applies a new
# likelihood function to the posterior making narrower as you are targeting. How do you know what
# these likelihood functions look like in general, though? Well, they're going to come from `dbinom`
# in the case; how many times do you need to "square" the likelihood? Assuming you start from a flat
# prior.
# - https://en.wikipedia.org/wiki/Binomial_distribution
#
# How would you do this experimental/hard way? That is, through simulation.
#
# Doesn't this depend on the prior? You have to assume a flat prior, it seems. Why not assume a
# prior that is already correct, that is, that already has the required shape?
#
# Doesn't this depend on the value of p as well?
#
# The answer you're looking for here is the N in the binomial distribution; that's the number of
# tosses required.
