library(IRdisplay)

display_markdown("
**2M1.** Recall the globe tossing model from the chapter. Compute and plot the grid approximate
posterior distribution for each of the following sets of observations. In each case, assume a
uniform prior for p.
")

# Uniform prior
prior <- rep(1, 20)

# 20-point grid
p_grid <- seq(from=0, to=1, length.out=20)

iplot <- function(plot_func) {
    # https://stackoverflow.com/a/35805352/622049
    png("/tmp/plot.png", width=740, height=620, res=120)
    plot_func()
    dev.off()
    display_png(file="/tmp/plot.png")
}

plot_posterior <- function(prior, likelihood) {
    # compute product of likelihood and prior
    unstd.posterior <- likelihood * prior
    # standardize the posterior, so it sums to 1
    posterior <- unstd.posterior / sum(unstd.posterior)
    iplot(function() {
        plot(p_grid, posterior, type="b", xlab="probability of water", ylab="posterior probability")
    })
}

display_markdown("`(1) W, W, W`")
likelihood <- dbinom(3, size=3, prob=p_grid)
plot_posterior(prior, likelihood)

display_markdown("`(2) W, W, W, L`")
likelihood <- dbinom(3, size=4, prob=p_grid)
plot_posterior(prior, likelihood)

display_markdown("`(3) L, W, W, L, W, W, W`")
likelihood <- dbinom(5, size=7, prob=p_grid)
plot_posterior(prior, likelihood)

display_markdown("
**2M2.** Now assume a prior for p that is equal to zero when p < 0.5 and is a positive constant when
p ≥ 0.5. Again compute and plot the grid approximate posterior distribution for each of the sets of
observations in the problem just above.
")

# Prior similar to "R code 2.5" on pg. 41.
# TODO: Why does "R code 2.5" not integrate to one?
prior <- ifelse(p_grid < 0.5, 0, 1)

display_markdown("`(1) W, W, W`")
likelihood <- dbinom(3, size=3, prob=p_grid)
plot_posterior(prior, likelihood)

display_markdown("`(2) W, W, W, L`")
likelihood <- dbinom(3, size=4, prob=p_grid)
plot_posterior(prior, likelihood)

display_markdown("`(3) L, W, W, L, W, W, W`")
likelihood <- dbinom(5, size=7, prob=p_grid)
plot_posterior(prior, likelihood)

display_markdown("
**2M3.** Suppose there are two globes, one for Earth and one for Mars. The Earth globe is 70% covered
in water. The Mars globe is 100% land. Further suppose that one of these globes—you don’t know
which—was tossed in the air and produced a “land” observation. Assume that each globe was equally
likely to be tossed. Show that the posterior probability that the globe was the Earth, conditional on
seeing “land” (Pr(Earth|land)), is 0.23.

**Solution**

From a Bayesian perspective, we have a multi-level model where the first level (selecting the globe)
is a Bernoulli trial with `p₁ = 0.5`. The second level is also a Bernoulli trial, where the value of
`p` depends on the globe selected in the first level.

There are no unobservable variables in this problem; we know the probabilities for both levels of
the model. We could also look at this as an example of fixing the parameters, like one part of grid
search. In the language of machine learning this is running 'inference' (making a prediction) on the
input of 'land' to the model.

Mechanically:
```
Pr(Earth|land) = Pr(Earth, land) / Pr(land)
Pr(Earth, land) = 0.5*0.3 = 0.15
Pr(land) = 0.5*0.3 + 0.5*1.0 = 0.65
Pr(Earth|land) = 0.15 / 0.65 = 0.23
```
")

display_markdown("
**2M4.** Suppose you have a deck with only three cards. Each card has two sides, and each side is either
black or white. One card has two black sides. The second card has one black and one white side. The
third card has two white sides. Now suppose all three cards are placed in a bag and shuffled. Someone
reaches into the bag and pulls out a card and places it flat on a table. A black side is shown facing up,
but you don’t know the color of the side facing down. Show that the probability that the other side is
also black is 2/3. Use the counting method (Section 2 of the chapter) to approach this problem. This
means counting up the ways that each card could produce the observed data (a black side facing up
on the table).

**Solution**

| Outcome | Ways to produce data | Plausibility |
| ---     | ---                  | ---          |
| `BB`    | 2                    | 2/3          |
| `BW`    | 1                    | 1/3          |
| `WW`    | 0                    | 0/3          |
")

display_markdown("
**2M5.** Now suppose there are four cards: B/B, B/W, W/W, and another B/B. Again suppose a card is
drawn from the bag and a black side appears face up. Again calculate the probability that the other
side is black.

**Solution**

| Outcome | Ways to produce data | Plausibility |
| ---     | ---                  | ---          |
| `BB`    | 2                    | 2/5          |
| `BW`    | 1                    | 1/5          |
| `WW`    | 0                    | 0/5          |
| `BB`    | 2                    | 2/5          |

2/5 + 2/5 = 4/5
")

display_markdown("
**2M6.** Imagine that black ink is heavy, and so cards with black sides are heavier than cards with
white sides. As a result, it’s less likely that a card with black sides is pulled from the bag. So
again assume there are three cards: B/B, B/W, and W/W. After experimenting a number of times, you
conclude that for every way to pull the B/B card from the bag, there are 2 ways to pull the B/W card
and 3 ways to pull the W/W card. Again suppose that a card is pulled and a black side appears face
up. Show that the probability the other side is black is now 0.5. Use the counting method, as
before.

**Solution**
")

ways_to_produce = c(1, 2, 3)
names(ways_to_produce) <- c("bb", "bw", "ww")
ways_to_produce = ways_to_produce * c(2, 1, 0)
normalized = ways_to_produce / sum(ways_to_produce)
display(normalized)

display_markdown("
**2M7.** Assume again the original card problem, with a single card showing a black side face up.
Before looking at the other side, we draw another card from the bag and lay it face up on the table.
The face that is shown on the new card is white. Show that the probability that the first card, the
one showing a black side, has black on its other side is now 0.75. Use the counting method, if you
can. Hint: Treat this like the sequence of globe tosses, counting all the ways to see each
observation, for each possible first card.

**Solution**
")

ways_to_produce = c(2, 1, 0)
names(ways_to_produce) <- c("bb", "bw", "ww")
# If the first card was `bb` we can produce a white face in three ways:
# - One side of the `bw` card.
# - Two sides of the `ww` card.
#
# If the first card was `bw` we can produce a white face in only two ways, the two sides of the `ww`
# card.
ways_to_produce = ways_to_produce * c(3, 2, 0)
normalized = ways_to_produce / sum(ways_to_produce)
display(normalized)
