library(rethinking)

## R code 11.24
data(chimpanzees)
d <- chimpanzees
d$treatment <- 1 + d$prosoc_left + 2 * d$condition
d$side <- d$prosoc_left + 1 # right 1, left 2
d$cond <- d$condition + 1 # no partner 1, partner 2
d_aggregated <- aggregate(
  d$pulled_left,
  list(
    treatment = d$treatment, actor = d$actor,
    side = d$side, cond = d$cond
  ),
  sum
)
colnames(d_aggregated)[5] <- "left_pulls"
