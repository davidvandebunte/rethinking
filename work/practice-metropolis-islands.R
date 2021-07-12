source('iplot.R')

display_markdown("
**9H5.** Modify the Metropolis algorithm code from the chapter to handle the case that the island
populations have a different distribution than the island labels. This means the island’s number
will not be the same as its population.

**Answer.** The new island populations are proportional to:

```R
island_pop_prop = sqrt(island_number)
```
")

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
  prob_move <- sqrt(proposal) / sqrt(current)
  current <- ifelse(runif(1) < prob_move, proposal, current)
}

iplot(function() {
  par(mfrow=c(1,2))
  plot(1:100, positions[1:100], main="Sample positions", xlab="week", ylab="island")
  plot(table(positions), main="Full distribution", xlab="island", ylab="number of weeks")
}, ar=1.8)
