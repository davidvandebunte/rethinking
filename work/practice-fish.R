source('iplot.R')
library(rethinking)

display_markdown("
**12H6.** The data in `data(Fish)` are records of visits to a national park. See `?Fish` for
details. The question of interest is how many fish an average visitor takes per hour, when fishing.
The problem is that not everyone tried to fish, so the `fish_caught` numbers are zero-inflated. As
with the monks example in the chapter, there is a process that determines who is fishing (working)
and another process that determines fish per hour (manuscripts per day), conditional on fishing
(working). We want to model both. Otherwise we’ll end up with an underestimate of rate of fish
extraction from the park.

You will model these data using zero-inflated Poisson GLMs. Predict `fish_caught` as a function
of any of the other variables you think are relevant. One thing you must do, however, is use a
proper Poisson offset/exposure in the Poisson portion of the zero-inflated model. Then use the
`hours` variable to construct the offset. This will adjust the model for the differing amount of
time individuals spent in the park.

**Answer.** The short documentation on `Fish`:
")

display(help(Fish))

data(Fish)

display_markdown("The `head` of `Fish`:")

display(head(Fish))

display_markdown("
<br/>
Notice several outliers in the full dataframe (not represented in `head`):
")

display(summary(Fish))

display_markdown(r"(
<br/>
One complication introduced by the question and data is how to infer fish per 'average visitor' when
every observation includes several visitors, both adults and children. This solution disregards
children, assuming any fishing they do is offset by the time they take away from fishing. Addressing
`persons` can be done the same way we address `hours`, by treating it as an exposure. That is:
$$
\begin{align}
log \left(\frac{fish}{person \cdot hour}\right)  & = log(fish) - log(person \cdot hour) \\
                                    & = log(fish) - log(person) - log(hour)
\end{align}
$$
)")

fish_dat <- list(
  FishCaught = Fish$fish_caught,
  LogPersonsInGroup = log(Fish$persons),
  LogHours = log(Fish$hours)
)


m_fish <- ulam(
  alist(
    FishCaught ~ dzipois(p, fish_per_person_hour),
    log(fish_per_person_hour) <- LogPersonsInGroup + LogHours + al,
    logit(p) <- ap,
    ap ~ dnorm(-0.5, 1),
    al ~ dnorm(1, 2)
  ), data = fish_dat, chains = 4
)
flush.console()

display_markdown("
<br/>
The precision of the parameters, which is rather hard to interpret. We can take from `ap` that most
visitors who were a part of this survey chose to fish. That is, the probability of not fishing looks
solidly below 0.5, since `ap` is solidly negative and any score less than zero implies a probability
of less than 0.5:
")

iplot(function() {
  plot(precis(m_fish))
}, ar=4.5)

display_markdown("
<br/>
Finally, a posterior check. Despite the large uncertainty in `ap` the model is still surprised by
outliers. An obvious alternative based on this chapter is a Gamma-Poisson, but this solution doesn't
pursue that.
")

iplot(function() {
  result <- postcheck(m_fish, window=250)
  display_markdown("The raw data:")
  display(result)
})
