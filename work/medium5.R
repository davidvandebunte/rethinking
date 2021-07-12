library(rethinking)

source("iplot.R")

display_markdown("# Medium")
display_markdown(r"(
**5M1.** Invent your own example of a spurious correlation. An outcome variable should be correlated
with both predictor variables. But when both predictors are entered in the same model, the
correlation between the outcome and one of the predictors should mostly vanish (or at least be
greatly reduced).

**Answer.** Predict drowning deaths (outcome variable S) from ice cream sales (predictor variable I)
and temperature (predictor variable T).
)")

display_markdown(r"(
**5M2.** Invent your own example of a masked relationship. An outcome variable should be correlated with
both predictor variables, but in opposite directions. And the two predictor variables should be
correlated with one another.

**Answer.** Predict savings (outcome variable S) from age (predictor variable A) and health
(predictor variable H). It's likely that age leads to both greater savings and decreased health.
)")

display_markdown(r"(
**5M3.** It is sometimes observed that the best predictor of fire risk is the presence of
firefighters— States and localities with many firefighters also have more fires. Presumably
firefighters do not cause fires. Nevertheless, this is not a spurious correlation. Instead fires
cause firefighters. Consider the same reversal of causal inference in the context of the divorce and
marriage data. How might a high divorce rate cause a higher marriage rate? Can you think of a way to
evaluate this relationship, using multiple regression?

**Answer.** A high divorce rate would lead to more people available for marriage; many people get
married multiple times.

We could regress M on D and A, then condition on A. This could include counterfactual plots that
involve manipulating D and checking the effect on M.
)")

display_markdown(r"(
5M4. In the divorce data, States with high numbers of Mormons (members of The Church of Jesus Christ
of Latter-day Saints, LDS) have much lower divorce rates than the regression models expected. Find a
list of LDS population by State and use those numbers as a predictor variable, predicting divorce
rate using marriage rate, median age at marriage, and percent LDS population (possibly
standardized). You may want to consider transformations of the raw percent LDS variable.
)")

data(WaffleDivorce)
d <- WaffleDivorce

d$A <- scale( d$MedianAgeMarriage )
d$D <- scale( d$Divorce )
d$M <- scale( d$Marriage )

# TODO: Skipping to avoid data collection and management.

display_markdown("
**5M5.** One way to reason through multiple causation hypotheses is to imagine detailed mechanisms
through which predictor variables may influence outcomes. For example, it is sometimes argued that
the price of gasoline (predictor variable) is positively associated with lower obesity rates (outcome
variable). However, there are at least two important mechanisms by which the price of gas could
reduce obesity. First, it could lead to less driving and therefore more exercise. Second, it could lead to
less driving, which leads to less eating out, which leads to less consumption of huge restaurant meals.
Can you outline one or more multiple regressions that address these two mechanisms? Assume you
can have any predictor data you need.

**Answer.** Call the price of gas $P$. We could define gas consumption $G$ as a predictor of
unobservable driving distance or better assume we have some magical $D$ indicating how much driving
is happening, perhaps from mileage from car dealerships. Either way, $D$ is what we really care
about. At some point people won't drive anymore as gas prices drop.

Call $E$ some proxy for exercise, such as the amount of walking observed in some area.

Call restaurant attendance $R$. We could get a proxy of this through restaurant income, but we were
told we could get any predictor we wanted and we aren't afraid to ask. We could filter to
restaurants with large high-calorie meals.

Call the obesity rate $O$.

Both theories are causal. The first theory has the graph:
```
D -> E -> O
```
```
```

The second theory has the graph:
```
D -> R -> O
```
```
```

So to combine them:
```
D -> R -> O <- E <- D
```
```
```

We could perform a regression of $O$ on $E$ and $R$ conditioning on $D$ to see which has a larger
effect on the obesity rate, perhaps running a counterfactual and controlling $D$ to see how the
effect varies in our model.
")
