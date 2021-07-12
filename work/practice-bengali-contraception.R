library(rethinking)
library(IRdisplay)
source("iplot.R")

display_markdown(r"(
**13H1.** In 1980, a typical Bengali woman could have 5 or more children in her lifetime. By the
year 2000, a typical Bengali woman had only 2 or 3. You’re going to look at a historical set of
data, when contraception was widely available but many families chose not to use it. These data
reside in `data(bangladesh)` and come from the 1988 Bangladesh Fertility Survey. Each row is one of
1934 women. There are six variables, but you can focus on two of them for this practice problem:

1. `district`: ID number of administrative district each woman resided in
2. `use.contraception`: An indicator (0/1) of whether the woman was using contraception

The first thing to do is ensure that the cluster variable, `district`, is a contiguous set of
integers. Recall that these values will be index values inside the model. If there are gaps, you’ll
have parameters for which there is no data to inform them. Worse, the model probably won’t run. Look
at the unique values of the `district` variable:

R code 13.40

```
> sort(unique(d$district))

[1] 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
[26] 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
[51] 51 52 53 55 56 57 58 59 60 61
```
<br/>

District 54 is absent. So `district` isn’t yet a good index variable, because it’s not contiguous.
This is easy to fix. Just make a new variable that is contiguous. This is enough to do it:

R code 13.41

```
> d$district_id <- as.integer(as.factor(d$district))
> sort(unique(d$district_id))

[1] 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
[26] 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
[51] 51 52 53 54 55 56 57 58 59 60
```
<br/>

Now there are 60 values, contiguous integers 1 to 60. Now, focus on predicting `use.contraception`,
clustered by `district_id`. Fit both (1) a traditional fixed-effects model that uses an index
variable for district and (2) a multilevel model with varying intercepts for district. Plot the
predicted proportions of women in each district using contraception, for both the fixed-effects
model and the varying-effects model. That is, make a plot in which district ID is on the horizontal
axis and expected proportion using contraception is on the vertical. Make one plot for each model,
or layer them on the same plot, as you prefer. How do the models disagree? Can you explain the
pattern of disagreement? In particular, can you explain the most extreme cases of disagreement, both
why they happen where they do and why the models reach different inferences?

**Answer.** The `help` for the `bangladesh` data.frame, to confirm we aren't missing anything:
)")

data(bangladesh)
display(help(bangladesh))

bc_df <- bangladesh
bc_df$district_id <- as.integer(as.factor(bc_df$district))
sort(unique(bc_df$district_id))

display_markdown(r"(
The `head` of the `bangladesh` data.frame, with the new variable suggested by the author:
)")
display(head(bc_df))

display_markdown(r"(
<br/>
A `summary` of the `bangladesh` data.frame:
)")
display(summary(bc_df))

display_markdown(r"(
<br/>
Sampling from the fixed effects model:
)")

bc_dat <- list(
  UseContraception = bc_df$use.contraception,
  DistrictId = bc_df$district_id
)

m_bc_fe <- ulam(
  alist(
    UseContraception ~ dbinom(1, p),
    logit(p) <- a[DistrictId],
    a[DistrictId] ~ dnorm(0, 1.5)
  ),
  data = bc_dat, chains = 4, cores = 4, log_lik = TRUE
)
display(precis(m_bc_fe, depth=2), mimetypes="text/plain")
iplot(function() {
  plot(precis(m_bc_fe, depth=2), main='m_bc_fe')
}, ar=0.8)

display_markdown(r"(
<br/>
Sampling from the varying effects model:
)")

m_bc_ve <- ulam(
  alist(
    UseContraception ~ dbinom(1, p),
    logit(p) <- a[DistrictId],
    a[DistrictId] ~ dnorm(a_bar, sigma),
    a_bar ~ dnorm(0, 1.5),
    sigma ~ dexp(1)
  ),
  data = bc_dat, chains = 4, cores = 4, log_lik = TRUE
)
display(precis(m_bc_ve, depth=2), mimetypes="text/plain")
iplot(function() {
  plot(precis(m_bc_ve, depth=2), main='m_bc_ve')
}, ar=0.8)

post_fe <- extract.samples(m_bc_fe)
post_ve <- extract.samples(m_bc_ve)
p_fe_c <- logistic(apply(post_fe$a, 2, mean))
p_ve_c <- logistic(apply(post_ve$a, 2, mean))

display_markdown(r"(
The plot suggested by the author in the question:
)")

iplot(function() {
  plot(p_fe_c,
    ylim = c(0, 1), pch = 16, xaxt = "n",
    xlab = "district ID", ylab = "proportion using contraception", col = rangi2,
    main="Predicted proportion using contraception"
  )
  axis(1, at = seq(1, 60, by=2), las=2)
  
  # overlay posterior means
  points(p_ve_c)
  
  # mark posterior mean probability across districts
  abline(h = mean(inv_logit(post_ve$a_bar)), lty = 2)
})

display_markdown(r"(
The number of observations (women) in every district:
)")

iplot(function() {
  barplot(setNames(table(bc_df$district_id), sort(unique(bc_df$district_id))))
})

display_markdown(r"(
There are two factors affecting shrinkage, as discussed under Figure 13.1 in the text. The first is
the number of observations in the district; notice that district 3 (with only a few observations)
shrinks much more towards the weighted cluster mean (the dashed line) than any other district. The
second is the distance from the dashed line. The estimate for district 35, which still has a
reasonable number of observations, shrinks significantly towards the dashed line because it starts
far from the mean. District 14 starts from a similar distance to the dashed line but shrinks less
because it has so many observations.
)")
