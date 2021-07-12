source('iplot.R')
library(rethinking)
library(dagitty)

display_markdown(r"(
**11H4.** The data in `data(NWOgrants)` are outcomes for scientific funding applications for the
Netherlands Organization for Scientific Research (NWO) from 2010-2012 (see van der Lee and Ellemers
(2015) for data and context). These data have a very similar structure to the `UCBAdmit` data
discussed in the chapter. I want you to consider a similar question: What are the total and indirect
causal effects of gender on grant awards? Consider a mediation path (a pipe) through `discipline`.
Draw the corresponding DAG and then use one or more binomial GLMs to answer the question. What is
your causal interpretation? If NWO's goal is to equalize rates of funding between men and women,
what type of intervention would be most effective?

**Answer.** The `NWOGrants` data.frame:
)")

data(NWOGrants)
display(NWOGrants)
nwo <- NWOGrants

display_markdown("
<br/>
Reproduce the DAG from the text, where `D` now means `discipline` rather than department:
")

nwo_dag <- dagitty('
dag {
    bb="0,0,1,1"
    A [pos="0.500,0.400"]
    D [pos="0.300,0.300"]
    G [pos="0.100,0.400"]
    D -> A
    G -> A
    G -> D
}')
iplot(function() plot(nwo_dag), scale=10)

display_markdown("
For the total causal effect of gender on grant awards, do not condition on discipline:
")

nwo_dat <- list(
  awards = nwo$awards,
  applications = nwo$applications,
  gid = ifelse(as.character(nwo$gender) == "m", 1, 2)
)
m_nwo_tot <- quap(
  alist(
    awards ~ dbinom(applications, p),
    logit(p) <- a[gid],
    a[gid] ~ dnorm(0, 1.5)
  ),
  data = nwo_dat
)
iplot(function() {
  display_markdown("Raw data:")
  x <- precis(m_nwo_tot, depth = 2)
  display(x)
  plot(x, main="m_nwo_tot")
}, ar=4.5)

display_markdown("
[ct]: https://en.wikipedia.org/wiki/Contrast_(statistics)

These two coefficients are on the log-odds (parameter) scale, so the odds of a male getting an award
from an application is about $exp(-1.53) = 0.217$ and the odds of a female about $exp(-1.74) =
0.176$. If it helps you can convert these absolute odds to probabilities with $p = o/(1+o)$.

Lets also look at the relative performance of males to females (see [Contrast (statistics)][ct]). As
in the text we'll do so on the logit and probability scale:
")
post <- extract.samples(m_nwo_tot)
diff_a <- post$a[,1] - post$a[,2]
diff_p <- inv_logit(post$a[,1]) - inv_logit(post$a[,2])
iplot(function() {
  display_markdown("Raw data:")
  x <- precis(list(diff_a=diff_a, diff_p=diff_p))
  display(x)
  plot(x, main="m_nwo_tot: contrast")
}, ar=4.5)

display_markdown("
For the direct causal effect of gender on grant awards, condition on discipline:
")

nwo_dat$disc_id <- rep(1:9, each = 2)
m_nwo_dir <- quap(
  alist(
    awards ~ dbinom(applications, p),
    logit(p) <- a[gid] + delta[disc_id],
    a[gid] ~ dnorm(0, 1.5),
    delta[disc_id] ~ dnorm(0, 1.5)
  ),
  data = nwo_dat
)
iplot(function() {
  display_markdown("Raw data:")
  x <- precis(m_nwo_dir, depth = 2)
  display(x)
  plot(x, main="m_nwo_dir")
}, ar=3)

post <- extract.samples(m_nwo_dir)
diff_a <- post$a[,1] - post$a[,2]
diff_p <- inv_logit(post$a[,1]) - inv_logit(post$a[,2])
iplot(function() {
  display_markdown("Raw data:")
  x <- precis(list(diff_a=diff_a, diff_p=diff_p))
  display(x)
  plot(x, main="m_nwo_dir: contrast")
}, ar=4.5)

display_markdown("
Although the advantage for males has decreased, there is still some evidence for it.

One possible intervention is to increase funding to topics considered more valuable by females,
assuming they prefer to do research in the areas they are applying. Another is to encourage females
to get more involved (probably at a younger age) in the topics with more funding.
")

display_markdown("
**11H5.** Suppose that the NWO Grants sample has an unobserved confound that influences both choice of
discipline and the probability of an award. One example of such a confound could be the career stage
of each applicant. Suppose that in some disciplines, junior scholars apply for most of the grants.
In other disciplines, scholars from all career stages compete. As a result, career stage influences
discipline as well as the probability of being awarded a grant. Add these influences to your DAG
from the previous problem. What happens now when you condition on discipline? Does it provide an
un-confounded estimate of the direct path from gender to an award? Why or why not? Justify your
answer with the backdoor criterion. If you have trouble thinking this though, try simulating fake
data, assuming your DAG is true. Then analyze it using the model from the previous problem. What do
you conclude? Is it possible for gender to have a real direct causal influence but for a regression
conditioning on both gender and discipline to suggest zero influence?

**Answer.** Reproducing the DAG from the text (where U is now career stage):
")

nwo_career_stage_dag <- dagitty('
dag {
    bb="0,0,1,1"
    A [pos="0.500,0.400"]
    D [pos="0.300,0.300"]
    G [pos="0.100,0.400"]
    U [latent,pos="0.500,0.300"]
    D -> A
    G -> A
    G -> D
    U -> D
    U -> A
}')
iplot(function() plot(nwo_career_stage_dag), scale=10)

display_markdown("
If we condition on `D` then we open the backdoor path 'G -> D <- U -> A', which could confound any
causal inferences we make about the effect of gender on award. That is, we will induce a statistical
association between `G` and `A`. This association could cancel out any direct causal influence of
`G` on `A`.
")

# set.seed(145)
# n <- 100
# U <- rnorm(n)
# gid <- rbinom(n, 1, 0.5) + 1
# gender_to_disc_log_odds <- c(-0.2, 0.2)
# p_disc <- inv_logit(gender_to_disc_log_odds[gid] + U)
# disc_id <- rbinom(n, 1, p_disc) + 1
# gender_to_award_log_odds <- c(-1.5, -2)
# disc_to_award_log_odds <- c(0, 0)
# p_award <- inv_logit(gender_to_award_log_odds[gid] + U + disc_to_award_log_odds[disc_id])
# awards <- rbinom(n, 1, p_award)
# 
# display_markdown("
# Imagine there are only two disciplines. Researchers with more scholarly experience tend to work in
# the second discipline, and females tend to work in the second discipline. If we know you're in the
# second discipline, and you got an award, then it's likely you got the award because you're
# experienced.
# 
# 
# We're assuming that discipline has no influence on award in this case; this assumption is not
# necessary for conditioning on discipline to connect (d-connect) gender and scholarly experience.
# 
# G D U
# m 1 2
# f 1
# m 2 2
# f 2
# ")
