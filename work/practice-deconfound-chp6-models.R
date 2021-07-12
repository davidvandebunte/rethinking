library(rethinking)
library(IRdisplay)
source("iplot.R")

display_markdown("## 6.6. Practice")

display_markdown("
**6E1.** List three mechanisms by which multiple regression can produce false inferences about
causal effects.

**Answer.** Multicollinearity, Post-treatment bias, and Collider bias (the Chapter's section
titles).")

display_markdown(r"(
**6E3.** List the four elemental confounds. Can you explain the conditional dependencies of each?

**Answer.** From the beginning of Section 6.4 (Confronting confounding):
- The Fork
- The Pipe
- The Collider
- The Descendant

[dswt]: http://dagitty.net/learn/dsep/index.html

For practice understanding which variables are 'connected' (dependent, covariate) without the help
of automatic tools, see the DAGitty tutorial [d-Separation Without Tears][dswt].

The Fork and Pipe actually have one and the same conditional dependency:
$$
X \perp \!\!\! \perp Y | Z
$$

The Collider has no conditional dependencies, but $X \perp Y$ unconditionally.

The Descendant is similar to the Collider. It still has $X \perp Y$ unconditionally, but has the
additional conditional dependencies:
$$
X \perp \!\!\! \perp D | Z \\
Y \perp \!\!\! \perp D | Z
$$

Notice `X -> Z -> D` and `Y -> Z -> D` are Pipes in themselves.
)")

display_markdown("
**6E4.** How is a biased sample like conditioning on a collider? Think of the example at the open of
the chapter.

**Answer.** In a biased sample you're presumably unaware of the additional variable; it's simply not
in your tables and generally not on your radar (e.g. in your causal diagram).

When you condition on a collider you are aware of the variable but instead choose to sample from the
part of your table that is biased toward a particular realization of the variable.
")

library(dagitty)
set.seed(106)

display_markdown("
**6M1.** Modify the DAG on page 186 to include the variable V, an unobserved cause of C and Y: C ← V →
Y. Reanalyze the DAG. How many paths connect X to Y? Which must be closed? Which variables should
you condition on now?")

# Draw a DAG here, then tweak the positions:
# - http://dagitty.net/dags.html
dag6.m1 = dagitty('
dag {
    bb="0,0,1,1"
    A [pos="0.5,0.175"]
    B [pos="0.5,0.225"]
    C [pos="0.75,0.2"]
    U [latent,pos="0.25,0.2"]
    V [latent,pos="1.0,0.25"]
    X [exposure,pos="0.25,0.3"]
    Y [outcome,pos="0.75,0.3"]
    A -> C
    A -> U
    C -> B
    C -> Y
    U -> B
    U -> X
    V -> C
    V -> Y
    X -> Y
}')
iplot(function() plot(dag6.m1), scale=10)
display(adjustmentSets(dag6.m1))
display_markdown("")

display_markdown("
**6M2.** Sometimes, in order to avoid multicollinearity, people inspect pairwise correlations among
predictors before including them in a model. This is a bad procedure, because what matters is the
conditional association, not the association before the variables are included in the model. To
highlight this, consider the DAG X → Z → Y. Simulate data from this DAG so that the correlation
between X and Z is very large. Then include both in a model prediction Y. Do you observe any
multicollinearity? Why or why not? What is different from the legs example in the chapter?

**Answer.** We still observe multicollinearity, most likely because it's now impossible to
distinguish the noise inherent in our measurement of Y from any noise added to Z relative to X. If
the noise we add as part of generating Y is on the same scale as the noise added to Z, then we don't
observe this issue.

Removing predictors based on pairwise correlation is dangerous because we don't know whether (in
this case) either X or Z has a causal influence on Y. We really have no reason to prefer removing
either X or Z from the analysis based on pairwise correlation, so we could easily remove the wrong
one (X in this case) and infer that X has a direct (rather than total/indirect) causal influence on
Y. If we were to then condition on Z, we'd be surprised with the results.")

# TODO: Are you confident in this answer?

n <- 100
X <- rnorm( n )
Z <- rnorm( n , mean=X , sd=0.02)
Y <- rnorm( n , mean=Z , sd=0.2)
d_sim <- data.frame(X=X,Z=Z,Y=Y)
iplot(function() pairs(d_sim))

display(cor(d_sim$X, d_sim$Z))
display_markdown("")

m6.m2 <- quap(
    alist(
        Y ~ dnorm( mu , sigma ) ,
        mu <- a + bZ*Z + bX*X ,
        a ~ dnorm( 0 , 100 ) ,
        bZ ~ dnorm( 1 , 10 ) ,
        bX ~ dnorm( 1 , 10 ) ,
        sigma ~ dexp( 1 )
    ) , data=d_sim )
display(precis(m6.m2))

# See:
# - https://github.com/rmcelreath/rethinking/issues/22#issuecomment-731401760
setMethod( "plot" , "coeftab" , function(x,y,...) coeftab_plot(x,y,...) )
iplot(function() plot(coeftab(m6.m2)))

post <- extract.samples(m6.m2)
iplot(function() plot( bZ ~ bX , post , col=col.alpha(rangi2,0.1) , pch=16 ))

display_markdown("
**6M3.** Learning to analyze DAGs requires practice. For each of the four DAGs below, state which
variables, if any, you must adjust for (condition on) to estimate the total causal influence of X on
Y.

**Answer.** Reading the DAGs from the left to right, top to bottom:
1. Only Z, although A would not hurt (it is a competing exposure).
2. Only A. Conditioning on Z would measure direct effect rather than total effect.
3. None. Conditioning on Z would lead to collider bias.
4. Only A, which is otherwise a 'Fork' confounder (an ancestor of both X and Y).

See also:
- http://dagitty.net/learn/graphs/roles.html
- http://dagitty.net/learn/graphs/table2-fallacy.html")

set.seed(100)

display_markdown("
**6H1.** Use the Waffle House data, `data(WaffleDivorce)`, to find the total causal influence of
number of Waffle Houses on divorce rate. Justify your model or models with a causal graph.")

dag6.h1 = dagitty('
dag {
    bb="0,0,1,1"
    A [pos="0.3,0.4"]
    D [outcome,pos="0.7,0.4"]
    M [pos="0.5,0.3"]
    S [pos="0.3,0.2"]
    W [exposure,pos="0.7,0.2"]
    A -> D
    A -> M
    M -> D
    S -> A
    S -> M
    S -> W
    W -> D
}')

iplot(function() plot(dag6.h1), scale=10)
display_markdown("Adjustment Sets:")
display(adjustmentSets(dag6.h1))
display_markdown("")

data(WaffleDivorce)
d <- WaffleDivorce
d$A <- standardize( d$MedianAgeMarriage )
d$D <- standardize( d$Divorce )
d$M <- standardize( d$Marriage )
d$W <- d$WaffleHouses
d$S <- ifelse(d$South==1, 2, 1)

m6.h1 <- quap(
    alist(
        D ~ dnorm( mu , sigma ) ,
        mu <- a[S] + bW*W,
        a[S] ~ dnorm( 0 , 8 ) ,
        bW ~ dnorm( 0 , 10 ) ,
        sigma ~ dunif( 0 , 50 )
    ) , data=d )

# See:
# - https://github.com/rmcelreath/rethinking/issues/22#issuecomment-731401760
setMethod( "plot" , "coeftab" , function(x,y,...) coeftab_plot(x,y,...) )
iplot(function() plot(coeftab(m6.h1)), scale=15)

display_markdown("
**6H2.** Build a series of models to test the implied conditional independencies of the causal graph you
used in the previous problem. If any of the tests fail, how do you think the graph needs to be
amended? Does the graph need more or fewer arrows? Feel free to nominate variables that aren’t in
the data.")

display(impliedConditionalIndependencies(dag6.h1))
m6.h2_a <- quap(
    alist(
        A ~ dnorm( mu , sigma ) ,
        mu <- a[S] + bW*W,
        a[S] ~ dnorm( 10 , 2 ) ,
        bW ~ dnorm( 0 , 10 ) ,
        sigma ~ dunif( 0 , 50 )
    ) , data=d )
iplot(function() plot(coeftab(m6.h2_a)), ar=2.5)
m6.h2_c <- quap(
    alist(
        M ~ dnorm( mu , sigma ) ,
        mu <- a[S] + bW*W,
        a[S] ~ dnorm( 10 , 2 ) ,
        bW ~ dnorm( 0 , 10 ) ,
        sigma ~ dunif( 0 , 50 )
    ) , data=d )
iplot(function() plot(coeftab(m6.h2_c)), ar=2.5)

# TODO: The middle conditional independency

display_markdown("
All three problems below are based on the same data. The data in `data(foxes)` are 116 foxes from 30
different urban groups in England. These foxes are like street gangs. Group size varies from 2 to 8
individuals. Each group maintains its own urban territory. Some territories are larger than others.
The area variable encodes this information. Some territories also have more avgfood than others. We
want to model the weight of each fox. For the problems below, assume the following DAG:")

source('load-fox-models.R')
iplot(function() plot(dag_foxes), scale=10)
display_markdown("Implied Conditional Dependencies:")
display(impliedConditionalIndependencies(dag_foxes))
display_markdown("")

display_markdown("
**6H3.** Use a model to infer the total causal influence of area on weight. Would increasing the area
available to each fox make it heavier (healthier)? You might want to standardize the variables.
Regardless, use prior predictive simulation to show that your model’s prior predictions stay within
the possible outcome range.")

display_markdown("
**Answer.** We have nothing to condition on if we only care about the total causal effect of area on
weight:"
)
display(adjustmentSets(dag_foxes, exposure="area", effect="total"))
display_markdown("")

display_markdown(r"(
**Answer.** Assume the average area corresponds to the average weight in the priors ($\beta_0$):)"
)
prior <- extract.prior(mfox_Weight_Area)
mu <- link(mfox_Weight_Area, post=prior , data=list( Area=c(-2,2) ) )
iplot(function() {
    plot( NULL , ylab="Weight", xlab="Area", xlim=c(-2,2) , ylim=c(-2,2) )
    for ( i in 1:50 ) lines( c(-2,2) , mu[i,] , col=col.alpha("black",0.4) )
})

display_markdown("
Presumably because a larger area implies both more food (increasing weight) and a larger group
(decreasing weight, more mouths to feed) the total causal effect of area on weight is nearly zero:
")
iplot(function() plot(coeftab(mfox_Weight_Area)), ar=3)

display_markdown("
**6H4.** Now infer the causal impact of adding food to a territory. Would this make foxes heavier?
Which covariates do you need to adjust for to estimate the total causal influence of food?")

display_markdown(
"**Answer.** We have nothing to condition on if we only care about the total causal effect of food
on weight:"
)
display(adjustmentSets(dag_foxes, exposure="avgfood", effect="total"))
display_markdown("")

display_markdown(
"**Answer.** Presumably because food also affects group size, and a larger group means more mouths
to feed, there appars to be no noticeable impact of more food on weight:"
)
iplot(function() plot(coeftab(mfox_Weight_Avgfood)), ar=3.5)

display_markdown("
It's more interesting to look at the direct affect of food on weight. To see this we'll need to
adjust for groupsize:
")
display(adjustmentSets(dag_foxes, exposure="avgfood", effect="direct"))
display_markdown("")

display_markdown("
As expected, food has a positive influence on weight (`bAvgfood` is positive):
")
iplot(function() plot(coeftab(mfox_Weight_AvgfoodGroupsize)), ar=3)

display_markdown("
**6H5.** Now infer the causal impact of group size. Which covariates do you need to adjust for?
Looking at the posterior distribution of the resulting model, what do you think explains these data?
That is, can you explain the estimates for all three problems? How do they go together?")

display_markdown("
**Answer.** We need to adjust for only avgfood assuming we are interested in the total causal
effect of groupsize (otherwise avgfood would be a confounder):
")
display(adjustmentSets(dag_foxes, exposure="groupsize", effect="total"))
display_markdown("")

display_markdown("
See the model `mfox_Weight_AvgfoodGroupsize` above for a model that already includes only groupsize and avgfood. As
expected, groupsize has a negative influence on weight (`bGroupsize` is negative) presumably because
a bigger group means more mouths to feed.
")
