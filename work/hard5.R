library(rethinking)

library(dagitty)

source("iplot.R")

display_markdown("# Hard")
display_markdown("
**5H1.** In the divorce example, suppose the DAG is: M → A → D. What are the implied conditional
independencies of the graph? Are the data consistent with it?

**Answer.** The data is consistent with this implied conditional dependency because it is the same
conditional dependency we saw in the text.")

dag5.h1 = dagitty('dag{ M -> A -> D}')
display(impliedConditionalIndependencies(dag5.h1))
display_markdown("")
display_markdown("The equivalent DAGs:")
iplot(function() drawdag(equivalentDAGs(dag5.h1)), scale=10)

display_markdown("
**5H2.** Assuming that the DAG for the divorce example is indeed M → A → D, fit a new model and use
it to estimate the counterfactual effect of halving a State’s marriage rate M. Use the
counterfactual example from the chapter (starting on page 140) as a template.")

data(WaffleDivorce)
d <- list()
d$A <- standardize( WaffleDivorce$MedianAgeMarriage )
d$D <- standardize( WaffleDivorce$Divorce )
d$M <- standardize( WaffleDivorce$Marriage )

m5.h2 <- quap(
    alist(
        ## A -> D
        D ~ dnorm( mu , sigma ) ,
        mu <- a + bA*A ,
        a ~ dnorm( 0 , 0.2 ) ,
        bA ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 ),
        ## M -> A
        A ~ dnorm( mu_A , sigma_A ),
        mu_A <- aA + bMA*M,
        aA ~ dnorm( 0 , 0.2 ),
        bMA ~ dnorm( 0 , 0.5 ),
        sigma_A ~ dexp( 1 )
    ) , data = d )
display(precis( m5.h2 ))

M_seq <- seq( from=-2 , to=2 , length.out=30 )
sim_dat <- data.frame( M=M_seq )
s <- sim( m5.h2 , data=sim_dat , vars=c("A","D") )

iplot(function() {
    plot( sim_dat$M , colMeans(s$D) , ylim=c(-2,2) , type="l" ,
         xlab="manipulated M" , ylab="counterfactual D" )
    shade( apply(s$D,2,PI) , sim_dat$M )
    mtext( "Total counterfactual effect of M on D" )
})

post <- extract.samples(m5.h2)
plot( bl ~ br , post , col=col.alpha(rangi2,0.1) , pch=16 )

display_markdown("
**5H3.** Return to the milk energy model, m5.7. Suppose that the true causal relationship among the
variables is:")

dag5.h3 = dagitty('dag{ K <- M -> N -> K}')
coordinates(dag5.h3) <- list( x=c(M=0,K=1,N=2) , y=c(M=0,K=1,N=0) )
iplot(function() drawdag(dag5.h3), scale=5)

display_markdown("
Now compute the counterfactual effect on K of doubling M. You will need to account for both the
direct and indirect paths of causation. Use the counterfactual example from the chapter (starting on
page 140) as a template.")

data(milk)
d <- milk
display(precis(d))

d$K <- scale( d$kcal.per.g )
d$N <- scale( d$neocortex.perc )
d$M <- scale( log(d$mass) )
dcc <- d[ complete.cases(d$K,d$N,d$M) , ]

m5.h3 <- quap(
    alist(
        ## M -> K <- N
        K ~ dnorm( mu , sigma ) ,
        mu <- a + bM*M + bN*N ,
        a ~ dnorm( 0 , 0.2 ) ,
        bM ~ dnorm( 0 , 0.5 ) ,
        bN ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 ),
        ## M -> N
        N ~ dnorm( mu_N , sigma_N ),
        mu_N <- aN + bMN*M,
        aN ~ dnorm( 0 , 0.2 ),
        bMN ~ dnorm( 0 , 0.5 ),
        sigma_N ~ dexp( 1 )
    ) , data = dcc )

display(precis( m5.h3 ))

M_seq <- seq( from=-2 , to=2 , length.out=30 )
sim_dat <- data.frame( M=M_seq )
s <- sim( m5.h3 , data=sim_dat , vars=c("N","K") )

iplot(function() {
    plot( sim_dat$M , colMeans(s$K) , ylim=c(-2,2) , type="l" ,
         xlab="manipulated M" , ylab="counterfactual K" )
    shade( apply(s$K,2,PI) , sim_dat$M )
    mtext( "Total counterfactual effect of M on K" )
})

display_markdown("
**5H4.** Here is an open practice problem to engage your imagination. In the divorce date, States in
the southern United States have many of the highest divorce rates. Add the South indicator variable
to the analysis. First, draw one or more DAGs that represent your ideas for how Southern American
culture might influence any of the other three variables (D, M or A). Then list the testable
implications of your DAGs, if there are any, and fit one or more models to evaluate the
implications. What do you think the influence of “Southerness” is?")

# TODO: See the DAG at the end of Chp. 6.
