library(rethinking)
library(IRdisplay)

set.seed(100)
iplot <- function(plot_func) {
    # https://stackoverflow.com/a/35805352/622049
    png("/tmp/plot.png", width=740, height=620, res=120)
    plot_func()
    dev.off()
    display_png(file="/tmp/plot.png")
}

display_markdown("# Hard")

display_markdown("
**4H1.** The weights listed below were recorded in the !Kung census, but heights were not recorded
for these individuals. Provide predicted heights and 89% intervals for each of these individuals.
That is, fill in the table below, using model-based predictions.

| Individual | weight | expected height | 89% interval |
| ---        | ---    | ---             | ---          |
| 1          | 46.95  |                 |              |
| 2          | 43.72  |                 |              |
| 3          | 64.78  |                 |              |
| 4          | 32.59  |                 |              |
| 5          | 54.63  |                 |              | ")

library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

# define the average weight, x-bar
xbar <- mean(d2$weight)

# fit model
m4.3 <- quap(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + b*( weight - xbar ) ,
        a ~ dnorm( 178 , 20 ) ,
        b ~ dlnorm( 0 , 1 ) ,
        sigma ~ dunif( 0 , 50 )
        ) ,
    data=d2 )

weight.seq <- c(46.95, 43.72, 64.78, 32.59, 54.63)
mu <- link( m4.3 , data=data.frame(weight=weight.seq) )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )
display(mu.mean)
display(mu.PI)

display_markdown("
**4H2.** Select out all the rows in the `Howell1` data with ages below 18 years of age. If you do it
right, you should end up with a new data frame with 192 rows in it.

**(a)** Fit a linear regression to these data, using `quap`. Present and interpret the estimates. For
every 10 units of increase in weight, how much taller does the model predict a child gets?")
d4 <- d[ d$age < 18 , ]
display(nrow(d4))

# define the average weight, x-bar
xbar <- mean(d4$weight)

# fit model
m4h.2 <- quap(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + b*( weight - xbar ) ,
        a ~ dnorm( 110 , 20 ) ,
        b ~ dlnorm( 0 , 1 ) ,
        sigma ~ dunif( 0 , 30 )
        ) ,
    data=d4 )

display(precis( m4h.2 ))

display_markdown("
For every 1 kg (1 unit of increase in weight) the model predicts a child gets ~2.7 cm taller. So for
every 10 kg (10 units of increase in weight), it predicts about 27 cm of increase in height.")

display_markdown("
**(b)** Plot the raw data, with height on the vertical axis and weight on the horizontal axis.
Super-impose the MAP regression line and 89% interval for the mean. Also superimpose the 89%
interval for predicted heights.")

weight.seq <- seq( from=min(d4$weight) , to=max(d4$weight) , by=1 )
mu <- link( m4h.2, data=data.frame(weight=weight.seq) )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )
sim.height <- sim( m4h.2 , data=list(weight=weight.seq) )
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )

iplot(function() {
    plot( height ~ weight , data=d4 , col=rangi2 )
    lines( weight.seq , mu.mean )
    shade( mu.PI , weight.seq )
    shade( height.PI , weight.seq )
})

display_markdown("
**(c)** What aspects of the model fit concern you? Describe the kinds of assumptions you would
change, if any, to improve the model. You don’t have to write any new code. Just explain what the
model appears to be doing a bad job of, and what you hypothesize would be a better model.

**Answer.** The model doesn't perform well at the extremes of height and weight (the youngest and
oldest children). It's likely a non-linear model would perform better in these areas.
")

display_markdown("
**4H3.** Suppose a colleague of yours, who works on allometry, glances at the practice problems just
above. Your colleague exclaims, “That’s silly. Everyone knows that it’s only the *logarithm* of body
weight that scales with height!” Let’s take your colleague’s advice and see what happens.

**(a)** Model the relationship between height (cm) and the natural logarithm of weight (log-kg). Use
the entire `Howell1` data frame, all 544 rows, adults and non-adults. Fit this model, using
quadratic approximation:

$$ h_i ∼ Normal(µ_i, σ) $$
$$ µ_i = α + β log(w_i) $$
$$ α ∼ Normal(178, 20) $$
$$ β ∼ LogNormal(0, 1) $$
$$ σ ∼ Uniform(0, 50) $$

where $h_i$ is the height of individual *i* and $w_i$ is the weight (in kg) of individual *i.* The
function for computing a natural log in R is just `log`. Can you interpret the resulting estimates?

**Answer.** The parameter β now represents the increase in cm (~47 cm) per unit increase in the order
of magnitude of the weight.
")

m4h.3 <- quap(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + b*log(weight) ,
        a ~ dnorm( 178 , 20 ) ,
        b ~ dlnorm( 0 , 1 ) ,
        sigma ~ dunif( 0 , 50 )
        ) ,
    data=d )

display(precis( m4h.3 ))

display_markdown("
**(b)** Begin with this plot:

```R
plot( height ~ weight, data=Howell1,
     col=col.alpha(rangi2, 0.4) )
```
```
```

Then use samples from the quadratic approximate posterior of the model in (a) to superimpose on
the plot: (1) the predicted mean height as a function of weight, (2) the 97% interval for the mean, and
(3) the 97% interval for predicted heights.

**Answer.** See the book 'Scale' by Geoffrey West")

weight.seq <- seq( from=min(d$weight) , to=max(d$weight) , by=1 )
mu <- link( m4h.3, data=data.frame(weight=weight.seq) )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.97 )
sim.height <- sim( m4h.3 , data=list(weight=weight.seq) )
height.PI <- apply( sim.height , 2 , PI , prob=0.97 )

iplot(function() {
    plot( height ~ weight , data=d4 , col=rangi2 )
    lines( weight.seq , mu.mean )
    shade( mu.PI , weight.seq )
    shade( height.PI , weight.seq )
})

display_markdown("
**4H4.** Plot the prior predictive distribution for the polynomial regression model in the chapter.
You can modify the code that plots the linear regression prior predictive distribution. Can you
modify the prior distributions of α, β₁, and β₂ so that the prior predictions stay within the
biologically reasonable outcome space? That is to say: Do not try to fit the data by hand. But do
try to keep the curves consistent with what you know about height and weight, before seeing these
exact data.")

sample_a <- rnorm(nrow(d), 178, 20)
sample_b1 <- rlnorm(nrow(d), 0, 1)
sample_b2 <- rnorm(nrow(d), 0, 1)
sample_sigma <- runif(nrow(d), 0, 50)

d$weight_s <- (d$weight - mean(d$weight))/sd(d$weight)
d$weight_s2 <- d$weight_s^2

sample_mu <- sample_a + sample_b1*d$weight_s + sample_b2*d$weight_s2
iplot(function() dens( sample_mu ))

prior_h <- rnorm(1e4, sample_mu, sample_sigma)
iplot(function() dens( prior_h ))
# TODO: The prior already looks biologically reasonable. What are you missing?

# TODO: See the comment on pg. 114 about setting the prior for β₂. Why must it be positive?
