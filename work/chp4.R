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

pos <- replicate( 1000 , sum( runif(16,-1,1) ) )
iplot(function() hist(pos))
iplot(function() plot(density(pos)))

library(rethinking)
data(Howell1)
d <- Howell1

display(precis( d ))

d2 <- d[ d$age >= 18 , ]
iplot(function() curve( dnorm( x , 178 , 20 ) , from=100 , to=250 ))
iplot(function() curve( dunif( x , 0 , 50 ) , from=-10 , to=60 ) )

sample_mu <- rnorm( 1e4 , 178 , 20 )
sample_sigma <- runif( 1e4 , 0 , 50 )
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma )
iplot(function() dens( prior_h ))

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

display(precis( m4.3 ))
display(round( vcov( m4.3 ) , 3 ))
