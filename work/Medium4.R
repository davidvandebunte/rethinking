library(rethinking)
library(IRdisplay)

display_markdown("# Medium")
set.seed(100)
iplot <- function(plot_func) {
    # https://stackoverflow.com/a/35805352/622049
    png("/tmp/plot.png", width=740, height=620, res=120)
    plot_func()
    dev.off()
    display_png(file="/tmp/plot.png")
}

display_markdown("
**4M1.** For the model definition below, simulate observed y values from the prior (not the posterior).

$$ y_i ∼ Normal(µ, σ) $$
$$ µ ∼ Normal(0, 10) $$
$$ σ ∼ Exponential(1) $$")

sample_mu <- rnorm( 1e4 , 0 , 10 )
sample_sigma <- rexp( 1e4 , 1 )
prior_y <- rnorm( 1e4 , sample_mu , sample_sigma )
iplot(function() dens( prior_y ))

display_markdown("**4M2.** Translate the model just above into a quap formula.
```
flist <- alist(
    y ~ dnorm( mu , sigma ),
    mu ~ dnorm( 0 , 10 ),
    sigma ~ dexp( 1 )
)
```")

display_markdown("**4M3.** Translate the quap model formula below into a mathematical model definition.
```
flist <- alist(
    y ~ dnorm( mu , sigma ),
    mu <- a + b*x,
    a ~ dnorm( 0 , 10 ),
    b ~ dunif( 0 , 1 ),
    sigma ~ dexp( 1 )
)
```

$$ y_i ∼ Normal(µ_i, σ) $$
$$ µ_i ∼ a + b·x $$
$$ a ∼ Normal(0, 10) $$
$$ b ∼ Uniform(0, 1) $$
$$ σ ∼ Exponential(1) $$")

display_markdown("
**4M4.** A sample of students is measured for height each year for 3 years. After the third year,
you want to fit a linear regression predicting height using year as a predictor. Write down the
mathematical model definition for this regression, using any variable names and priors you choose.
Be prepared to defend your choice of priors.

$$ h_i ∼ Normal(µ_i, σ) $$
$$ µ_i ∼ a + b·(y_i - \\overline{y}) $$
$$ a ∼ Normal(178, 20) $$
$$ b ∼ LogNormal(0, 1) $$
$$ σ ∼ Uniform(0, 50) $$

Use LogNormal and Uniform for b and σ to keep them positive.

**4M5.** Now suppose I remind you that every student got taller each year. Does this information lead
you to change your choice of priors? How?

The priors already assume this.

**4M6.** Now suppose I tell you that the variance among heights for students of the same age is never
more than 64cm. How does this lead you to revise your priors?

If the variance is no more than 64cm, then the standard deviation is no more than 8cm. It's probably
reasonable to reduce the range of the allowed values of that prior:

$$ σ ∼ Uniform(2, 10) $$
")
