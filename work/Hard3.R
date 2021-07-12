library(IRdisplay)

display_markdown("
**3H1.** Using grid approximation, compute the posterior distribution for the probability of a birth
being a boy. Assume a uniform prior probability. Which parameter value maximizes the posterior
probability?")

library(rethinking)
data(homeworkch3)

display(birth1)

iplot <- function(plot_func) {
    # https://stackoverflow.com/a/35805352/622049
    png("/tmp/plot.png", width=740, height=620, res=120)
    plot_func()
    dev.off()
    display_png(file="/tmp/plot.png")
}

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( sum(birth1) + sum(birth2) , size=2*length(birth1) , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
iplot(function() {
    plot(p_grid, posterior, type="l", xlab="probability of boy", ylab="posterior probability")
})
display(p_grid[posterior == max(posterior)])

display_markdown("
**3H2.** Using the sample function, draw 10,000 random parameter values from the posterior
distribution you calculated above. Use these samples to estimate the 50%, 89%, and 97% highest
posterior density intervals.")
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
display(HPDI( samples, prob=0.5 ))
display(HPDI( samples, prob=0.89 ))
display(HPDI( samples, prob=0.97 ))
iplot(function() {
    dens(samples)
})

display_markdown("
**3H3.** Use rbinom to simulate 10,000 replicates of 200 births. You should end up with 10,000
numbers, each one a count of boys out of 200 births. Compare the distribution of predicted numbers
of boys to the actual count in the data (111 boys out of 200 births). There are many good ways to
visualize the simulations, but the dens command (part of the `rethinking` package) is probably the
easiest way in this case. Does it look like the model fits the data well? That is, does the
distribution of predictions include the actual observation as a central, likely outcome?

**Solution**
See below; it looks like the model fits the one piece of data we have well.")
w <- rbinom( 1e4, size=200, prob=samples )
iplot(function() {
    dens(w)
})


display_markdown("
**3H4.** Now compare 10,000 counts of boys from 100 simulated first borns only to the number of boys
in the first births, `birth1`. How does the model look in this light?

**Solution**
The model is performing more poorly on only the first births.")
display(sum(birth1))
w <- rbinom( 1e4, size=100, prob=samples )
iplot(function() {
    dens(w)
})

display_markdown("
**3H5.** The model assumes that sex of first and second births are independent. To check this
assumption, focus now on second births that followed female first borns. Compare 10,000 simulated
counts of boys to only those second births that followed girls. To do this correctly, you need to
count the number of first borns who were girls and simulate that many births, 10,000 times. Compare
the counts of boys in your simulations to the actual observed count of boys following girls. How
does the model look in this light? Any guesses what is going on in these data?")
# TODO: What is he asking?
