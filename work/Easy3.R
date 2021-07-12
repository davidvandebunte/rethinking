library(IRdisplay)

# TODO: He mentions checking answers exactly here, but you're not aware of any posted solutions.
# Options:
# - https://www.erikkusch.com/category/statistical-rethinking/
# - https://github.com/rmcelreath/stat_rethinking_2020/tree/main/homework
# - https://sr2-solutions.wjakethompson.com/
# - https://jmgirard.com/tag/bayesian/
display_markdown("
These problems use the samples from the posterior distribution for the globe tossing example. This
code will give you a specific set of samples, so that you can check your answers exactly.

Use the values in `samples` to answer the questions that follow.")

p_grid <- seq( from=0 , to=1 , length.out=1000 )
# TODO: Again, why does this not integrate to one? Perhaps he's just not being technically correct
# since he normalizes later anyways. If you see this as counting, perhaps it doesn't matter.
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

display_markdown("**3E1.** How much posterior probability lies below p = 0.2?")
display(sum( samples < 0.2 ) / 1e4)

display_markdown("**3E2.** How much posterior probability lies above p = 0.8?")
display(sum( samples > 0.8 ) / 1e4)

display_markdown("**3E3.** How much posterior probability lies between p = 0.2 and p = 0.8?")
display(sum( samples > 0.2 & samples < 0.8 ) / 1e4)

display_markdown("**3E4.** 20% of the posterior probability lies below which value of p?")
display(quantile( samples, 0.2 ))

display_markdown("**3E5.** 20% of the posterior probability lies above which value of p?")
display(quantile( samples, 0.8 ))

display_markdown("
**3E6.** Which values of p contain the narrowest interval equal to 66% of the posterior probability?
")
library(rethinking)
display(HPDI( samples, prob=0.66 ))

display_markdown("
**3E7.** Which values of p contain 66% of the posterior probability, assuming equal posterior
probability both below and above the interval?
")
display(PI( samples, prob=0.66 ))
