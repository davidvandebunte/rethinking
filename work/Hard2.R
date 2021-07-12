library(IRdisplay)

display_markdown("
**2H1.** Suppose there are two species of panda bear. Both are equally common in the wild and live
in the same places. They look exactly alike and eat the same food, and there is yet no genetic assay
capable of telling them apart. They differ however in their family sizes. Species A gives birth to
twins 10% of the time, otherwise birthing a single infant. Species B births twins 20% of the time,
otherwise birthing singleton infants. Assume these numbers are known with certainty, from many years
of field research.

Now suppose you are managing a captive panda breeding program. You have a new female panda of
unknown species, and she has just given birth to twins. What is the probability that her next birth
will also be twins?

**Solution**

Assume a flat prior on the species of our panda. Since the two species are equally common in the
wild, we'll say the species of our panda is determined by a Bernoulli trial with `p = 0.5`.

The unobservable variable is not `p` in this case, but the outcome of the Bernoulli trial where we
selected this panda from the wild.

Assume the occurrence of twins only depends on the panda species, not on whether a particular panda
is more or less likely to have twins. To summarize:
```
Pr(A) = 0.5
Pr(B) = 0.5
Pr(twins|A) = 0.1
Pr(twins|B) = 0.2
Pr(twins) = Pr(twins|A) Pr(A) + Pr(twins|B) Pr(B)
```
")

prior = c(0.5, 0.5)
names(prior) <- c("A", "B")
likelihood = c(0.1, 0.2)
unstd.posterior = likelihood * prior
posterior = unstd.posterior / sum(unstd.posterior)
display(posterior)
twins = likelihood[1] * posterior[1] + likelihood[2] * posterior[2]
display(twins)

display_markdown("
**2H2.** Recall all the facts from the problem above. Now compute the probability that the panda we
have is from species A, assuming we have observed only the first birth and that it was twins.

**Solution**
In the language of machine learning this is running 'inference' (making a prediction) on the input
of 'twins' to the model. Notice prediction produces a probability, not a yes-no answer, but a
probability can be converted to a yes-no answer with a threshold (an event threshold).

```
Pr(A|twins) = Pr(twins|A) Pr(A) / Pr(twins)
```
")

display(0.1 * 0.5 / (0.1 * 0.5 + 0.2 * 0.5))

display_markdown("
**2H3.** Continuing on from the previous problem, suppose the same panda mother has a second birth
and that it is not twins, but a singleton infant. Compute the posterior probability that this panda
is species A.
")

likelihood = c(0.9, 0.8)
unstd.posterior = likelihood * posterior
posterior = unstd.posterior / sum(unstd.posterior)
display(posterior)

display_markdown("
**2H4.** A common boast of Bayesian statisticians is that Bayesian inference makes it easy to use
all of the data, even if the data are of different types.

So suppose now that a veterinarian comes along who has a new genetic test that she claims can
identify the species of our mother panda. But the test, like all tests, is imperfect. This is the
information you have about the test:
- The probability it correctly identifies a species A panda is 0.8.
- The probability it correctly identifies a species B panda is 0.65.

The vet administers the test to your panda and tells you that the test is positive for species A. First
ignore your previous information from the births and compute the posterior probability that your
panda is species A. Then redo your calculation, now using the birth data as well.

**Solution**

Presumably this is a yes-no test that produces either the answer A or B, which we can model as a
Bernoulli trial with two outcomes (testA and testB). The outcome of this trial is conditional on the
actual species of the panda (contingent on).

| x     | testA | testB | Total |
| ---   | ---   | ---   | ---   |
| A     | 0.8   | 0.2   | 1.0   |
| B     | 0.35  | 0.65  | 1.0   |
| Total | 1.15  | 0.85  | 2.0   |

```
Pr(testA|A) = Pr(testA, A) / P(A) = 0.8 * 0.5 / 0.5 = 0.8
Pr(testA|B) = 0.35
Pr(testB|A) = 0.2
Pr(testB|B) = 0.65
Pr(A|testA) = 
```
")

prior = c(0.5, 0.5)
names(prior) <- c("A", "B")
likelihood = c(0.8, 0.35)
unstd.posterior = likelihood * prior
posterior = unstd.posterior / sum(unstd.posterior)
display(posterior)

prior = c(0.5, 0.5)
names(prior) <- c("A", "B")
likelihood = c(0.8, 0.35)
unstd.posterior = likelihood * prior
posterior = unstd.posterior / sum(unstd.posterior)
likelihood = c(0.1, 0.2)
unstd.posterior = likelihood * posterior
posterior = unstd.posterior / sum(unstd.posterior)
likelihood = c(0.9, 0.8)
unstd.posterior = likelihood * posterior
posterior = unstd.posterior / sum(unstd.posterior)
display(posterior)
