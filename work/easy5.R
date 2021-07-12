library(rethinking)
library(IRdisplay)

display_markdown("# Easy")
display_markdown(r"(
**5E1.** Which of the linear models below are multiple linear regressions?

1. $ \mu_i = \alpha + \beta x_i $
1. $ \mu_i = \beta_x x_i + \beta_z z_i $
1. $ \mu_i = \alpha + \beta(x_i − z_i) $
1. $ \mu_i = \alpha + \beta_x x_i + \beta_z z_i $

**Answer.** The 2nd and 4th.
)")

display_markdown("
**5E2.** Write down a multiple regression to evaluate the claim: *Animal diversity is linearly
related to latitude, but only after controlling for plant diversity*. You just need to write down the
model definition.

**Answer.** Call animal diversity $A$, plant diversity $P$, and latitude $L$. Assume
standardization so it's easier to assign priors.

$$
\\begin{align}
A_i & ∼ Normal(\\mu_i, \\sigma) \\\\
\\mu_i & = \\alpha + \\beta_P P_i + \\beta_L L_i \\\\
\\alpha & ∼ Normal(0, 0.2) \\\\
\\beta_P & ∼ Normal(0, 0.5) \\\\
\\beta_L & ∼ Normal(0, 0.5) \\\\
\\sigma & ∼ Exponential(1)
\\end{align}
$$")

display_markdown("
**5E3.** Write down a multiple regression to evaluate the claim: *Neither amount of funding nor size
of laboratory is by itself a good predictor of time to PhD degree; but together these variables are
both positively associated with time to degree*. Write down the model definition and indicate which
side of zero each slope parameter should be on.

**Answer.** Call amount of funding $F$, laboratory size $S$, and time to degree $T$. Assume
standardization so it's easier to assign priors.

TODO: Is the question implying a masked relationship? This isn't clear to you from the statement. If
so, one parameter would be negative and one positive.

$$ T_i ∼ Normal(\\mu_i, \\sigma) $$
$$ \\mu_i = \\alpha + \\beta_F F_i + \\beta_S S_i $$
$$ \\alpha ∼ Normal(0, 0.2) $$
$$ \\beta_F ∼ Normal(0, 0.5) $$
$$ \\beta_S ∼ Normal(0, 0.5) $$
$$ \\sigma ∼ Exponential(1) $$")

display_markdown("
**5E4.** Suppose you have a single categorical predictor with 4 levels (unique values), labeled A, B, C
and D. Let $A_i$ be an indicator variable that is 1 where case i is in category A. Also suppose
$B_i$ , $C_i$ , and $D_i$ for the other categories. Now which of the following linear models are
inferentially equivalent ways to include the categorical variable in a regression? Models are
inferentially equivalent when it’s possible to compute one posterior distribution from the posterior
distribution of another model.

1. $ \\mu_i = \\alpha + \\beta_A A_i + \\beta_B B_i + \\beta_D D_i $
1. $ \\mu_i = \\alpha + \\beta_A A_i + \\beta_B B_i + \\beta_C C_i + \\beta_D D_i $
1. $ \\mu_i = \\alpha + \\beta_B B_i + \\beta_C C_i + \\beta_D D_i $
1. $ \\mu_i = \\alpha_A A_i + \\alpha_B B_i + \\alpha_C C_i + \\alpha_D D_i $
1. $ \\mu_i = \\alpha_A (1 − B_i − C_i − D_i ) + \\alpha_B B_i + \\alpha_C C_i + \\alpha_D D_i $

**Solution.** The 4th and the 5th are equivalent. The others are not. For example, the 1st assumes
more uncertainty in A, B, and D than in C. It doesn't even have C in the posterior.
")
