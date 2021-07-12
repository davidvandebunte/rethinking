source("iplot.R")

display_markdown(r"(
**14E1.** Add to the following model varying slopes on the predictor *x*.

$$
\begin{align}
y_i & \sim Normal(\mu_i, \sigma) \\
\mu_i & = \alpha_{GROUP[i]} + \beta x_i \\
\alpha_{GROUP} & \sim Normal(\alpha, \sigma_{\alpha}) \\
\alpha & \sim Normal(0, 10) \\
\beta & \sim Normal(0, 1) \\
\sigma & \sim Exponential(1) \\
\sigma_{\alpha} & \sim Exponential(1)
\end{align}
$$

**Answer.** See section **14.1.3**. Notice the distinction of $i$ and $j$ indexes.

$$
\begin{align}
y_i & \sim Normal(\mu_i, \sigma) \\
\mu_i & = \alpha_{GROUP[j]} + \beta_{GROUP[j]} x_i \\
\begin{bmatrix}
\alpha_{GROUP} \\
\beta_{GROUP}
\end{bmatrix}
& \sim
MVNormal(
\begin{bmatrix}
\alpha \\
\beta
\end{bmatrix}
, \mathbf{S}) \\
\alpha & \sim Normal(0, 10) \\
\beta & \sim Normal(0, 1) \\
\mathbf{S} & =
\begin{pmatrix}
\sigma_{\alpha} & 0 \\
0 & \sigma_{\beta}
\end{pmatrix}
\mathbf{R}
\begin{pmatrix}
\sigma_{\alpha} & 0 \\
0 & \sigma_{\beta}
\end{pmatrix} \\
\sigma_{\alpha} & \sim Exponential(1) \\
\sigma_{\beta} & \sim Exponential(1)
\end{align}
$$
)")

display_markdown(r"(
**14E2.** Think up a context in which varying intercepts will be positively correlated with varying
slopes. Provide a mechanistic explanation for the correlation.

**Answer.** If you believe that education leads to greater wealth, then in the prediction of wealth
based on parent's wealth. If you come from family with a lot of money you'll start off with a lot of
money (the intercept), and because wealth builds on wealth the addition of education will help more
than for someone who doesn't have as many resources to work with to start (the slope). You could
replace education with ambition or something else along that theme, as well.

We could also adapt the cafe example to use an `M` indicator (for morning) rather than an `A`
indicator for afternoon. The intercepts would have to become the afternoon wait. Long afternoon
waits are correlated with even longer morning waits.
)")

display_markdown(r"(
**14E3.** When is it possible for a varying slopes model to have fewer effective parameters (as
estimated by WAIC or PSIS) than the corresponding model with fixed (unpooled) slopes? Explain.

**Answer.** When there is some relationship between the intercepts and slopes that helps the model
regularize itself. That is, if there is some correlation between intercepts and slopes, the model
should be able to detect this and effectively learn to predict intercepts from slopes, and vice
versa. This is similar to how in an intercepts-only multilevel model knowing some intercepts should
help the model predict other intercepts; the known intercepts are a regularizing prior for new
intercepts.
)")
