source('iplot.R')
library(rethinking)

display_markdown(r"(
**9H7.** Can you write your own Hamiltonian Monte Carlo algorithm for the globe tossing data, using
the R code in the chapter? You will have to write your own functions for the likelihood and
gradient, but you can use the `HMC2` function.

**Answer.** The probability mass function for the binomial distribution is defined as:
$$
f(p, k, n) = \binom{n}{k}p^k(1-p)^{n-k}
$$

Taking the logarithm:
$$
g(p, k, n) = log(f(p, k, n)) = log\left(\binom{n}{k}p^k(1-p)^{n-k}\right) \\
g(p, k, n) = log\left(\binom{n}{k}\right) + k·log(p) + (n-k)·log(1-p)
$$

And the derivative with respect to $p$:
$$
\begin{align}
\frac{\partial g(p, k, n)}{\partial p} & = \frac{\partial log\left(\binom{n}{k}\right)}{\partial p} + \frac{\partial k·log(p)}{\partial p} + \frac{\partial z(p,k,n)}{\partial p} \\
z(p, k, n) & = (n -k)·log(y) \\
y(p) & = 1 - p \\
\end{align}
$$

Notice that:
$$
\begin{align}
\frac{\partial z(p, k, n)}{\partial y} & = \frac{n -k}{y} \\
\frac{\partial y(p)}{\partial p} & = -1
\end{align}
$$

So that:
$$
\begin{align}
\frac{\partial g(p, k, n)}{\partial p} & = 0 + k/p + \frac{\partial z}{\partial y}\frac{\partial y}{\partial p} \\
\frac{\partial g(p, k, n)}{\partial p} & = k/p + \frac{n - k}{p - 1} 
\end{align}
$$

To partially verify the above in sympy:
```python
p, n, k = symbols('p n k')
diff(log(p**k) - log((1-p)**(n-k)), p)
```

)")

set.seed(7)
Water <- 6
Land <- 3
U_globe <- function(q) {
  p <- q[1]
  U_globe <- dbinom(Water, Water + Land, p, log=TRUE)
  return (-U_globe)
}

U_globe_gradient <- function(q) {
  p <- q[1]
  grad_p = Water/p + Land/(p-1)
  return (-grad_p)
}

Q_globe <- list()
Q_globe$q <- c(0.5)
n_samples <- 1000
step <- 0.02
L <- 4

p = iplot(function() {
  p <- rep(NA, n_samples)
  plot(NULL, ylab = "p", main = "HMC2 chain", xlab = "sample", xlim = c(1,n_samples), ylim = c(0, 1))
  p[1] <- Q_globe$q[1]
  points(1, p[1], pch = 4, col = "black")
  for (i in 2:n_samples) {
    Q_globe <- HMC2(U_globe, U_globe_gradient, step, L, Q_globe$q)
    p[i] = Q_globe$q
    points(i, p[i],
      pch = ifelse(Q_globe$accept == 1, 16, 1),
      col = ifelse(abs(Q_globe$dH) > 0.1, "red", "black")
    )
  }
  return (p)
})

iplot(function() {
  par(mfrow=c(1,2))
  plot(1:100, p[1:100], main="Samples from posterior", xlab="sample", ylab="p")
  dens(p, xlim = c(0, 1))
  curve(dbeta(x, Water + 1, Land + 1), lty = 2, add = TRUE)
  title(main="Sampled/analytical posterior")
}, ar=1.8)
