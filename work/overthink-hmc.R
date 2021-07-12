source('iplot.R')
library(rethinking)

## R code 9.5
# U needs to return neg-log-probability
U <- function(q, a = 0, b = 1, k = 0, d = 1) {
  muy <- q[1]
  mux <- q[2]
  U <- sum(dnorm(y, muy, 1, log = TRUE)) + sum(dnorm(x, mux, 1, log = TRUE)) +
    dnorm(muy, a, b, log = TRUE) + dnorm(mux, k, d, log = TRUE)
  return(-U)
}

## R code 9.6
# gradient function
# need vector of partial derivatives of U with respect to vector q
U_gradient <- function(q, a = 0, b = 1, k = 0, d = 1) {
  muy <- q[1]
  mux <- q[2]
  G1 <- sum(y - muy) + (a - muy) / b^2 # dU/dmuy
  G2 <- sum(x - mux) + (k - mux) / d^2 # dU/dmux
  return(c(-G1, -G2)) # negative bc energy is neg-log-prob
}
# test data
set.seed(7)
y <- rnorm(50)
x <- rnorm(50)
x <- as.numeric(scale(x))
y <- as.numeric(scale(y))

## R code 9.7
library(shape) # for fancy arrows
Q <- list()
Q$q <- c(-0.1, 0.2)
pr <- 0.3
step <- 0.03
L <- 11 # 0.03/28 for U-turns --- 11 for working example
n_samples <- 4
path_col <- col.alpha("black", 0.5)

iplot(function() {
  plot(NULL, ylab = "muy", xlab = "mux", xlim = c(-pr, pr), ylim = c(-pr, pr))
  points(Q$q[1], Q$q[2], pch = 4, col = "black")
  for (i in 1:n_samples) {
    Q <- HMC2(U, U_gradient, step, L, Q$q)
    if (n_samples < 10) {
      for (j in 1:L) {
        K0 <- sum(Q$ptraj[j, ]^2) / 2 # kinetic energy
        lines(Q$traj[j:(j + 1), 1], Q$traj[j:(j + 1), 2], col = path_col, lwd = 1 + 2 * K0)
      }
      points(Q$traj[1:L + 1, ], pch = 16, col = "white", cex = 0.35)
      Arrows(Q$traj[L, 1], Q$traj[L, 2], Q$traj[L + 1, 1], Q$traj[L + 1, 2],
        arr.length = 0.35, arr.adj = 0.7
      )
      text(Q$traj[L + 1, 1], Q$traj[L + 1, 2], i, cex = 0.8, pos = 4, offset = 0.4)
    }
    points(Q$traj[L + 1, 1], Q$traj[L + 1, 2],
      pch = ifelse(Q$accept == 1, 16, 1),
      col = ifelse(abs(Q$dH) > 0.1, "red", "black")
    )
  }
})

## R code 9.8
HMC2 <- function(U, grad_U, epsilon, L, current_q) {
  q <- current_q
  p <- rnorm(length(q), 0, 1) # random flick - p is momentum.
  current_p <- p
  # Make a half step for momentum at the beginning
  p <- p - epsilon * grad_U(q) / 2
  # initialize bookkeeping - saves trajectory
  qtraj <- matrix(NA, nrow = L + 1, ncol = length(q))
  ptraj <- qtraj
  qtraj[1, ] <- current_q
  ptraj[1, ] <- p

  ## R code 9.9
  # Alternate full steps for position and momentum
  for (i in 1:L) {
    q <- q + epsilon * p # Full step for the position
    # Make a full step for the momentum, except at end of trajectory
    if (i != L) {
      p <- p - epsilon * grad_U(q)
      ptraj[i + 1, ] <- p
    }
    qtraj[i + 1, ] <- q
  }

  ## R code 9.10
  # Make a half step for momentum at the end
  p <- p - epsilon * grad_U(q) / 2
  ptraj[L + 1, ] <- p
  # Negate momentum at end of trajectory to make the proposal symmetric
  p <- -p
  # Evaluate potential and kinetic energies at start and end of trajectory
  current_U <- U(current_q)
  current_K <- sum(current_p^2) / 2
  proposed_U <- U(q)
  proposed_K <- sum(p^2) / 2
  # Accept or reject the state at end of trajectory, returning either
  # the position at the end of the trajectory or the initial position
  accept <- 0
  if (runif(1) < exp(current_U - proposed_U + current_K - proposed_K)) {
    new_q <- q # accept
    accept <- 1
  } else {
    new_q <- current_q
  } # reject
  return(list(q = new_q, traj = qtraj, ptraj = ptraj, accept = accept))
}
