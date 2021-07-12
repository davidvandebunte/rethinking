## R code 6.2
N <- 100 # number of individuals
set.seed(909)
height <- rnorm(N, 10, 2) # sim total height of each
leg_prop <- runif(N, 0.4, 0.5) # leg as proportion of height
leg_left <- leg_prop * height + # sim left leg as proportion + error
  rnorm(N, 0, 0.02)
leg_right <- leg_prop * height + # sim right leg as proportion + error
  rnorm(N, 0, 0.02)
# combine into data frame
d <- data.frame(height, leg_left, leg_right)
