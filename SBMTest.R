require(Rcpp)

sourceCpp('cpp/main.cpp')

Y = matrix(c(0, 1, 2, 2, 0, 1, 3, 1, 0), ncol = 3)
Y

#       [,1] [,2] [,3]
# [1,]    0    2    3
# [2,]    1    0    1
# [3,]    2    1    0

# This is a vector representing top-right triangle of the Pi matrix
Pi = c(0.1)
Pi

#       [,1] [,2]
# [1,]  0.5  0.1
# [2,]  0.9  0.5

z = c(0, 1, 0)

logLikelihood(Y, z, Pi) ==
  log(choose(3, 2) * 0.1 ** 2 * 0.9 ** 1) +
  log(choose(5, 3) * 0.5 ** 3 * 0.5 ** 2) +
  log(choose(3, 1) * 0.9 ** 1 * 0.1 ** 2) +
  log(choose(2, 1) * 0.9 ** 1 * 0.1 ** 1) +
  log(choose(5, 2) * 0.5 ** 2 * 0.5 ** 3) +
  log(choose(2, 1) * 0.1 ** 1 * 0.9 ** 1) 
# TRUE

model <- runMetropolisHastings(Y, numCommunities = 2, burnIn = 200, numIterations = 800)

# top-right element of Pi
apply(model$pi, 1, mean)

# frequencies of group assignments Z
apply(model$z, 1, table)