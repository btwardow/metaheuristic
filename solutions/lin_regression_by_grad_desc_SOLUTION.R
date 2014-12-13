#
# Metaheuristic Exercise
# Simple implementation of loggistic regression
# using gradient descent.
#
# Based on Coursera Machine Learning Course by 
# Andrew Ng
#
# 2014 (c) Bartlomiej Twardowski
# B.Twardowski@ii.pw.edu.pl

## cost function to minimize
cost_fun <- function(X, y, theta) {
  #TODO: implement cost function
  sum( (X %*% theta - y)^2 ) / (2*length(y))
}

## Gradient descent function
# X - observation data matrix
# y - dependent variable vector
# init_theta = initial theta (coefficients)
# cost_fun - cost computation function (fittness function)
# alpha - learning parameter
gradient_descent <- function(X, y, init_theta, cost_f = cost_fun,  alpha = 0.01 , num_iters = 1000) {
  cost_h <- double(num_iters) #history
  theta_h <- list(num_iters) #history
  theta <- init_theta
  # add a column of 1's for the intercept coefficient
  X <- cbind(1, matrix(X))
  # gradient descent
  for (i in 1:num_iters) {
    error <- (X %*% theta - y)
    delta <- t(X) %*% error / length(y) #gradient calc.
    theta <- theta - alpha * delta # update step
    # save history
    cost_h[i] <- cost_fun(X, y, theta)
    theta_h[[i]] <- theta
  }
  return(list(theta=theta_h[[num_iters]], cost=cost_h[num_iters], theta_history=theta_h, cost_history=cost_h))
}

## Run on cars dataset
require(datasets)
## TODO: change spped and distance to metric values 
cars$speed <- cars$speed * 1.609344
cars$dist <- cars$dist * 0.3048

## TODO: Do dataset analysis


# initialize coefficients
theta_init <- matrix(c(0,0), nrow=2)
iterations <- 100
# run gradient descent
gd_result <- gradient_descent(X = cars$speed, 
                              y = cars$dist, 
                              cost_f = cost_fun, 
                              init_theta = theta_init,
                              alpha = 0.0001,
                              num_iters = iterations
)


plot(cars, xlab = "Speed (km/h)", ylab = "Stopping distance (m)", 
     las = 1, col=rgb(0.2,0.4,0.6,0.4), 
     main='Linear regression by gradient descent')

# plot data and converging fit
for (i in c(1,3,6,10,14,seq(20,iterations,by=10))) {
  abline(coef=gd_result$theta_history[[i]], col=rgb(0.8,0,0,0.3))
}
abline(coef=gd_result$theta, col="blue")

# check out the trajectory of the cost function
gd_result$cost_history[seq(1,iterations, by=10)]
plot(gd_result$cost_history, type='l', col='blue', lwd=2, main='Cost function', ylab='cost', xlab='Iterations')

# TODO: compare with model from lm()
lm1 <- lm(dist~speed, cars)
# print result info
lm1
# plot result on data model
plot(cars)
abline(lm1, col="red")
# plot detail information about lm1
plot(lm1)

