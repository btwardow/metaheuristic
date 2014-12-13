#
# Metaheuristic Exercise
#
# 2014 (c) Bartlomiej Twardowski
# B.Twardowski@ii.pw.edu.pl

metaheuristic <- function(X, y, S_init, quality, tweek, max_iter = 1000, threshold = 0.00001) {
  i <- 0
  S <- S_init
  quality_h <- double(max_iter) #history
  S_h <- list(max_iter) #history
  repeat {
   i <- i + 1
   R <- tweak(S)
   if(quality(R,X,y) < quality(S,X,y))
     S <- R
   #save to history
   quality_h[i] <- quality(S,X,y)
   S_h[[i]] <- S
   if(quality(S,X,y) <= threshold || i == max_iter)
     break
  }
  list(S=S,quality=quality(S,X,y), S_history=S_h, quality_history=quality_h)
}

quality <- function(S, X, y) {
  #TODO: implement!!!
  0.0
}
  
tweak <- function(S) { 
  #TODO: implement!!!
  S 
}

## Run on cars dataset
rm(cars)
require(datasets)
cars$speed_km <- cars$speed * 1.609344
cars$dist_m <- cars$dist * 0.3048

S_init <- matrix(c(0,0), nrow=2)
X <- cbind(1, matrix(cars$speed_km))
y <- cars$dist_m
iterations <- 1000
# TODO: implement!!!!
#result <- metaheuristic(...)

plot(cars$speed_km, cars$dist_m, xlab = "Speed (km/h)", ylab = "Stopping distance (m)", 
     las = 1, main='Linear regression by Hill-Climbing')

# plot data and converging fit
for (i in c(1,3,6,10,14,seq(20,iterations,by=10))) {
  abline(coef=result$S_history[[i]], col="gray")
}
abline(coef=result$S, col="red")

# check out the trajectory of the cost function
result$quality_history[seq(1,iterations, by=10)]
plot(result$quality_history, type='l', col='blue', lwd=2, main='Quality function', ylab='quality(S,X,y)', xlab='Iterations')

##
## Hill Climbing - Harder Example
##
range2 = c(-4, -4, 4, 4)
x2 <- seq(range2[1], range2[3], length= 100)
y2 <- seq(range2[2], range2[4], length= 100)
f2 <- function(x, y) { sin(1/2 * x^2 - 1/4 * y^2 + 3) * cos(2 * x + 1 - exp(y)) }
z2 <- outer(x2,y2, f2)
persp(x2, y2, z2, theta = 10, phi = 30)
contour(x2,y2,z2)

quality2 <- function(S, X, y) {
 f2(S[1],S[2]) - y 
}

tweak2 <- function(S) { 
  S + runif(nrow(S), min=-0.1, max=0.1)
}

S_init2 <- matrix(c(0,0), nrow=2)
iterations2 <- 100
result2 <- metaheuristic(x2, y2, S_init2, quality2, tweek2, iterations2)
contour(x2,y2,z2)
points(result2$S[1,1],result2$S[2,1], col='red') 
