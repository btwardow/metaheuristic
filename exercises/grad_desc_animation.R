#
# Metaheuristic Exercise
#
# 2014 (c) Bartlomiej Twardowski
# B.Twardowski@ii.pw.edu.pl

# install packege - if not present
#install.packages('animation')

library(animation)
par(mar = c(4, 4, 2, 0.1))
?grad.desc
gd = grad.desc(interact = TRUE, gamma = 0.1)
gd$par  # solution
gd$persp(col = "lightblue", phi = 30)

ani.options(nmax = 70)
par(mar = c(4, 4, 2, 0.1))
f2 <- function(x, y) sin(1/2 * x^2 - 1/4 * y^2 + 3) * cos(2 * x + 1 - exp(y))
range = c(-2, -2, 2, 2)
# gradient descent
gd <- grad.desc(f2, range, c(-1, 0.5), gamma = 0.3, tol = 1e-04, interact = TRUE)
# result
gd$par
# function visualization
gd$persp(col = "lightblue", theta = 30, phi = 30)
