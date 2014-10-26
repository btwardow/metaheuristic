#install.packages("GA")
library(GA)

Rastrigin <- function(x){
  fact=10^9
  
  penalty=(abs(x[1]+x[2]-4)
           +abs(x[3]+x[4]-2)+abs(x[1]+x[3]-5)+abs(x[2]+x[4]-1))

  return(c(1000,1250,1350,1500)%*%x+fact*penalty)
}

GA <- ga(type = "real-valued", fitness =  function(x) -Rastrigin(x),
         min = c(0, 0,0,0), max = c(4, 4,2,2), 
         popSize = 50, maxiter = 100)

summary(GA)
plot(GA)
