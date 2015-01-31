require('xlsx')
stock_data <- read.xlsx('./data/PLMATRX00017.xls', 1)
# Date type conversion
stock_data$Data=as.Date(stock_data$Data,format="%Y-%m-%d")

summary(stock_data)
plot(stock_data$Kurs.zamkniecia~stock_data$Data)


stock.week.avg <- as.numeric(
  aggregate(
    stock_data$Kurs.zamkniecia,
    by=list(floor((stock_data$Data-stock_data$Data[1])/7)),
    FUN=mean)$x
)


par(mfrow=c(1,2))
with(stock_data, {
  plot(Kurs.zamkniecia~Data,type='l',xlab="Date",ylab="Closing price(PLN)")
})
plot(stock.week.avg,type='l',xlab="Week",ylab="Avg closing price(PLN)")

predict_price <- function(weights=c(w1,w2,w3,w4), weekNum, weekAvg=stock.week.avg) {
  prediction <- 0
  weight_sum <-0
  for(i in 1:length(weights)) {
    if(weekNum>i) {
      prediction <- prediction + weights[i]*weekAvg[weekNum-i] 
      weight_sum <- weight_sum + weights[i]
    }
  }
  return (prediction/weight_sum)
}

predict_price(c(1,1,1,1),5,stock.week.avg)

stock.week.avg[5]

predict_eval <- function(weights=c(w1,w2,w3,w4), weekAvg = stock.week.avg){
  predictions <-as.numeric(lapply(4:length(weekAvg),
                                  FUN=function(w) predict_price(weights,w,weekAvg)
  )
  )
  
  ## data normalization!
  max_val=max(abs(c(weekAvg[-(1:3)],predictions)))
  return(sqrt(mean((weekAvg[-(1:3)]/max_val-predictions/max_val)^2)))
}

predict_eval(c(1,1,1,1))


all_week_predictions<-function(par,weekAvg=stock.week.avg) {
  return (as.numeric(lapply(
    2:length(weekAvg),
    FUN=function(n) predict_price(par,n,weekAvg)
  )))
}


#PSO

library(pso)
#random initial weights
init_weights=runif(4,0,1)
result_pso<-psoptim(init_weights, predict_eval, gr=NULL, stock.week.avg,
                 lower=rep(0,length(init_weights)), 
                 upper=rep(1,length(init_weights)),
                 control=list(maxit=100,type='SPSO2011',reltol=1e-8,vectorize=TRUE,trace=1))

result_pso$par #best weights
result_pso$value

#SA
init_weights=runif(4,0,1)
result_sa <- optim(init_weights, predict_eval, gr=NULL, stock.week.avg,
                   method = "SANN",
                   control = list(maxit = 10000, temp = 10000, trace = TRUE,
                                  REPORT = 100, tmax=10)
)
result_sa$par
result_sa$value

#GA
#install.packages('GA')
require(GA)
init_weights=runif(4,0,1)

result_ga <- ga("real-valued", predict_eval, stock.week.avg,
                min=rep(0,length(init_weights)), 
                max=rep(1,length(init_weights)),
                maxiter = 1000
)
summary(result_ga)
result_ga@solution


#plot
par(mfrow=c(1,1))
plot(stock.week.avg,type='o',
     ylim=c(min(stock.week.avg),max(stock.week.avg)),xlab="Week",ylab="Avg price(PLN)")


pso_predictions <- all_week_predictions(result_pso$par)
points(pso_predictions,type='o',col='red')

dummy_predictions <- all_week_predictions(c(1,1,1,1))
points(dummy_predictions,type='o',col='green')

sa_predictions <- all_week_predictions(result_sa$par)
points(sa_predictions,type='o',col='blue')

ga_predictions <- all_week_predictions(result_ga@solution)
points(ga_predictions,type='o',col='orange')

legend('topleft', c('Real Value', 'Particle Swarm', 'Dummy', 'Simulated Anneling', 'Genetic Algorithm') , 
       lty=1, col=c('black', 'red', 'green','blue', 'orange'), bty='n', cex=.75)

