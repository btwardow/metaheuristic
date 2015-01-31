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

predict_price <- function(weights=c(w1,w2,w3,w4), weekNum, weekAvg = stock.week.avg) {
  prediction <- 0
  count <-0
  for(i in 1:length(weights)) {
    if(weekNum>i) {
      prediction <- prediction + weights[i]*weekAvg[weekNum-i] 
      count <- count + 1
    }
  }
  return (prediction/count)
}

predict_price(c(1,1,1,1),5)

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

library(pso)
#random initial weights
init_weights=runif(4,0,1)
opt_par<-psoptim(init_weights, predict_eval, gr=NULL, stock.week.avg,
                 lower=rep(0,length(init_weights)), 
                 upper=rep(1,length(init_weights)),
                 control=list(maxit=100,type='SPSO2011',reltol=1e-8,vectorize=TRUE,trace=1))

opt_par$par #best weights

opt_par$value


test_prediction <-as.numeric(lapply(
  2:length(stock.week.avg),
  FUN=function(w) predict_price(opt_par$par,w,stock.week.avg)
))

test_prediction

par(mfrow=c(1,1))
plot(test_prediction,type='o',col='red',ylim=c(min(test_prediction),max(test_prediction)),xlab="Week",ylab="Avg price(PLN)")
points(stock.week.avg,type='o')


