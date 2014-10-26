#install.packages('GA')
library(GA)


# liczba dostawcow D
# liczba fabryk F
#laczna produkcja sD
#laczne zapotrzebowanie sF
randTransportProblem <- function(F,D,sF,sD){
  #koszty K
  pp=round(runif(F*D,1000,2000))
  K=matrix(pp,D)
  #ograniczenia produkcji
  oD=rmultinom(1,sD,prob=rep(1/2,D))
  #ograniczenia zaotrzebowania
  oF=rmultinom(1,sF,prob=rep(1/2,F))
  
  return (list(K=K,oD=oD,oF=oF))
}

# maksymalizowana funkcja

Rastrigin <- function(x,K,oD,oF,D,F)
{
  fact=10^6
  
  X=matrix(x,D)
  penalty=sum(abs(X%*%rep(1,F)-oD))+sum(abs(t(X)%*%rep(1,D)-oF))
  
  return(as.numeric(K)%*%x+fact*penalty)
}

D=?
F=?
sD=?
sF=?
ret=randTransportProblem(F,D,sF,sD)


GA <- ga(type = "real-valued", fitness =  function(x) -Rastrigin(x,ret$K,ret$oD,ret$oF,D,F),
         min = ?, max = ?,
         popSize = ?, maxiter = ?)
summary(GA)
plot(GA)