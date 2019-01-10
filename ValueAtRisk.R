## Numerische Methoden in der Finanzwirtschaft - Homework 2b
## Anton Wohlgemuth
## 11778996

ytm <- function(C, NV, s1, s2, tol) {
  # c ... coupon
  # NV ... notional value
  # s1 s2 ... spot rates
  
  MV <- function(C,NV,s1,s2) C/(1+s1) + (C+NV)/(1+s2)^2
  MV <- MV(C,NV,s1,s2)
  f <- function(x) abs(C/(1+x) + (C+NV)/(1+x)^2 - MV)

  # optimize solution
  #ytm <- optimize(f, c(0.01, 0.1))$minimum
  
  
  # newton solution
  
  x0 <-  mean(c(s1,s2))  #start value for x0
  f1 <- function(x) C * (-1) * (1+x)^(-2) + (C+NV) * (-2) * (1+x)^(-3)
 
  newton <- function(x0) x0 - f(x0)/f1(x0)
  
  diff <- tol+1
  
 while(diff > tol)
  {
    x1 <- newton(x0)
    diff <- abs(x0 - x1)
    x0 <- x1
  }
  x0
}

# ytm (5, 100, 0.04, 0.06, 0.000000001)


## BONUS - TASK


ytm.bonus <- function(C, NV, sT, tol) {
  
  # c ... coupon
  # NV ... notional value
  # sT ... vector containing all values for s1, s2, .. sT
  
  # MV <- function(C,NV,s1,s2) (C+NV)/(1+sT[i])^i
  # MV <- MV(C,NV,s1,s2)
  
  MV <- 0
  
  for ( i in 1:length(sT))
  {
    MV <- (C+NV)/(1+sT[i])^i
  }

  t <- length(sT)
  
  f <- function(ym) {
    x = 0
    for (i in 1:t) {
      x = x + C/((1+ym)**i)
    }
    x = x + NV/((1+ym)**i)
    abs(x - MV)
  }
  
  diff <- tol + 1 # startvalue
  ym0 = mean(sT)
  
  while (diff > tol) {
    ym1 <- optimize(f,c(min(sT),max(sT)),tol=tol)$minimum
    diff <- abs(ym0-ym1)
    ym0 <- ym1
  }
  
  ym0
}

ytm.bonus (5, 100, c(0.04,0.05), 0.1)

