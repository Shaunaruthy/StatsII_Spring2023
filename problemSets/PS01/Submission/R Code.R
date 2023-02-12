#Question 1
library(dplyr)

set.seed(123)
data <- rcauchy(1000, location = 1, scale = 1)

ECDF <- ecdf(data)
empiricalCDF <- ECDF(data)

s <- data.frame(pnorm(data))
dfCDF <- data.frame(empiricalCDF)

D <- max(abs(empiricalCDF-pnorm(data)))

#Function for working out the critical value:
critval<- 1.358 * sqrt(count(s) + count(dfCDF))/ (count(s) * count(dfCDF)) 
critval

crit_Val <- function(x) {
  cv<- 1.358 * sqrt(count(data.frame(pnorm(data))) + count(dfCDF))/ (count(data.frame(pnorm(data))) * count(dfCDF))
  d <- max(abs(empiricalCDF-pnorm(data)))
  print(paste("Asymptotic two-sample Kolmogorov-Smirnov test.
  D = ", d, "critical value =", cv))
  
}



crit_Val(empiricalCDF)


pValue = sum(empiricalCDF >= D) / count(data.frame(empiricalCDF))
pValue

nrow(dfCDF)

ks_test <- function(x, d){
  #Run Kolmogorov-Smirnov test on x = data and d = D test statistic.
  pv <- sum(x >= d)/ nrow(data.frame(x))
  print(paste("Asymptotic two-sample Kolmogorov-Smirnov test.
  D = ", d, "p value ="))
  print(pv)
}

ks_test(empiricalCDF, D)




#TESTING 
?ks.test
t <- ks.test(pnorm(data), empiricalCDF)

t



ks.test(empiricalCDF, pnorm,
        alternative = c("two.sided", "less", "greater"),
        exact = NULL, simulate.p.value = FALSE, B = 2000)


t$p.value

plot(pnorm(data),empiricalCDF)


         
#Question 2:
#Estimate an OLS regression in R that uses the Newton-Raphson
#(specically BFGS,which is a quasi-Newton method)
#show that you get the equivalent results to using lm.
library(tidyverse) 

set.seed(123)

datas <- data.frame(x=runif(200,1,10))

datas$y <- 0 + 2.75*datas$x + rnorm(200,0,1.5)

linear.lik <- function(theta,y,X){
  n <- nrow(X)
  k <- ncol(X)
  beta <- theta [1:k]
  sigma2 <- theta[k+1]^2
  e <- y - X%*%beta
  logl <- -.5*n*log(2*pi)-.5*n*log(sigma2)-((t(e)%*%
                                               e)/(2*sigma2))
  return(-logl)}

ols_R <- optim(fn=linear.lik, par = c(1,1,1), hessian = TRUE,
                    y=datas$y, X=cbind(1,datas$x), method = "BFGS")

ols_R$par

#compare to LM

lm(datas$y ~ datas$x)



