#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

library(tidyverse)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2023/blob/main/datasets/climateSupport.RData?raw=true"))
summary(climateSupport)

#check for na values

sapply(climateSupport,function(x) sum(is.na(x)))

#1. 

levels(climateSupport$countries)

model <- glm(choice ~ countries 
             + sanctions,family=binomial(link='logit'),data=climateSupport)
summary(model)

(exp(-.0057)) / (1 + exp(-.0057))

#The probability that someone will support a policy is .4986.

#Chi Square
X2 <- 111783 - 11568
X2

#There are p - 5 predictor variables degrees of freedom


(exp(-.0057)) / (1 + exp(-.0057))


#2. a: climateSupport$sanctions)
exp(-.01811)


1-.9821 = 17.9%

Policies with 15% sanctions are associated with a 17.9%
reduction in policy support. 


#2b

(-.0057)+(-0.0010)

ep =(exp(-.0057)+(-0.0010))/(1 + exp((-.0057)+(-0.0010)))
ep


#2(c) Would the answers to 2a and 2b potentially change if we included the interaction
#term in this model? Why?


#likelihood ratio test. 

model2 <- glm(choice ~ countries 
             * sanctions,family=binomial(link='logit'),data=climateSupport)
summary(model2)
install.packages('lmtest')
library(lmtest)

lrtest(model, model2)

