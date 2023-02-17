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

#Convert explanatory variables to scores



library(tidyverse)


levels(climateSupport$countries)

climateSupport$countries <- ifelse(climateSupport$countries == "20 of 192", 1, ifelse(climateSupport$countries == "80 of 192", 2, 3))

summary(climateSupport)

climateSupport$sanctions <- ifelse(climateSupport$sanction == "None", 0, ifelse(climateSupport$sanctions == "5%", 2, 3, 4))
climateSupport$sanctions <- ifelse(climateSupport$sanctions == "None", 0, ifelse(climateSupport$sanctions == "5%", 1, ifelse(climateSupport$sanctions == "15%", 2, ifelse(climateSupport$sanctions == "20%", 3,4))))
summary(climateSupport)

model <- glm(choice ~ countries 
             + sanctions,family=binomial(link='logit'),data=climateSupport)
summary(model)


