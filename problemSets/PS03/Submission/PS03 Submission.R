#reg and oil
#GDPWDIFF


#Construct and interpret an unordered multinomial 
#logit with GDPWdiff as the output
#and "no change" as the reference category, 
#including the estimated cuto points and
#coecients.

require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
require(nnet)
library(stargazer)

require(MASS)
require(Hmisc)
require(reshape2)



install.packages("stargazer")

install.packages("MASS")

gdpChange$GDPWdiff2 <- cut(gdpChange$GDPWdiff,
                               breaks=c(-100000, -1, 1, 10000),
                               labels = c("negative",
                                          "no change",
                                          "positive"))

gdpChange$OIL <- factor(gdpChange$OIL,
                           levels = c(0,1),
                           labels = c("DoesnotExceed50%", "Exceeds50%"))


gdpChange$REG <- factor(gdpChange$REG,
                        levels = c(0,1),
                        labels = c("NonDemocracy","Democracy"))



gdpChange$GDPWdiff2 <- relevel(factor(gdpChange$GDPWdiff2), ref = "no change")


test <- multinom(GDPWdiff2 ~ OIL + REG, data = gdpChange)
summary(test)




#P values for the model

z <- summary(test)$coefficients/summary(test)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

#The p values indicate that the variable REGDemocracy
#has a statistically significant effect on being in the positive GDP difference
#category vs the no-change category. Our coefficient indicates this is a positive
#effect.


#The final value of the model is 2384.132.
#A one-unit increase in the variable REGDemocracy is associated with an increase
#in the log odds of being in the negative GDP category VS the no-change GDP category
#in the amount of 1.012


#A one-unit increase in the variable OILExceeds50% is associated with an increase
#in the log odds of being in the negative GDP category VS the no-change GDP category
#in the amount of .575


#A one-unit increase in the variable REGDemocracy is associated with an increase
#in the log odds of being in the positive GDP category VS the no-change GDP category
#in the amount of 1.401

#A one-unit increase in the variable OILExceeds50% is associated with an increase
#in the log odds of being in the positive GDP category VS the no-change GDP category
#in the amount of .362.


#Relative Risk:

exp(coef(test))

#Keeping all other variable constant, if REGDemocracy increases by one unit, the country
#is 2.752 times more likely to be in the negative GDPWdiff category vs. the no- change category.
# Keeping all other variable constant, if REGDemocracy increases by one unit, the country
#is 4.075 times more likely to be in the positive GDP diff category
#than the no - change category. 

# Keeping all other variable constant, if OILExceeds50% increases by one unit, the country
#is 1.776 times more likely to be in the negative GDP diff category
#than the no - change category. 

# Keeping all other variable constant, if OILExceeds50% increases by one unit, the country
#is 1.436 times more likely to be in the positive GDP diff category
#than the no - change category. 




#2. Construct and interpret an ordered multinomial logit with GDPWdiff as the outcome
#variable, including the estimated cuto points and coecients.



model_fit <- polr(GDPWdiff2 ~ OIL + REG, data = gdpChange, Hess = TRUE)
summary(model_fit)

summary_table <- coef(summary(model_fit))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table

coef(model_fit)

#p value

(ctable <- coef(summary(model_fit)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

p

(ctable <- cbind(ctable, "p value" = p))

#Our p values indicate that all the variables are statistically
#significant contributors to the model except for OILExceeds50.

#For a one unit increase in OILExceeds50 (i.e., going from 0 to 1), we expect a -.206
#decrease in the expected value of GDPDiff on the log odds scale, given all of the 
#other variables in the model are held constant

#For a one unit increase in REGDemocracy (i.e., going from 0 to 1), we would expect 
#a .402 increase in the expected value of GDPDiff in the log odds scale, 
#given that all of the other variables in the model are held constant.

#odds ratio

(ci <- confint(model_fit))
exp(cbind(OR = coef(model_fit), ci))




#Q2
#Run a Poisson regression because the outcome is a count variable. Is there evidence
#that PAN presidential candidates visit swing districts more? Provide a test statistic
#and p-value.

require(ggplot2)
require(sandwich)
require(msm)

install.packages("msm")

MexicoMuniData <- within(MexicoMuniData, {
  competitive.district <- as.logical(competitive.district)
  PAN.governor.06 <- as.logical(PAN.governor.06)
})


str(MexicoMuniData)
summary(MexicoMuniData)

with(MexicoMuniData,
     list(mean(PAN.visits.06), var(PAN.visits.06)))



hist(MexicoMuniData$PAN.visits.06)

ggplot(MexicoMuniData, aes(competitive.district, PAN.visits.06, color = marginality.06)) +
  geom_jitter(alpha = 0.5)


#run the poisson model
poisson1 <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data = MexicoMuniData, family = poisson)
summary(poisson1)

#The intercept has a coefficient of -3.810, this is the log
#of expected count of state visits when all other variables are zero.





# interpreting outputs
cfs <- coef(poisson1)
cfs

#Due to slight variance and mean difference robust standard errors are used:

cov.poisson1 <- vcovHC(poisson1, type="HC0")
std.err <- sqrt(diag(cov.poisson1))
r.est <- cbind(Estimate= coef(poisson1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(poisson1)/std.err), lower.tail=FALSE),
               LL = coef(poisson1) - 1.96 * std.err,
               UL = coef(poisson1) + 1.96 * std.err)

#provide test statistic and p value
with(poisson1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))
#We conclude that the model fits reasonably well because the goodness-of-fit chi-squared test is not statistically significant.



#Interpret the marginality.06 and PAN.governor.06 coecients.


#For all other factors being held constant, a one-unit increase in marginality is
#associted with a decrease of state visits by a factor of .125 (exp(-2.0814). It's P value indicates it is a highly 
#significant predictor of State Visits. 

#For all other factors being held constant, a one-unit increase in PAN.governor.06 
#is associated with a decrease in the expected number of state visits by a factor of
#.732 (exp(-.3116)). It;s p value indicates it is not a highly significant predictor
#of state visits.

exp(-0.3116)


#The expected log count for a one-unit increase in PAN.governor.06 , so having a PAN
#affiliated governor versus not, is -.3116

#Provide the estimated mean number of visits from the winning PAN presidential candi-
#date for a hypothetical district that was competitive (competitive.district=1), had
#an average poverty level (marginality.06 = 0), and a PAN governor (PAN.governor.06=1).



pred <- data.frame(competitive.district = TRUE,
                   marginality.06 = 0,
                   PAN.governor.06 = TRUE)

predict(poisson1, newdata = pred, type = "response")

cfs <- coef(poisson1)
cfs


ggplot(data = NULL, aes(x = poisson1$fitted.values, y = MexicoMuniData$competitive.district)) +
  geom_jitter(alpha = 0.5) +
  geom_abline(color = "blue")+
  geom_smooth(method = "loess", color = "red")


