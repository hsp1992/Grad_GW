# Lab 3 
head(AAA)
tail(AAA)

# Q1
aaa.lm <- lm(Demand~Price, data = AAA)
summary(aaa.lm)

# Q2
AAA["LnDemand"] <- log(AAA$Demand) 
AAA.lm <-lm(LnDemand~Price, data = AAA)
summary(AAA.lm)
  # For all Dx * b1: %Dy = 100(eDx*b1-1)
100*((exp(-0.50964)-1))

# Q3
AAA["LnPrice"] <- log(AAA$Price)
AAAln.lm <- lm(LnDemand~LnPrice, data = AAA)
summary(AAAln.lm)

# Q4
summary(aaa.lm) # linear : Multiple R-squared:  0.9873
summary(AAA.lm) # Log Lin : Multiple R-squared:  0.9974
summary(AAAln.lm) # Log Log : Multiple R-squared:  0.9989

# Q5
summary(aaa.lm)
9666.81 - (1770.12*3)
# [1] 4356.45

# Q6
InquiryPrice <- 3
lnPredict <- predict(AAA.lm, data.frame(Price = InquiryPrice))
Predict <- exp(lnPredict)
c(3,lnPredict,Predict)
# 3.000000    8.395162 4425.603647 

# Q7
lnInquiryPrice <- log(3)
lnPredict <- predict(AAAln.lm, data.frame(LnPrice = lnInquiryPrice))
Predict <- exp(lnPredict)
c(3,lnInquiryPrice,lnPredict,Predict)
#   3.000000    1.098612    8.407379 4480.002807 
