
# I used Session menu to set up working directory.

# Import flixIT INC data
flixit <- read.csv("FlixIt_2022.txt",header = TRUE, sep = " ")

# Checking the data.
head(flixit,5)
tail(flixit,5)
summary(flixit)

flix.logit <- glm(Partic~Age, data = flixit, family= binomial(link="logit"))
summary(flix.logit)
AgeVals <- seq(20,70,1)
NewData <- data.frame(AgeVals)
NewData['PredVal'] <- predict(flix.logit, list(Age = NewData$AgeVals), type = "link")
NewData['PredProb'] <- exp(NewData$PredVal) / (1+ exp(NewData$PredVal))
plot(NewData$Age, NewData$PredProb, pch = 16, xlab = "Age", ylab = "Predicted Probability")

# Question 2
install.packages("pscl")
library(pscl)
pR2(flix.logit)

# Question 3
summary(flix.logit)
exp(0.27695) 
# Log = -12.96833 + 0.27695 * Age
# odds = exp(-12.96833 + 0.27695 * Age)
# prob = odds / (1 + odds)

# Question 4
install.packages("InformationValue")
library(InformationValue)

flixit['PredVal'] <- predict(flix.logit, list(Age = flixit$Age), type = "link")
flixit['Predprob'] <- predict(flix.logit, list(Age = flixit$Age), type = "response")
head(flixit)
summary(flix.logit)
  
confusionMatrix(flixit$Partic,flixit$Predprob,0.5)
sensitivity(flixit$Partic,flixit$Predprob,0.5)
specificity(flixit$Partic,flixit$Predprob,0.5)
precision(flixit$Partic,flixit$Predprob,0.5)
npv(flixit$Partic,flixit$Predprob,0.5)

(134 + 29) / (134+29+15+22) * 100

# Question 5 ~ 8
#     0  1
# 0 134 22
# 1  15 29

# 5.
# Participant & Incorrectly non-participant (FN / FN + TP)
22 / (22+29) * 100
# 6. 
# Non - Participant & Incorrectly participant (FP / FP + TN)
15 / (15 + 134) * 100
# 7.
# Participant & incorrectly classify 
15 / (15 + 29) * 100
# 8.
# non - participant & Incorrectly classify
22 / (134 + 22) * 100
#Question 9 ~
logit <- predict(flix.logit, newdata = data.frame(Age = c(30,40)), type = "link")
odds <- exp(logit)
odds

# 40 years old participates compared to 30 years old
odds[2] / odds[1]

# probability 
prob <- odds / (1+odds)
prob

0.009468481 /(1+0.009468481 )

# Question 13
install.packages("pROC")
library(pROC)
ROC.curve = roc(Partic~Age , data = flixit)
plot(ROC.curve, col = "red")
auc(ROC.curve)



# Question 14
# install.packages("InformationValue")
library(InformationValue)
optimalCutoff(flixit$Partic,flixit$Predprob)

# Question 15
confusionMatrix(flixit$Partic,flixit$Predprob,0.3814781)
(132+37) / (132+37 + 17 + 14)

# Question 16
install.packages("ResourceSelection")
library(ResourceSelection)

hoslem.test(flixit$Partic, fitted(flix.logit), g = 10)
0.1441 > 0.05
