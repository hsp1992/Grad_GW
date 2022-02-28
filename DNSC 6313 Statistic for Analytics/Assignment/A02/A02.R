library(dplyr)

#A02

df <- Daxguard

summary(df)
typeof(df$Age)
typeof(df$Gender)
typeof(df$Sysdiff)

df$Gender <- as.factor(df$Gender)


# Question 1

total.lm <- lm(Sysdiff~Age , data = df)
summary(total.lm)


# Question 2

male <- subset(df, Gender == 'M')
male.lm <- lm(Sysdiff~Age, data = male)
summary(male.lm)

# Quesiton 3

female <- subset(df, Gender =='F')
female.lm <- lm(Sysdiff~Age, data = female)
summary(female.lm)

# Question 4

plot(1, type = "n",
     xlab = "age", ylab = "Sysdiff",
     xlim = c(-10,100), ylim = c(-10,100))
abline(female.lm, lty = 1)
abline(male.lm, lty = 2)
legend("bottomright", c("F","M"), lty=c(1,2))

# Question 5
summary(male.lm)

# Question 6
total.lm <- lm(Sysdiff~Age + Gender + Age*Gender, data = df)
summary(total.lm)

summary(df) # to check the average of age
  # Age mean is 49.51
predict(total.lm, data.frame(Gender ="F", Age = 49.51)) - predict(total.lm, data.frame(Gender ="M", Age = 49.51))

# Question 7
  # Main effect of Age = Average of slope
summary(male.lm) #0.50252
summary(female.lm)# -0.46563
(-0.46563 + 0.50252) / 2

# Question 8
female_49.51 <- predict(total.lm, data.frame(Gender ="F", Age = 49.51))
female_49.51

male_49.51 <- predict(total.lm, data.frame(Gender ="M", Age = 49.51))
male_49.51

female_49.51 - male_49.51

# Question 9
#Subtract Sysdiff of Male,30 to Sysdiff of Female,25
predict(total.lm, data.frame(Gender = "M", Age = 30)) -
  predict(total.lm, data.frame(Gender = "F", Age = 25))
