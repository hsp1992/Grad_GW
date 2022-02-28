# Lab4
# install.packages("gvlma")

library(gvlma)

head(a4,10)
tail(a4,10)
a4.lm <- lm(sales~age+income+education+tenure, data =  a4)
summary(a4.lm)

# question 1~ question 4
a4.gvl <- gvlma(a4.lm)
summary(a4.gvl)

# questino 5
hatvalues(a4.lm)

hv <- as.data.frame(hatvalues(a4.lm))
colnames(hv) <- c("hatvalues")
mn <- mean(hv$hatvalues)
hv$warn <- ifelse(hv$hatvalues >3*mn, 'x3',
                  ifelse(hv$hatvalues > 2*mn, 'x2', '-'))
subset(hv, warn %in% c("x2","x3"))

hv[order(hv$hatvalues),]

# question 6
rstudent(a4.lm)

rs <- as.data.frame(rstudent(a4.lm))
colnames(rs) <- "rstudent"
critval <- qt(.95, nrow(rs)-2-1)
rs$warn <- ifelse(abs(rs$rstudent)>critval,
                  'Warn', '-')
subset(rs, warn == "Warn")

rs[order(rs$rstudent),]


# question 7 ~ question 11
dfbetas(a4.lm)

dfb <- as.data.frame(dfbetas(a4.lm))
critval <- 2/sqrt(nrow(dfb))
dfb$Warn <- ifelse(abs(dfb)>critval, "Warn", "-")
subset(dfb, Warn[,1] == "Warn" | Warn[,2] == "Warn")

# question 12
cooks.distance(a4.lm)

cd <- as.data.frame(cooks.distance(a4.lm))
colnames(cd) <- "CooksD"
critival <- qf(.50,2,nrow(cd)-2)
cd$warn <- ifelse(abs(cd$CooksD)>critival,
                  'Warn', '-')
subset(cd, warn=="Warn")

cd[order(cd$CooksD), ]
