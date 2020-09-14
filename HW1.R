
x <- 1:50
w <- 1 + sqrt(x)/2
example1 <- data.frame(x=x, y= x + rnorm(x)*w)
attach(example1)

fm <- lm(y ~ x)
summary(fm)

lrf <- lowess(x, y)
plot(x, y)
lines(x, lrf$y)
abline(0, 1, lty=3)
abline(coef(fm))

detach()

load("acs2017_ny_data.RData")
#glimpse(acs2017_ny) try this later
acs2017_ny[1:10,1:7]

summary(acs2017_ny)

print(NN_obs <- length(acs2017_ny$AGE))

summary(acs2017_ny$AGE[acs2017_ny$female == 1])


summary(acs2017_ny$AGE[acs2017_ny$female == 0])

#mean for Women
mean(acs2017_ny$AGE[acs2017_ny$female == 1])


#Mean for men
mean(acs2017_ny$AGE[acs2017_ny$female == 0])

#Compare Family size between people with High school vs secondary education

mean(acs2017_ny$AGE[acs2017_ny$female == 0])

summary(acs2017_ny$FAMSIZE[acs2017_ny$educ_college == 0])

summary(acs2017_ny$FAMSIZE[acs2017_ny$educ_college == 1])

#The Family size of people who have a college degree is 2.7 and those without is 3.156 


summary(acs2017_ny$FAMSIZE[acs2017_ny$educ_hs == 0])

summary(acs2017_ny$FAMSIZE[acs2017_ny$educ_hs == 1])

# 3.2 for no Highschool and 2.7 for at least highschool 


summary(acs2017_ny$FAMSIZE[acs2017_ny$educ_college == 1])

summary(acs2017_ny$FAMSIZE[acs2017_ny$educ_advdeg == 1])

#Comparing college grads to advanced degrees

# 4 year degree = 2.714  advanced degree = 2.631


#Importing S&P 500 Data
library(readr)
X_GSPC <- read_csv("^GSPC.csv")
View(X_GSPC)

summary(X_GSPC)

#What is the mean return on the SP500 index 2010 to today? 10 years

mean((X_GSPC$Close - X_GSPC$Open))

# Mean return of the index is 0.4 

#2 What is the mean return on days when the previous day's return was positive? 
library(dplyr)

X_GSPC$Positive=0
X_GSPC$Positive[X_GSPC$Close - X_GSPC$Open> 0] = 1
X_GSPC$Prev[lag(X_GSPC$Positive > 0, k=1)]= 1
attach(X_GSPC)

X_GSPC$Return = 0
X_GSPC$Return[X_GSPC$Close - X_GSPC$Open]

NewX_GSPC <- mutate(X_GSPC, Prev = lead(Positive) )

New2X_GSPC <- mutate(X_GSPC, Prev = lead(Positive, 2) )

summary(NewX_GSPC$Close - NewX_GSPC$Open)

summary(NewX_GSPC$Close[X_GSPC$Prev == 1] - NewX_GSPC$Open[X_GSPC$Prev == 1])

summary(New2X_GSPC$Close[X_GSPC$Prev == 1] - New2X_GSPC$Open[X_GSPC$Prev == 1])

summary(X_GSPC$Close[X_GSPC$Prev == 1] - X_GSPC$Open[X_GSPC$Prev == 1])

View(NewX_GSPC)

#The Means for the returns on days that had positive returns the day before and the 2 days before are both 11.4



#test

summary(NewX_GSPC$Close[X_GSPC$Prev == 1] )

summary(New2X_GSPC$Close[X_GSPC$Prev == 1] )

summary(X_GSPC$Close[X_GSPC$Prev == 1] )

Test <- (X_GSPC[X_GSPC$Prev == 1])







