
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


#Next is to find the the mean of of the returns on days after positive days
# First i created a dummy variable called positive  and made it = 1 when the current day had
# a positie return.
X_GSPC$Positive=0
X_GSPC$Positive[X_GSPC$Close - X_GSPC$Open> 0] = 1
X_GSPC$Prev[lag(X_GSPC$Positive > 0, k=1)]= 1

#Then i made 2 other data frame one with a dummy variable (Prev) that is just the positive variable 
#but pushed up one day so that so that i can take only the informattion from days that were after 
#positive days.

NewX_GSPC <- mutate(X_GSPC, Prev = lag(Positive) )

# the next data frame is the same as the last one but the Prev dummy variable is pushed up 2 days 
New2X_GSPC <- mutate(X_GSPC, Return = lag(Positive, 2) )

summary(NewX_GSPC$Close - NewX_GSPC$Open)

summary(NewX_GSPC$Close[X_GSPC$Prev == 1] - NewX_GSPC$Open[X_GSPC$Prev == 1])

#not sure how to do this efficently so I made a lot of dummy variables with a 3rd new data frame...

New3X_GSPC <- mutate(New2X_GSPC, Prev2 = New2X_GSPC$Prev + New2X_GSPC$Return)

#I check to see if both the day before (Prev) and the day before thaat (Return) are both 1
#then if so took the return of this 3rd day and found the mean.

summary(New3X_GSPC$Close[New3X_GSPC$Prev2 == 2] - New3X_GSPC$Open[New3X_GSPC$Prev2 == 2])

#The Means end up being 
#the mean on every day is 0.399
#The day after a positive day is -0.163
#the day after 2 positive days is 0.3585

#Now was does this say about this gives insight into investing because the day after a positive should be
#negative and then day after 2 postive days is going to be positive is not the right take away.

#the hot hands fallacy is a bias that a person who experiences a succesful outcome has a greater
#chance of success in the future.

#This is proven wrong by the results I found because the average return on days after a successful 
#trading day on the SP500 was negative 





