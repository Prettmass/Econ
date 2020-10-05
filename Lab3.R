load("acs2017_ny_data.RData")

dat_NYC <- subset(acs2017_ny, (acs2017_ny$in_NYC == 1)&(acs2017_ny$AGE > 20) & (acs2017_ny$AGE < 66))
attach(dat_NYC)
borough_f <- factor((in_Bronx + 2*in_Manhattan + 3*in_StatenI + 4*in_Brooklyn + 5*in_Queens), levels=c(1,2,3,4,5),labels = c("Bronx","Manhattan","Staten Island","Brooklyn","Queens"))


norm_varb <- function(X_in) {
  (max(X_in, na.rm = TRUE) - X_in)/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE) )
}

#Next, fix up the data,

is.na(OWNCOST) <- which(OWNCOST == 9999999)
housing_cost <- COSTGAS
norm_inc_tot <- norm_varb(FTOTINC)
norm_housing_cost <- norm_varb(housing_cost)

#Here we create the dataframe to use

data_use_prelim <- data.frame(norm_inc_tot,norm_housing_cost)
good_obs_data_use <- complete.cases(data_use_prelim,borough_f)
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(borough_f,good_obs_data_use)

#Next split the data into 2 parts: one part to train
#the algo, then the other part to test how well it works for new data. Here we use an 80/20 split.

set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]

#Finally run the k-nn algo and compare against the simple means,

summary(cl_data)
prop.table(summary(cl_data))
summary(train_data)
require(class)
for (indx in seq(1, 9, by= 2)) {
  pred_borough <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
  num_correct_labels <- sum(pred_borough == true_data)
  correct_rate <- num_correct_labels/length(true_data)
  print(c(indx,correct_rate))
}

# The key to getting high accuracy in the K-nn algorithm is to pick a variable that has different
# means in all 5 burroughs. here are my means tests to see which one I would end up using.



mean(acs2017_ny$Commute_car[acs2017_ny$in_Bronx == 1])

mean(acs2017_ny$Commute_car[acs2017_ny$in_Manhattan == 1])

mean(acs2017_ny$Commute_car[acs2017_ny$in_StatenI == 1])

mean(acs2017_ny$Commute_car[acs2017_ny$in_Queens == 1])

mean(acs2017_ny$Commute_car[acs2017_ny$in_Brooklyn == 1])


mean(acs2017_ny$COSTGAS[acs2017_ny$in_Bronx == 1])

mean(acs2017_ny$COSTGAS[acs2017_ny$in_Manhattan == 1])

mean(acs2017_ny$COSTGAS[acs2017_ny$in_StatenI == 1])

mean(acs2017_ny$COSTGAS[acs2017_ny$in_Queens == 1])

mean(acs2017_ny$COSTGAS[acs2017_ny$in_Brooklyn == 1])



#after looking at COSTGAS I noticed varying means between the different borough. This would make sense because there are different levels of gas infrastructure throughout the 5 boroughs. if there aren't gas lines in an area the amount of gass being bought is 0.

# The means of the 5 boroughs with respect o gas cost are as follows.
# bronx and manhattan are high and about the same, but the other 3 have varying means.
# staten island being the lowest and quenns being the highest of the 3.

# with this wide range of means, the K-nn algorithm gives an increase in accuracy compared to 
# the Rent + Owncost variables.

# most varibles will also giving higher accuracy compared to a compind variable because,
# when 2 variables are combine and then split back into 5 groups the means between those 5 groups
# will be closer together due to the peaks and troughs of the 2 variables not always lining up.


# in conclusion the the best way to get high accuracy for algorithm which predicts the 5 boroughs
# is to let it use a variable that differs greatly between the boroughs.






