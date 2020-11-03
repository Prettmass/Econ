```{r}
attach(acs2017_ny)
use_varb <- (AGE >= 25) & (AGE <= 55) & (LABFORCE == 2) & (WKSWORK2 > 4) & (UHRSWORK >= 35) & (Hispanic == 1) & (female == 1) & ((educ_college == 1) | (educ_advdeg == 1))
dat_use <- subset(acs2017_ny,use_varb) # 
detach()
```



```{r}
LinearAge = lm(acs2017_ny$INCWAGE ~ acs2017_ny$AGE + I(acs2017_ny$AGE^2) + I(acs2017_ny$AGE^3) + I(acs2017_ny$AGE^4))
summary(LinearAge)

plot(LinearAge)

LinearAge2 = lm(acs2017_ny$INCWAGE ~ I(acs2017_ny$AGE^3) + I(acs2017_ny$AGE^4))
summary(LinearAge2)

LinearLog = lm(acs2017_ny$INCWAGE ~ acs2017_ny$AGE + log(acs2017_ny$AGE))
summary(LinearLog)
```

What is the peak of predicted wage?
- based off teh max, 641975 is the max wage

What if you add higher order polynomials of age?
- the max wage increases to 686692

Do a hypothesis test of whether all of those higher-order polynomial terms are jointly significant. Describe the pattern of predicted wage as a function of age. What if you used (log(Age))? (And why would polynomials in (log(Age)) be useless? Experiment.)
- The higher polynomials are equally as significate.

Recall about how dummy variables work. If you added educ_hs in a regression using the subset given above, what would that do? (Experiment, if you aren’t sure.) What is interpretation of coefficient on educ_college in that subset? What would happen if you put both educ_college and educ_advdeg into a regression? Are your other dummy variables in the regression working sensibly with your selection criteria?
- All the dummy variables who present a positive effect on income in comparison to the base of No Highschool. since education level is directly
relative to wage level.

Why don’t we use polynomial terms of dummy variables? Experiment.
- you can't use polynomial on dummy variables because dummy variables are only 0 and 1. So, you can't add higher significance to high levels of
some college for example because there is no "higher" levels of that perticualar dummy since it is either a yes or a no.

What is difference in regression from using log wage as the dependent variable? Compare the pattern of predicted values from the two models (remember to take exp() of the predicted value, where the dependent is log wage). Discuss.
- Log wage will "push down" the higher wage levels in the data set. this gives low weight to the outlyers in the data set.



```{r}
LinearGender = lm(acs2017_ny$INCWAGE ~ acs2017_ny$AGE + I(acs2017_ny$AGE^2) + acs2017_ny$female)
summary(LinearGender)
plot(LinearGender)
length(acs2017_ny$AGE)
length(LinearGender$fitted.values)
length(acs2017_ny$female)
```


What are the other variables you are using in your regression? Do they have the expected signs and patterns of significance? Explain if there is a plausible causal link from X variables to Y and not the reverse. Explain your results, giving details about the estimation, some predicted values, and providing any relevant graphics. Impress.
- Age and gender can't have a reverse relationship with wage. Y effecting X in this case since wage can't effect your age and gender. 

