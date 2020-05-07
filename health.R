## Isaac Yoo

#Import SurveySp13.csv data set
SurveySp13 <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Emory/S2019/QTM 100/Lab/0 Datasets/SurveySp13.csv")

##1
# a)
hist(SurveySp13$GPA) #produce a histogram of SurveySp13$GPA
#I produced a histogram of SurveySp13$GPA. The distribution seems left-skewed.

# b)
GPAclean <- subset(SurveySp13, SurveySp13$GPA<4.0) #create a new data set that excludes GPA 4.0 and over
summary(GPAclean) #displays summary of GPAclean
#Some GPA scores are over 4.0 in the original dataset. 
#The new data set excludes these and contains 207 observations. 



##2
# a)
hist(GPAclean$hrs_extracurricular) #produce a histogram of GPAclean$hrs_extracurricular
#I produced a histogram of GPAclean$hrs_extracurricular. The distribution seems right-skewed.

# b)
plot(GPAclean$GPA, GPAclean$hrs_extracurricular) #shows relationship between GPA and hrs_extracurricular
#I produced a correlation plot. There seems to be a very weak positive correlation. 

# c)
cor(GPAclean$GPA, GPAclean$hrs_extracurricular) #estimates correlation between GPA and hrs_extracurricular
#The correlation between GPA and hrs_extracurricular is 0.1646612.
#The strength of the association is very weak, and the correlation is positive.

cor.test(GPAclean$GPA, GPAclean$hrs_extracurricular) #performs a test to see if the correlation is significantly different from zero
#Confidence interval: (0.02894107, 0.29442008)
#We are 95% confident that the true correlation (r) for GPA vs hrs_extracurricular is within the interval (0.02894107, 0.29442008).
#The test statistic=2.3902, df=205, and the p-value=0.01774.
#At a significance level of 0.05, we reject the null (p=0) because p<0.05; the data suggests that the correlatin between GPA and hrs_extracurricular is significantly different from zero.



##3
# a)
m1 <- lm(GPAclean$GPA ~ GPAclean$hrs_extracurricular) #stores linear model (y=GPA, x=hrs_extracurricular) into m1
summary(m1) #view regression results
#y(hat) = 3.384281 + 0.008729(hrs_extracurricular)

# b)
confint(m1) #get confidence intervals for B0 and B1
#y-intercept: The predicted GPA of a student with 0 hours of extracurricular activity is 3.384281.
#[p-value: <2e-16] Since the p-value is <0.05, the intercept is statistically significantly different from zero.
#Confidence interval: (3.306374243, 3.46218719)


# c)
confint(m1) #get confidence intervals for B0 and B1
#slope: For each additional hour of extracurricuar activity, the student's predicted GPA increases by 0.008729.
#[p-value: 0.0177] Since the p-value is <0.05, the slope is statistically significantly different from zero.
#Confidence interval: (0.001528789, 0.01592954)

# d)
#[Multiple R-squared]: 0.02711
#2.71% of the variability in GPAclean$GPA is explained by GPAclean$hrs_extracurricular.
#The association between hrs_extracurricular and GPA is very weak (r=0.1646612, r^2=0.02711).

# e)
#Residual standard error: 0.3912 (df=205)
#The residual standard error of 0.3912 represents the standard deviation of the residuals, measuring how close the fit is to the points.



##4
# a)
hist(rstandard(m1)) #produce histogram of standardized residuals
qqnorm(rstandard(m1)) #produce qq plot of standardized residuals
qqline(rstandard(m1)) ##add line to qq plot
#The residuals are NOT approximately normally distributed.
#Rather, the histogram of the residuals show a left-skewed distribution. 

# b)
plot(GPAclean$hrs_extracurricular, rstandard(m1), xlab = "hrs_extracurricular", ylab = "Standardized Residuals") #produce plot of x=hrs_extracurricular, y=standardized residuals
abline(h=0, lty=2) #add a line y=0
# i) No, there doesn't seem to be evidence of a non-linear trend. As there is a good scatter about the line y=0, assumptions regarding linear relationship is satisfied.
# ii) No, there doesn't seem to be evidence of a non-constant variance in the residuals. As there is a good scatter about the line y=0, assumptions regarding constant variance is satisfied.



##5
# a)
GPAclean[1,] #view summary of student 1
#GPA: 3.3, hrs_extracurricular: 18

# b)
#Model 1 predicted student 1's GPA to be 3.541403.

# c)
resid(m1)[1] #regular residuals for the first observation based on model 1 (m1)
#Regular residual: -0.2414057 
rstandard(m1)[1] #standardized residuals for the first observation based on model 1 (m1)
#Standardized residual: -0.6213396 
#Regular residuals: y-y(hat) --> true value - predicted value --> shows difference between true and predicted value
#Standardized residuals are regular residuals divided by the true standard deviation of the residuals.

# d)
#Student 1 had a lower GPA than predicted by Model 1.



##6
# a)
#days_exercise: predicted positive association with GPA
#sleep: predicted positive association with GPA

# b)
#days_exercise
exerciseLM <- lm(GPAclean$GPA ~ GPAclean$days_exercise) #stores linear model (y=GPA, x=days_exercise) into exerciseLM
summary(exerciseLM) #view regression results
confint(exerciseLM) #get confidence intervals for B0 and B1
#y(hat) = 3.41795 + 0.01023(days_exercise)
#slope: For each additional day of exercise in a week, the student's predicted GPA increases by 0.01023.
#[p-value: 0.463] Since the p-value is >0.05, the slope is not statistically significantly different from zero.
#Confidence interval: (-0.01720849, 0.03767793)
#Conclusion: days_exercise does not seem to be a significant predictor of GPA.

#sleep
sleepLM <- lm(GPAclean$GPA ~ GPAclean$sleep) #stores linear model (y=GPA, x=sleep) into sleepLM
summary(sleepLM) #view regression results
confint(sleepLM) #get confidence intervals for B0 and B1
#y(hat) = 3.17164 + 0.04000(sleep)
#slope: For each additional hour of sleep, the student's predicted GPA increases by 0.04000.
#[p-value: 0.116] Since the p-value is >0.05, the slope is not statistically significantly different from zero.
#Confidence interval: (-0.009986123, 0.08998708)
#Conclusion: sleep does not seem to be a significant predictor of GPA.

# c)
#Yes, the parameter estimates go in the direction that I anticipated, as exerciseLM had a slope of 0.01023, and sleepLM had a slope of 0.04000.
#As both slopes are positive, both variables have a positive association with GPA.



##Optional
pairs(~GPA + Emory_FirstChoice + hrs_extracurricular, data = GPAclean) #visually examine relationship between GPA, Emory_FirstChoice, and hrs_extracurricular
m2 <- lm(GPA ~ Emory_FirstChoice + hrs_extracurricular, data = GPAclean) #store linear model into m2; predicts GPA by Emory_FirstChoice and hrs_extracurricular
summary(m2) #view regression results
confint(m2) #get confidence intervals for B0 and B1

# 1) #y(hat) = 3.473549 - 0.172771(Emory_FirstChoiceYes) + 0.007883(hrs_extracurricular)

# 2) y-intercept
#The y-intercept is 3.473549 (when all other variables are 0).
#[p-value: <2e-16] Since the p-value is <0.05, the intercept is statistically significantly different from zero.
#Confidence interval: (3.3799726512, 3.56712595)

# 3) hrs_extracurricular
#slope: For each additional hour of extracurricuar activity, the student's predicted GPA increases by 0.007883.
#[p-value: 0.02878] Since the p-value is <0.05, the slope is statistically significantly different from zero.
#Confidence interval: (0.0008250716, 0.01494191)

# 4) Emory_FirstChoiceYes
#slope: Students who answered Yes to Emory_FirstChoice has a GPA, on average, of 0.172771 points less than students who answered No to Emory_FirstChoice, holding everything else in the model constant.
#[p-value: 0.00141] Since the p-value is <0.05, the slope is statistically significantly different from zero.
#Confidence interval: (-0.2779834674, -0.06755951)

# 5) Model 2 vs Model 1
resid(m2)[1] #regular residuals for the first observation based on model 2 (m2)
rstandard(m2)[1] #standardized residuals for the first observation based on model 2 (m2)
#Regular residual: -0.3154521 // Standardized residual: -0.8319994 
#There seems to be no improvement in the predictive ability of Model 2 compared to Model 1.
#Model 1 yielded a regular and standardized residual of -0.2414057 and -0.6213396, respectively, for student 1.
#Model 2 yielded a regular and standardized residual of -0.3154521 and -0.8319994, respectively, for student 1.
#Model 2 yields higher regular and standardized residuals than Model 1, showing that Model 2 has worse predictive ability than Model 1.
