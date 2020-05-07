## Isaac Yoo

#Import pharynx.csv data set
pharynx <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Emory/S2019/QTM 100/Lab/0 Datasets/pharynx.csv")

##1
#create new variable that indicates whether or not the individual survived 500 days post diagnosis
pharynx$survive500 <- factor(NA, levels=c("yes", "no")) #new variable survive500
pharynx$survive500[pharynx$TIME>=500] <- "yes" #assign values for "yes"
pharynx$survive500[pharynx$TIME<500] <- "no" #assign values for "no"

prop.table(table(pharynx$survive500)) #displays percentage of "yes" and "no"
#45.12821% of patients overall survived at least 500 days in this study.



##2
# a) One sample z test
# b) The parameter of interest is the percentage of survival 500 days post diagnoses of the true population of patients.
# c) [Null] H0: p = 0.5; [Alternative] Ha: p != 0.5
# d) Randomization: The data was obtained through randomization.
#    Independence: The observations are independent (sample is less than 10x the parameter).
#    Normality: Although the histogram of pharynx$TIME is right-skewed, np>10 and n(1-p)>10 is met; thus, normality is assumed.
#    Conditions reagrding sampling distributions are satisified for valid inference.

# Run one sample z test 
prop.test(table(pharynx$survive500), p=0.5, correct=F)

# e) Test statistic: [Chi-squared: 1.8513]; [Z-statistic: 1.36062485645]
# f) p-value: 0.1736
# g) At the a = 0.05 level of significance, I fail to reject H0.
# h) Confidence interval: (0.3830507, 0.5213957)

#Test conclusion
# i) Through the one sample z test, we may conclude that the survival to 500 days does not differ significantly from that of the Europeans. 
#    The test yielded a test statistic of [Chi-squared: 1.8513]; [Z-statistic: 1.36062485645], and a p-value of 0.1736.
#    At the 0.05 level of significance, we fail to reject the null hypothesis.
#    We can be 95% confident that the true percent of Americans that survive at least 500 days post diagnosis is in the interval (0.3830507, 0.5213957). 



##3
# a) The two tests that could be used are the "Two sample z test" and the "Chi-squared test".
#    Two sample z test: [Null] H0: proportion of patients who survived to 500 days on treatment 1 = proportion of patients who survived to 500 days on treatment 2; 
#                       [Alternative] Ha: proportion of patients who survived to 500 days on treatment 1 != proportion of patients who survived to 500 days on treatment 2; 
#                       [Appropriate]: Appropriate for comparing proportions (if there is a significant difference between the proportions)
#    Chi-squared test:  [Nul]: H0: survival and treatment type are independent (not associated);
#                       [Alternative]: Ha: survival and treatment type are dependent (associated)
#                       [Appropriate]: Appropriate for comparing categorical variables (if there is an association between the variables)

# b) Run Chi-squared test
Survival_Treatment_Chi <- table(pharynx$TX, pharynx$survive500) #create new variable w/ table
Survival_Treatment_Chi #displays table
addmargins(Survival_Treatment_Chi) #creates margin that displays sum
prop.table(Survival_Treatment_Chi, margin=1) #displays row proportions

chisq.test(Survival_Treatment_Chi, correct=F) #chi-squared test

#Test conclusion
#   Through the chi-squared test, we may conclude that there is no association between survival past 500 days and treatment group.
#    The test yielded a test statistic of [Chi-squared: 1.9674]; [Z-statistic: 1.40264036731], and a p-value of 0.1607.
#    At the 0.05 level of significance, we fail to reject the null hypothesis.

# c) The analysis performed is not sufficient to make definitive conclusions regarding the efficacy of the two treatment groups, as p-values tell us the strength of evidence of an association between two variables, NOT the strength of the association.



##4
# a) An assumption of the chi-squared test is that all expected cell counts are at least 5.

# b) 
Survival_Treatment_Exp <- chisq.test(Survival_Treatment_Chi, correct=F) #save chi-squared test results as an object
Survival_Treatment_Exp$expected #display table with expected cell counts
#Yes, the assumption regarding expected cell counts are satisfied, as all expected cell counts are at least 5.

# c)
Survival_Stage_Exp <- chisq.test(pharynx$T_STAGE, pharynx$survive500, correct=F) #save chi-squared test results as an object
Survival_Stage_Exp$expected #display table with expected cell counts
#No, the assumption regarding expected cell counts are not satisfied, as all expected cell counts are not at least 5.
#An appropriate alternative test would be Fisher's exact test.



##5
#a) 
1-pchisq(1, df=1)
#[p-value]: 0.3173105; [Significant?]: No

#b) 
1-pchisq(3, df=1)
#[p-value]: 0.08326452; [Significant?]: No

#c) 
1-pchisq(5, df=1)
#[p-value]: 0.02534732; [Significant?]: Yes

#d) 
1-pchisq(1, df=2)
#[p-value]: 0.6065307; [Significant?]: No

#e) 
1-pchisq(3, df=2)
#[p-value]: 0.2231302; [Significant?]: No

#f) 
1-pchisq(5, df=2)
#[p-value]: 0.082085; [Significant?]: No

#As the test statistic increases, the p-value decreases. Therefore, larger test statistics present more evidence against the null hypothesis.
#The same test statistic value with different degrees of freedom can result in a different conclusion for a specified level of significance.

