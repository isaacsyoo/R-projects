## Isaac Yoo

##1 
#Import lead.csv data set
lead <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Emory/S2019/QTM 100/Lab/Datasets/lead.csv")

# a)
#These measurements are paired, as one data is correlated to the other (one is the blood lead level in 1972, and the other is the same subject's blood lead level a year later).

# b)
lead$diffLd <- lead$Ld73 - lead$Ld72 #create new variable that is the difference of lead$Ld73 and lead$Ld72
install.packages("Rmisc") #installs Rmisc package that contains summarySE() function
library(Rmisc) #calls Rmisc library
summarySE(data = lead, measurevar = "diffLd", na.rm=T) #shows summary statistics of lead$diffLd
hist(lead$diffLd) #displays histogram of lead$diffLd

#There are 121 observations for which the difference is not missing. 
#Mean of difference: -3.371901
#Standard deviation of difference: 9.853199
#Shape of distribution of differences: Approximately normal

# c)
#On average, lead blood levels decrease from 1972 to 1973, indicating less lead exposure over time.


##2
# a)
#Appropriate test: Paired t-test
t.test(lead$Ld73, lead$Ld72, paired=TRUE) #paired t-test of lead$Ld73 and lead$Ld72

# b)
#Parameter of interest: Difference in blood lead levels between 1972 and 1973 among all children living in that area

# c)
#Null: H0: mu(d) = 0
#Alternative: Ha: mu(d) != 0

# d)
#Randomization: The samples were obtained through randomization.
#Independence: The two measurements (Ld72 & Ld73) are dependent, while the observations are independent.
#Normality: The sampling distribution of the sample mean difference is approximately normal.

# e)
#Test statistic: -3.7644

# f)
#p-value: 0.0002599

# g)
#At alpha = 0.05, I reject the null.

# h)
#95% confidence interval: (-5.145414, -1.598387)

# i)
#A paired t-test was conducted to test if there is a difference in blood lead levels between 1972 and 1973. The parameter of interest is the difference in blood lead levels between 1972 and 1973 among all children living in that area.
#The null hypothesis is that the mean of the differences (mu(d)) is = to 0.
#The alternative hypothesis is that the mean of the differences (mu(d)) is != to 0.
#All assumptions regarding sampling distributions were satisfied for valid inference, as the samples were obtained through randomization, The two measurements (Ld72 & Ld73) are dependent, while the observations are independent, and the sampling distribution of the sample mean difference is approximately normal.
#The test statistic is -3.7644, yielding a p-value of 0.0002599, meaning the null hypothesis is rejected at an alpha level of 0.05.
#The 95% confidence interval is (-5.145414, -1.598387).


##3
t.test(lead$diffLd ~ lead$Group, var.equal=TRUE) #two sample t-test on the difference in blood lead levels (1972 & 1973) by Group, equal variances
#There is evidence that the change in lead blood level exposure depends on which group the child is in.
#The two sample t-test gives a test statistic of 4.9803 and p-value of 2.172e-06.
#At alpha = 0.05, the null hypothesis is rejected.


##4
# a)
par(mfrow=c(1,2), pty="s") #change graphical parameter settings to present two plots in one window
hist(lead$Iqf[lead$Group == "control"], main = "Control", xlab = "full scale IQ") #creates histogram of Iqf among control group
hist(lead$Iqf[lead$Group == "lead"], main = "Lead", xlab = "full scale IQ") #creates histogram of Iqf among lead group
dev.off() #reset grpahical parameter settings
#2 side-by-side histograms were created to show the relationship between Group and Iqf. The control group seems to display a normal distribution, while the lead group does not, for Iqf.

# b)
summarySE(data = lead, measurevar = "Iqf", groupvars = "Group", na.rm=T) #displays summary statistics of lead$Iqf by Group
#Control Group: [# of children: 78], [Mean of Iqf: 92.88462], [Standard deviation of Iqf: 15.34451]
#Lead Group: [# of children: 46], [Mean of Iqf: 88.02174], [Standard deviation of Iqf: 12.20654]

# c)
#Distribution of Iqf in control group: The distribution seems to be approximately normally distributed.
#Distribution of Iqf in lead group: The distribution seems to be left skewed, with most data on the middle to right side of the graph.


##5
# a)
#Appropriate test: Two sample t-test
t.test(lead$Iqf ~ lead$Group, var.equal = FALSE) #two sample t-test on Iqf by Group, unequal variances

# b)
#Parameter of interest: Difference in Iqf between Control and Lead group

# c)
#Null: H0: mu(control) = mu(lead)
#Alternative: Ha: mu(control) != mu(lead)

# d)
#Randomization: The samples were obtained through randomization.
#Independence: The two groups (Iqf & Group) are independent, and the observations are independent.
#Normality: The sampling distribution of the sample mean difference is approximately normal.

# e)
#Test statistic: 1.9439

# f)
#p-value: 0.05442

# g)
#At alpha = 0.05, I fail to reject the null.

# h)
#95% confidence interval: (-0.09391511, 9.81966762)

# i)
#A two sample t-test was conducted to test if there is a difference in Iqf between Control and Lead group. The parameter of interest is the difference in Iqf between Control and Lead group.
#The null hypothesis is mu(control) = mu(lead) for Iqf.
#The alternative hypothesis is mu(control) != mu(lead) for Iqf.
#All assumptions regarding sampling distributions were satisfied for valid inference, as the samples were obtained through randomization, The two groups (Iqf & Group) are independent, and the observations are independent, and the sampling distribution of the sample mean difference is approximately normal.
#The test statistic is 1.9439, yielding a p-value of 0.05442, meaning that we fail to reject null at an alpha level of 0.05.
#The 95% confidence interval is (-0.09391511, 9.81966762)


##6
# a)
#Appropriate test: One-way ANOVA test

# b)
#Turn Area into a factor with appropriate levels
lead$AreaFactor <- factor(NA, levels=c("0-1 miles from smelter", "1-2.5 miles", "2.5-4 miles")) #create new variable for Area 
#assign values of Area
lead$AreaFactor[lead$Area == 1] <- "0-1 miles from smelter" #assign all values of "1"
lead$AreaFactor[lead$Area == 2] <- "1-2.5 miles" #assign all values of "2"
lead$AreaFactor[lead$Area == 3] <- "2.5-4 miles" #assign all values of "3"

#Generate appropriate figure
par(mfrow=c(1,3), pty="s") #change graphical parameter settings to present three plots in one window
hist(lead$Iqp_pc[lead$AreaFactor == "0-1 miles from smelter"], main = "0-1 miles from smelter", xlab = "Iqp_pc") #creates histogram of Iqp_pc among "0-1 miles from smelter" group
hist(lead$Iqp_pc[lead$AreaFactor == "1-2.5 miles"], main = "1-2.5 miles", xlab = "Iqp_pc") #creates histogram of Iqp_pc among "1-2.5 miles" group
hist(lead$Iqp_pc[lead$AreaFactor == "2.5-4 miles"], main = "2.5-4 miles", xlab = "Iqp_pc") #creates histogram of Iqp_pc among "2.5-4 miles" group
dev.off() #reset grpahical parameter settings
#3 side-by-side histograms were created to show the relationship between AreaFactor and Iqp_pc. 

# c)
anova.AreaFactor <- aov(lead$Iqp_pc ~ lead$AreaFactor) #ANOVA test of Iqp_pc & AreaFactor
summary(anova.AreaFactor) #summary of test
#Test-statistic: 3.419
#p-value: 0.0359

# d)
#At alpha = 0.05, I reject the null.

# e)
#Pairwise test - Tukey Method

# f)
TukeyHSD(anova.AreaFactor) #Pairwise test - Tukey method
plot(TukeyHSD(anova.AreaFactor)) #plot confidence ineravls from pairwise comparisons
#1-2.5 miles - 0-1 miles: [Mean difference: 0.4374269], [p-value: 0.7082947], [Upper bound: 1.747027], [Lower bound: -0.8721736]
#2.5-4 miles - 0-1 miles: [Mean difference: 1.8141946], [p-value: 0.0272280], [Upper bound: 3.462536], [Lower bound: 0.1658531]
#2.5-4 miles - 1-2.5 miles: [Mean difference: 1.3767677], [p-value: 0.1395997], [Upper bound: 3.085217], [Lower bound: -0.3316821]


