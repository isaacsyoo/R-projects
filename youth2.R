## Isaac Yoo

##1 
#Import yrbss2013.csv data set
yrbss2013 <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Emory/S2019/QTM 100/Lab/Datasets/yrbss2013.csv")

# a)
hist(yrbss2013$days_drink) #creates a histogram of yrbss2013$days_drink
#The histogram of yrbss2013$days_drink seems to show a strong right skew, with most of the data gathered on the left side of the graph.

# b & c)
install.packages("Rmisc") #installs Rmisc package that contains summarySE() function
library(Rmisc) #calls Rmisc library
summarySE(data = yrbss2013, measurevar = "days_drink") #shows summary statistics of yrbss2013$days_drink
#The true population mean is 1.45402 days. 
#The true population standard deviation is 3.781289 days. 


##2
#Random sample: The samples taken from the population are selected at random; thus, this condition is satisfied.
#Independence: The sample size of 15 is less than 10% of the population; thus, this condition is satisfied.
#Normal distribution: The histogram is skewed to the right.


##3
# a)
#take samples from yrbss2013$days_drink of size 40, alpha 0.05, and num.reps 50
numrep50 <- inference.means(variable=yrbss2013$days_drink, sample.size=40, alpha=0.05, num.reps=50)
plot.ci(results=numrep50,true.val=1.45402) #display the confidence intervals

# b)
#take samples from yrbss2013$days_drink of size 40, alpha 0.05, and num.reps 100
numrep100 <- inference.means(variable=yrbss2013$days_drink, sample.size=40, alpha=0.05, num.reps=100)
plot.ci(results=numrep100,true.val=1.45402) #display the confidence intervals

# c)
#take samples from yrbss2013$days_drink of size 40, alpha 0.05, and num.reps 1000
numrep1000 <- inference.means(variable=yrbss2013$days_drink, sample.size=40, alpha=0.05, num.reps=1000)
plot.ci(results=numrep1000,true.val=1.45402) #display the confidence intervals

# d)
#take samples from yrbss2013$days_drink of size 40, alpha 0.05, and num.reps 10000
numrep10000 <- inference.means(variable=yrbss2013$days_drink, sample.size=40, alpha=0.05, num.reps=10000)
plot.ci(results=numrep10000,true.val=1.45402) #display the confidence intervals

#As the number of repetitions increase, the width of the confidence intervals increases.


##4
# a)
#take samples from yrbss2013$days_drink of size 20, alpha 0.05, and num.reps 1000
size20 <- inference.means(variable=yrbss2013$days_drink, sample.size=20, alpha=0.05, num.reps=1000)
plot.ci(results=size20,true.val=1.45402) #display the confidence intervals

# b)
#take samples from yrbss2013$days_drink of size 50, alpha 0.05, and num.reps 1000
size50 <- inference.means(variable=yrbss2013$days_drink, sample.size=50, alpha=0.05, num.reps=1000)
plot.ci(results=size50,true.val=1.45402) #display the confidence intervals

# c)
#take samples from yrbss2013$days_drink of size 100, alpha 0.05, and num.reps 1000
size100 <- inference.means(variable=yrbss2013$days_drink, sample.size=100, alpha=0.05, num.reps=1000)
plot.ci(results=size100,true.val=1.45402) #display the confidence intervals

# d)
#take samples from yrbss2013$days_drink of size 250, alpha 0.05, and num.reps 1000
size250 <- inference.means(variable=yrbss2013$days_drink, sample.size=250, alpha=0.05, num.reps=1000)
plot.ci(results=size250,true.val=1.45402) #display the confidence intervals

#As the sample sizes increase, the width of the confidence intervals decreases.


##5
#table(variableName$decision) displays frequency table showing disribution of decision

#number of reps
table(numrep50$decision) #11/50 reject H0 = 0.22
table(numrep100$decision) #12/88 reject H0 = 0.12
table(numrep1000$decision) #154/1000 reject H0 = 0.154
table(numrep10000$decision) #1511/10000 reject H0 = 0.1511
#Increasing the number of reps decreases the targeted capture rate (alpha). 

#sample size
table(size20$decision) #175/1000 reject H0 = 0.175
table(size50$decision) #134/1000 reject H0 = 0.134
table(size100$decision) #101/1000 reject H0 = 0.101
table(size250$decision) #58/1000 reject H0 = 0.058
#Increasing the sample size decreases the targeted capture rate (alpha). 


##6
#In all of our hypothesis tests, we run the risk of committing a Type I error.


##7
#take samples from yrbss2013$days_drink of size 300, alpha 0.05, and num.reps 1000
question7 <- inference.means(variable=yrbss2013$days_drink, sample.size=300, alpha=0.05, num.reps=1000)
hist(question7$samp.est,main="Sample Means") #creates a histogram of the sample means
#The histogram shows an approximately normally distributed graph for the sample means.
hist(question7$test.stat,main="t test statistics") #creates a histogram of the test statistics
#The histogram shows an approximately normally distributed graph for the test statistics.
hist(question7$p.val,main="p-values") #creates a histogram of the p-values
#The histogram shows an approximately uniformly distributed graph for the p-values.


##8
plot.ci(results=question7, true.val=1.45402) #displays the confidence intervals
table(question7$capture) #displays a frequency table showing the distribution of whether or not the true parameter value was captured
#933/1000 (0.933) samples capture the true parameter value.
#The confidence intervals seem to range within varying lengths, with many of them containing the true parameter value of 1.454. 
#The confidence intervals seem to provide reasonable bounds for the mean, as they are mostly centered about the true paramter value.


##9
#take samples from yrbss2013$height_m of size 300, alpha 0.05, and num.reps 1000
question9 <- inference.means(variable=yrbss2013$height_m, sample.size=300, alpha=0.05, num.reps=1000)
question9a <- inference.means(variable=yrbss2013$height_m, sample.size=500, alpha=0.05, num.reps=1000)
table(question9a$capture)
#Yes, increasing the sample size "fixes" our results -- increasing the sample size lowers the Type I error rate and improves the percent of confidence intervals that capture the true parameter value.
#This may be due to a larger sample size providing a more accurate representation of the population, allowing us to be more certain of the results of our hypothesis test -- reducing the rate of a Type I error.
