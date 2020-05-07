## Isaac Yoo

##1 
#Import yrbss2013.csv data set
yrbss2013 <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Emory/S2019/QTM 100/Lab/Datasets/yrbss2013.csv")

# a)
#creates new variable height_cm that contains height in centimeters
yrbss2013$height_cm <- (yrbss2013$height_m * 100)

# b) 
install.packages("Rmisc") #installs Rmisc package that contains summarySE() function
library(Rmisc) #calls Rmisc library
summarySE(data = yrbss2013, measurevar = "height_cm") #shows summary statistics of yrbss2013$height_cm
#The mean is 168.729 cm. The standard deviation is 10.29791 cm. 
hist(yrbss2013$height_cm) #creates a histogram of yrbss2013$height_cm
#The histogram of yrbss2013$height_cm shows a roughly normal distribution, with a SLIGHT possible right skew.


##2
# a)
install.packages("Rmisc") #installs Rmisc package that contains summarySE() function
library(Rmisc) #calls Rmisc library
summarySE(data = yrbss2013, measurevar = "height_cm", groupvars = "sad") #shows summary statistics of yrbss2013$height_cm (separated by variable "sad")
#NO: The mean is 169.6967 cm. The standard deviation is 10.17103 cm. 
#YES: The mean is 166.4107 cm. The standard deviation is 10.23188 cm. 

# b)
boxplot(yrbss2013$height_cm~yrbss2013$sad) #creates a side-by-side boxplot of yrbss2013$height_cm, broken down by the "sad" variable
#The histogram of yrbss2013$height_cm~yrbss2013$sad shows a normal distribution for both groups in "sad". The "no" group of "sad" shows potentially more outliers than the "yes" group.


##3
yrbss2013subset <- subset(yrbss2013, sad == "yes") #creates a subset of people that were sad

# a)
sample_means100 <- rep(NA, 100) #creates a vector filled with 100 NA's, which will be filled later with the sample mean in each sample
for (i in 1:100) { #indication of for loop that will run 100 times
  samp <- sample(yrbss2013subset$height_cm, 10) #draws a sample of size 10 using the sample function, and stores in variable "samp"
  sample_means100[i] <- mean(samp) #stores the mean of "samp" in the i-th position of this vector
} #indicates end of code within this loop
mean(sample_means100) #calculates mean of sample_means100
#The mean is 166.35 cm.
sd(sample_means100) #calculates standard deviation of sample_means100
#The standard deviation is 3.305658 cm.
hist(sample_means100) #produces a histogram of sample_means100
#The histogram appears roughly normal, with a possible right skew.

# b)
sample_means500 <- rep(NA, 500) #creates a vector filled with 500 NA's, which will be filled later with the sample mean in each sample
for (i in 1:500) { #indication of for loop that will run 500 times
  samp <- sample(yrbss2013subset$height_cm, 10) #draws a sample of size 10 using the sample function, and stores in variable "samp"
  sample_means500[i] <- mean(samp) #stores the mean of "samp" in the i-th position of this vector
} #indicates end of code within this loop
mean(sample_means500) #calculates mean of sample_means500
#The mean is 166.5622 cm.
sd(sample_means500) #calculates standard deviation of sample_means500
#The standard deviation is 3.237145 cm.
hist(sample_means500) #produces a histogram of sample_means500
#The histogram appears roughly normal.

# c)
sample_means1000 <- rep(NA, 1000) #creates a vector filled with 1000 NA's, which will be filled later with the sample mean in each sample
for (i in 1:1000) { #indication of for loop that will run 1000 times
  samp <- sample(yrbss2013subset$height_cm, 10) #draws a sample of size 10 using the sample function, and stores in variable "samp"
  sample_means1000[i] <- mean(samp) #stores the mean of "samp" in the i-th position of this vector
} #indicates end of code within this loop
mean(sample_means1000) #calculates mean of sample_means1000
#The mean is 166.4709 cm.
sd(sample_means1000) #calculates standard deviation of sample_means1000
#The standard deviation is 3.274447 cm.
hist(sample_means1000) #produces a histogram of sample_means1000
#The histogram appears roughly normal.

# d)
sample_means5000 <- rep(NA, 5000) #creates a vector filled with 5000 NA's, which will be filled later with the sample mean in each sample
for (i in 1:5000) { #indication of for loop that will run 5000 times
  samp <- sample(yrbss2013subset$height_cm, 10) #draws a sample of size 10 using the sample function, and stores in variable "samp"
  sample_means5000[i] <- mean(samp) #stores the mean of "samp" in the i-th position of this vector
} #indicates end of code within this loop
mean(sample_means5000) #calculates mean of sample_means5000
#The mean is 166.4191 cm.
sd(sample_means5000) #calculates standard deviation of sample_means5000
#The standard deviation is 3.182728 cm.
hist(sample_means5000) #produces a histogram of sample_means5000
#The histogram appears roughly normal.

# e) Increasing the number of samples while keeping the size constant seems to yield similar mean and standard deviation calculations.


##4
yrbss2013subset <- subset(yrbss2013, sad == "yes") #creates a subset of height in cm of people that were sad

# a)
sample_means100 <- rep(NA, 5000) #creates a vector filled with 5000 NA's, which will be filled later with the sample mean in each sample (of size 100)
mean(sample_means100) #calculates mean of sample_means100
#The mean is 166.4161 cm.
sd(sample_means100) #calculates standard deviation of sample_means100
#The standard deviation is 1.023238 cm.

# b)
sample_means200 <- rep(NA, 5000) #creates a vector filled with 5000 NA's, which will be filled later with the sample mean in each sample (of size 200)
mean(sample_means200) #calculates mean of sample_means200
#The mean is 166.4037 cm.
sd(sample_means200) #calculates standard deviation of sample_means200
#The standard deviation is 0.6796421 cm.

# c)
sample_means500 <- rep(NA, 5000) #creates a vector filled with 5000 NA's, which will be filled later with the sample mean in each sample (of size 500)
mean(sample_means500) #calculates mean of sample_means500
#The mean is 166.4098 cm.
sd(sample_means500) #calculates standard deviation of sample_means500
#The standard deviation is 0.4119179 cm.

# FOR LOOP
for (i in 1:5000) { #indication of for loop that will run 5000 times
  samp <- sample(yrbss2013subset$height_cm, 100) #draws a sample of size 100 using the sample function, and stores in variable "samp"
  sample_means100[i] <- mean(samp) #stores the mean of "samp" in the i-th position of this vector
  samp <- sample(yrbss2013subset$height_cm, 200) #draws a sample of size 200 using the sample function, and stores in variable "samp"
  sample_means200[i] <- mean(samp) #stores the mean of "samp" in the i-th position of this vector
  samp <- sample(yrbss2013subset$height_cm, 500) #draws a sample of size 500 using the sample function, and stores in variable "samp"
  sample_means500[i] <- mean(samp) #stores the mean of "samp" in the i-th position of this vector
} #indicates end of code within this loop

# d) Increasing the size of each draw from the distribution while keeping the number of samples high and constant seems to yield a similar mean calculation, but a lower standard deviation calculation.


##5
# a)
mean(yrbss2013$days_drink) #calculates the mean of yrbss2013$days_drink
#The mean is 1.45402 days.
histogram(yrbss2013$days_drink) #draws a histogram of yrbss2013$days_drink
#The histogram of yrbss2013$days_drink shows a strong right skew, which is not similar to the roughly normal distribution of yrbss2013$height_cm.

# b)
sample_means10500 <- rep(NA, 500) #creates a vector filled with 500 NA's, which will be filled later with the sample mean in each sample
for (i in 1:500) { #indication of for loop that will run 500 times
  samp <- sample(yrbss2013$days_drink, 10) #draws a sample of size 10 using the sample function, and stores in variable "samp"
  sample_means10500[i] <- mean(samp) #stores the mean of "samp" in the i-th position of this vector
} #indicates end of code within this loop
mean(sample_means10500) #calculates mean of sample_means10500
#The mean is 1.3858 days.

# c)
sample_means50500 <- rep(NA, 500) #creates a vector filled with 500 NA's, which will be filled later with the sample mean in each sample
for (i in 1:500) { #indication of for loop that will run 500 times
  samp <- sample(yrbss2013$days_drink, 50) #draws a sample of size 50 using the sample function, and stores in variable "samp"
  sample_means50500[i] <- mean(samp) #stores the mean of "samp" in the i-th position of this vector
} #indicates end of code within this loop
mean(sample_means50500) #calculates mean of sample_means50500
#The mean is 1.45524 days.

# d)
sample_means105000 <- rep(NA, 5000) #creates a vector filled with 5000 NA's, which will be filled later with the sample mean in each sample
for (i in 1:5000) { #indication of for loop that will run 5000 times
  samp <- sample(yrbss2013$days_drink, 10) #draws a sample of size 10 using the sample function, and stores in variable "samp"
  sample_means105000[i] <- mean(samp) #stores the mean of "samp" in the i-th position of this vector
} #indicates end of code within this loop
mean(sample_means105000) #calculates mean of sample_means105000
#The mean is 1.46922 days.

# e)
sample_means5005000 <- rep(NA, 5000) #creates a vector filled with 5000 NA's, which will be filled later with the sample mean in each sample
for (i in 1:5000) { #indication of for loop that will run 5000 times
  samp <- sample(yrbss2013$days_drink, 500) #draws a sample of size 500 using the sample function, and stores in variable "samp"
  sample_means5005000[i] <- mean(samp) #stores the mean of "samp" in the i-th position of this vector
} #indicates end of code within this loop
mean(sample_means5005000) #calculates mean of sample_means5005000
#The mean is 1.454776 days.

# f) Changing the sample size and number of samples does not appear to have a significant impact on the estimate of the mean.

# g)
hist(sample_means500) #produces a histogram of sample_means 500
hist(yrbss2013$height_cm) #produces a histogram of yrbss2013$height_cm original population

hist(sample_means5005000) #produces a histogram of sample_means5005000
hist(yrbss2013$days_drink) #produces a histogram of yrbss2013$days_drink original population

#The distribution of both seem to be normally distributed. 
#The distributions of the population and sample of yrbss2013$height_cm seem to be similar, as they seem to be normally distributed.
#The distributions of the population and sample of yrbss2013$days_drink are different, as the original population distribution is heavily right skewed, while the sample distribution seems to be normally distributed.

   
##6
bullied <- subset(yrbss2013, bullied == "yes") #creates a subset of people who were bullied
notbullied <- subset(yrbss2013, bullied == "no") #creates a subset of people who were not bullied

sample_meansbullied <- rep(NA, 5000) #creates a vector filled with 5000 NA's, which will be filled later with the sample mean in each sample
for (i in 1:5000) { #indication of for loop that will run 5000 times
   samp <- sample(bullied$weight_kg, 100) #draws a sample of size 100 using the sample function, and stores in variable "samp"
   sample_meansbullied[i] <- mean(samp) #stores the mean of "samp" in the i-th position of this vector
 } #indicates end of code within this loop
 mean(sample_meansbullied) #calculates mean of sample_meansbullied
 #The mean is 65.89127 kg.
 
sample_meansnotbullied <- rep(NA, 5000) #creates a vector filled with 5000 NA's, which will be filled later with the sample mean in each sample
for (i in 1:5000) { #indication of for loop that will run 5000 times
   samp <- sample(notbullied$weight_kg, 100) #draws a sample of size 100 using the sample function, and stores in variable "samp"
   sample_meansnotbullied[i] <- mean(samp) #stores the mean of "samp" in the i-th position of this vector
 } #indicates end of code within this loop
 mean(sample_meansnotbullied) #calculates mean of sample_meansnotbullied
 #The mean is 66.79179 kg.
 
 # a) Students who were not bullied has a higher mean weight in kg.
 
 # b)
 hist(sample_meansbullied) #creates a histogram of weight in kg for students who were bullied
 hist(sample_meansnotbullied) #creates a histogram of weight in kg for students who were not bullied
 



