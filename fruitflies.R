## Isaac Yoo

#Import fruitfly.csv data set
fruitfly <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Emory/S2019/QTM 100/Lab/Datasets/fruitfly.csv")

##1 
# a)
boxplot(fruitfly$lifespan~fruitfly$type)
#This code produces 5 boxplots representing the lifespan of the fruitflies, broken down by their experimental assignment (type).

# b)
#Experimental type 5 (males assigned to live with 8 virgin females) shows the shortest average lifespan. 
install.packages("Rmisc") #installs Rmisc package that contains summarySE() function
library(Rmisc) #calls Rmisc library
summarySE(data = fruitfly, measurevar = "lifespan", groupvars = "type") #summary statistics of fruitfly's "lifespan" variable (separated by each type)
#summary statistics shows a mean of 38.72 days and standard deviation of 12.10207 for type 5.


##2
# a)
install.packages("Rmisc") #installs Rmisc package that contains summarySE() function
library(Rmisc) #calls Rmisc library
summarySE(data = fruitfly, measurevar = "lifespan", groupvars = "type") #summary statistics of fruitfly's "lifespan" variable (separated by each type)

#Supplied with 8 virgin females: N(38.7, 12.1)
#pnorm() calculates probabiltiies of certain outcomes of variable (the lower tail)
pnorm(q = 30, mean = 38.7, sd = 12.1) #Type 5 Days <30: 0.2361
#diff(pnorm()) calculates probability of variable falling into an interval
diff(pnorm(q = c(30, 50), mean = 38.7, sd = 12.1)) #Type 5 Days 30-50: 0.5888
diff(pnorm(q = c(50, 70), mean = 38.7, sd = 12.1)) #Type 5 Days 50-70: 0.1703
#1 - pnorm() calculates probabiltiies of certain outcomes of variable (the upper tail)
1 - pnorm(q = 70, mean = 38.7, sd = 12.1) #Type 5 Days >70: 0.0048

# b) 
#These fruitflies most likely came from the 'supplied with 8 newly pregnant females' group because this group has a probability of 0.4978 for a lifespan of 50-70 days, and 0.3245 for a lifespan of >70 days.
#Both of these probabilities are higher than that of the 'supplied with 8 virgin females' group, which are 0.1703 and 0.0048, respectively.

# c)
fruitflysubset <- subset(fruitfly, type==5) #creates a data set that only contains the group of fruitflies with the shortest average lifespan (type 5)

#Supplied with 8 virgin females: N(38.7, 12.1)
#qnorm() is used to find the percentiles of the normal distribution (theoretical)
qnorm(p = 0.1, mean = 38.7, sd = 12.1) #Type 5 10th percentile theoretical: 23.2
qnorm(p = 0.25, mean = 38.7, sd = 12.1) #Type 5 25th percentile theoretical: 30.5
qnorm(p = 0.5, mean = 38.7, sd = 12.1) #Type 5 50th percentile theoretical: 38.7
qnorm(p = 0.75, mean = 38.7, sd = 12.1) #Type 5 75th percentile theoretical: 46.9
qnorm(p = 0.9, mean = 38.7, sd = 12.1) #Type 5 90th percentile theoretical: 54.2
#quantile() is used to find the percentiles of the normal distribution (observed)
quantile(x = fruitflysubset$lifespan, probs = 0.1) #Type 5 10th percentile observed: 21.8
quantile(x = fruitflysubset$lifespan, probs = 0.25) #Type 5 25th percentile observed: 32.0
quantile(x = fruitflysubset$lifespan, probs = 0.5) #Type 5 50th percentile observed: 40.0
quantile(x = fruitflysubset$lifespan, probs = 0.75) #Type 5 75th percentile observed: 47.0
quantile(x = fruitflysubset$lifespan, probs = 0.9) #Type 5 90th percentile observed: 54.0

# d) 
#The theoretical and observed quantiles are close together, validating the assumption that lifespan follows a normal distribution.


##3
# a) 
#The following line of code gives us the proportion of fruitflies that survived at least 50 days among those that were supplied with 8 virgin females (type 5).
sum(fruitflysubset$lifespan >= 50) / length(fruitflysubset$lifespan) #0.2

# b)
#data.frame() creates a new data frame that holds the values of dbinom()
#dbinom() gives the probability that a certain number of fruitflies survive at least 50 days
Fruitfly10 <- data.frame(x = 0:10, prob = dbinom(x = 0:10, size = 10, prob = 0.2)) #create data frame
Fruitfly10 #view data frame

#Supplied with 8 virgin females: Bin(10, 0.2)
#0: 0.1074
#1: 0.2684
#2: 0.3020
#3: 0.2013
#4: 0.0881
#5: 0.0264
#6: 0.0055
#7: 0.0008
#8: 0.0001
#9: 0.0000
#10: 0.0000

# c)
#The probability of exactly 6 fruitflies surviving at least 50 days is 0.1343 for the 'supplied with 8 newly pregnant females' group, and 0.0055 for the 'supplied with 8 virgin females' group.
#The 'supplied with 8 newly pregnant females' group has a higher probability of exactly 5 fruitflies surviving at least 50 days.

# d)
#The most likely number of fruitflies out of 10 to survive at least 50 days for the 'supplied with 8 newly pregnant females' group is 8.
#The most likely number of fruitflies out of 10 to survive at least 50 days for the 'supplied with 8 virgin females' group is 2.
#The 'supplied with 8 newly pregnant females' group is expected to have more fruitflies survive at least 50 days (0.76 vs 0.2).

# e)
#sum(dbinom()) calculates the probability associated with a range of values by using sum
sum(dbinom(x = 5:10, size = 10, prob = 0.76)) #'supplied with 8 newly pregnant females' group: 0.9838884
sum(dbinom(x = 5:10, size = 10, prob = 0.2)) #'supplied with 8 virgin females' group: 0.0327935
#The 'supplied with 8 newly pregnant females' group is more likely to have at least 5 fruitflies survive past 50 days (0.9838884 vs 0.0327935).
