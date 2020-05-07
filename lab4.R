## Isaac Yoo

##1 
#Import abductees.csv data set
abductees <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Emory/S2019/QTM 100/Lab/Datasets/abductees.csv")
summary(abductees)
#two NA's in various variables, two "female" variables for sex, unusually high number (100) in abdtimes

# a)
abductees$sex2 <- abductees$sex #create new variable which will be the correctly cleaned version
abductees$sex2[abductees$sex == "Female"] <- "female" #re-code all values of "Female" to "female"
abductees$sex2 <- factor(abductees$sex2) #drop "Female" as a factor level
table(abductees$sex2) #check re-coding


boxplot(abductees$abdtimes) #boxplot of the number of times an individual claimed to be abducted
abductees$abdtimes2 <- abductees$abdtimes #create new variable which will be the correctly cleaned version
abductees$abdtimes2[abductees$abdtimes==60 | abductees$abdtimes==100] <- NA #re-code 60 & 100 to NA
table(abductees$abdtimes2) #check re-coding


##2
abductees$age <- 90 - abductees$yearbir #create new variable that calculates age of respondent


##3
abductees$edulevel <- factor(NA, levels=c("less than or equal to high school", "college education", "more than college")) #create new variable for education category
#assign values of education category
abductees$edulevel[abductees$educat<=12] <- "less than or equal to high school" #less than or equal to 12 years of education
abductees$edulevel[abductees$educat>12 & abductees$educat<=16] <- "college education" #13-16 years of education
abductees$edulevel[abductees$educat>16] <- "more than college" #more than 16 years of education
table(abductees$educat, abductees$edulevel) #check coding of education category (edulevel)


##4
abductees$marstatNew <- factor(ifelse(abductees$marstat=="Married", "Married", "Other")) #create dichotomous factor marriage status variable, implementation of if-else statement
table(abductees$marstat, abductees$marstatNew) #check re-coding of marstatNew


##5
# a)
boxplot(abductees$age~abductees$abdfeel) #side-by-side boxplot between distribution of age (age) and how respondents felt (abdfeel)
#It is difficult to easily identify a trend, as abdfeel is listed (on the x-axis) in alphabetical order rather than its correct order of level (negative --> positive).

# b)
abductees$abdfeel <- factor(abductees$abdfeel, levels=c("Entirely negative", "Mostly negative", "About equally positive and negative", "Mostly positive", "Entirely positive")) #order the levels of abdfeel in its correct order
boxplot(abductees$age~abductees$abdfeel) #side-by-side boxplot between distribution of age (age) and how respondents felt (abdfeel)
#Now, it is easy to identify a trend. There seems to be a positive correlation between age and how one felt about their abduction experience, as an increase in age tends to yield a more "positive" rating (in abdfeel).


##6
# a)
abducteesFemaleSubset <- subset(abductees, sex2 == "female") #creates a subset of just females
mean(abducteesFemaleSubset$age, na.rm=T) #mean of age w/ sex2 == "female"
#The average age of female respondents is 41.6.

# b)
abducteesMarOther <- subset(abductees, marstatNew == "Other") #creates a subset for individuals with a marriage status of "Other" (in marstatNew, the reclassified variable)
mean(abducteesMarOther$abdtimes) #mean of abdtimes w/ marstatNew == "Other"
#3.3: The average number of abduction times among individuals classified as "other" for marital status.

# c) 
summary(abductees$edulevel) #summary of edulevel to see frequencies
#29 individuals have a college education (but no more than a college education).

# d)
#There seems to be a positive correlation between age and how one felt about their abduction experience, as an increase in age tends to yield a more "positive" rating (in abdfeel).

