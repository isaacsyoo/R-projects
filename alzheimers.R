## Isaac Yoo

#1 
#Import ADNI.txt data set
ADNI <- read.delim("~/Library/Mobile Documents/com~apple~CloudDocs/Emory/S2019/QTM 100/Lab/Datasets/ADNI.txt")
dim(ADNI)
#There are 276 rows (276 observations).

#2
str(ADNI)
#DX: categorical, factor
#AGE: quantitative, num
#APOE4: categorical, int
#GENDER: categorical, factor
#MMSE: quantitative, int
#adas: quantitative, num
#WholeBrain: quantitative, int

#3
#Record APOE4 variable to a factor using the factor() function
ADNI$APOE4F <- factor(ADNI$APOE4, labels=c("No copies of the ApoE4 allele", "One copy of the ApoE4 allele", "Two copies of the ApoE4 allele"))
str(ADNI) #checking to see that factor variable has been added
summary(ADNI) #shows a summary of the data set 
#"Two copies of the ApoE4 allele" is the least common genetic variant.

#4
hist(ADNI$AGE)
#A histogram is appropriate to visualize the distribution of AGE. hist() produces this graph. 
#It can be observed that the distribution is in a unimodal fashion, with most observations having an age concentrated in the middle.

#5
# a) A boxplot is appropriate to visualize the relationship between Alzheimer's diagnosis (DX) and the cognitive impairment tests MMSE and adas.
# b) #Boxplot for relationship between DX and MMSE
boxplot(ADNI$MMSE~ADNI$DX) #produces boxplot
title(main = "Relationship between DX and MMSE") #creates title for boxplot
#For MMSE, individuals with Alzheimer's diagnosis generally have a lower score, while MCI and Normal individuals have a higher score with a similar mean, with MCI individuals having a larger range of scores.

#Boxplot for relationship between DX and adas
boxplot(ADNI$adas~ADNI$DX) #produces boxplot
title(main = "Relationship between DX and adas") #creates title for boxplot
#For adas, individuals with Alzheimer's diagnosis generally have a higher score, Normal individuals have a lower score, and the mean score of MCI individuals is between that of those with Alzheimer's diagnosis and Normal individuals. The difference in mean is quite apparent between the groups.

#c) acdas (Alzheimer's Disease Assessment Scale) identifies potential outliers.

#6
# a) Converts WholeBrain measurements to a smaller scale by dividing it by 100,000. Stores into variable WholeBrainMod.
ADNI$WholeBrainMod <- (ADNI$WholeBrain / 100000)

# b) 
install.packages("Rmisc") #installs Rmisc package that contains summarySE() function
library(Rmisc) #calls Rmisc library

#Age
summarySE(data = ADNI, measurevar = "AGE") #summary statistics of "age" variable (overall)
summarySE(data = ADNI, measurevar = "AGE", groupvars = "DX") #summary statistics of "age" variable (separated by each diagnosis)

#WholeBrainMod
summarySE(data = ADNI, measurevar = "WholeBrainMod") #summary statistics of "WholeBrainMod" variable (overall)
summarySE(data = ADNI, measurevar = "WholeBrainMod", groupvars = "DX") #summary statistics of "WholeBrainMod" variable (separated by each diagnosis)

#Gender
tableGender1 <- table(ADNI$GENDER) #creates tableGender1 object that stores Gender table
tableGender2 <- table(ADNI$GENDER, ADNI$DX) #creates tableGender2 object that stores 2-2 contingency table (Gender & DX)
tableGender1 #produces table that shows overall frequencies of gender
tableGender2 #produces table that shows frequencies of 2-2 contingency table (Gender & DX)
prop.table(tableGender1) #calculates overall gender proportions
prop.table(tableGender2,margin=2) #calculates column proportion (Gender & DX)

#APOE4F
tableAPOE4F1 <- table(ADNI$APOE4F) #creates tableAPOE4F1 object that stores APOE4F table
tableAPOE4F2 <- table(ADNI$APOE4F, ADNI$DX) #creates tableAPOE4F object that stores 2-2 contingency table (APOE4F & DX)
tableAPOE4F1 #produces table that shows overall frequencies of APOE4F
tableAPOE4F2 #produces table that shows frequencies of 2-2 contingency table (APOE4F & DX)
prop.table(tableAPOE4F1) #calculates overall APOE4F proportions
prop.table(tableAPOE4F2,margin=2) #calculates column proportion (APOE4F & DX)

#Overall: n = 276, Age = 73.6 +- 7.0, Gender(Male) = 153 (55.4%), Brain volume x 10^5 mm^3 = 10.2 +- 1.1, APOE4 No copies = 137 (49.6%), APOE4 One copy = 109 (39.5%), APOE4 Two copies = 30 (10.9%)
#AD: n = 54, Age = 73.9 +- 8.0, Gender(Male) = 30 (55.6%), Brain volume x 10^5 mm^3 = 9.7 +- 1.2, APOE4 No copies = 18 (33.3%), APOE4 One copy = 25 (46.3%), APOE4 Two copies = 11 (20.4%)
#MCI: n = 128, Age = 72.9 +- 7.3, Gender(Male) = 75 (58.6%), Brain volume x 10^5 mm^3 = 10.3 +- 1.1, APOE4 No copies = 57 (44.5%), APOE4 One copy = 56 (43.8%), APOE4 Two copies = 15 (11.7%)
#Normal: n = 94, Age = 74.3 +- 5.8, Gender(Male) = 48 (51.1%), Brain volume x 10^5 mm^3 = 10.4 +- 1.0, APOE4 No copies = 62 (66.0%), APOE4 One copy = 28 (29.8%), APOE4 Two copies = 4 (4.3%)

#7
#The ADNI study has 276 participants. The average age is 73.6 years and 55.4% are male. The Alzheimer's group has a lower average brain volume than the Normal group (9.7 vs 10.4 mm^3 x 10^5).
#Patients with Alzheimer's diagnosis have a higher prevalence of two copies of the APOE4 allele compared to normal diagnosis patients (20.4% vs 4.3%).

