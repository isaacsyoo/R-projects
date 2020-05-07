## Isaac Yoo

##1 
#Import CourseEvals.csv data set
CourseEvals <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Emory/S2019/QTM 100/Lab/Datasets/CourseEvals.csv")
no_dups<-CourseEvals[!duplicated(CourseEvals$prof_id),] #eliminates multiple course evals of same professor

##2
# a)
install.packages("Rmisc") #installs Rmisc package that contains summarySE() function
library(Rmisc) #calls Rmisc library
summarySE(data = no_dups, measurevar = "bty_avg") #shows summary statistics of no_dups$bty_avg
# There are 94 professors in the data set.

# b)
install.packages("Rmisc") #installs Rmisc package that contains summarySE() function
library(Rmisc) #calls Rmisc library
summarySE(data = no_dups, measurevar = "bty_avg") #shows summary statistics of no_dups$bty_avg
# no_dups$bty_avg -- MEAN: 4.590383, SD: 1.601527

# c)
histogram(no_dups$bty_avg) #draws a histogram of no_dups$bty_avg
# The histogram shows a bimodal distribution, with a peak at 4 and 6.5.


##3
# a)
# The parameter of interest is the beauty rating of all professors.

# b)
# H0: mu = 5
# HA: mu != 5

# c)
# The assumptions regarding sampling distributions are satisified for valid inference.
# SRS: The professors were randomly selected.
# Normality: There are 94 observations (professors); thus, by the CLT (n >= 30), the sample size is large enough for conditions for valid inference to be satisifed.
# Independence: As duplicate evaluations of professors have been removed, the observations are independent.

# d) 
t.test(no_dups$bty_avg, mu = 5) #t-test for bty_avg, H0: mu = 5, conf level = 0.95
#Test statistic: -2.4797

# e)
2 * pt(-2.4797, df=93) #p-value of test statistic=-2.4797 & df=93, two-sided alternative hypothesis
#p-value: 0.0149494

# f)
# At the alpha=0.05 level of significance, I reject H0.

# g)
# Since the p-value of 0.0149494 is lower than the significance level of 0.05, I have enough evidence to reject H0. 
# The data suggests that the beauty rating of the professors differs from 5 on average.

# h)
summarySE(data = no_dups, measurevar = "bty_avg") #shows summary statistics of no_dups$bty_avg
error <- qt(0.975, df=93) * (sd(no_dups$bty_avg) / sqrt(94))
#qt() obtains quantile that corresponds to the 97.5th percentile (for confidence interval) --> 1.985802
#error stores the margin of error
mean(no_dups$bty_avg) - error #to find lower bound of interval
mean(no_dups$bty_avg) + error #to find upper bound of interval
# Confidence interval: (4.262359, 4.918407)
# I am 95% confident that the true mean beauty rating for all professors is between 4.262359 and 4.918407.

##4
#In summarizing the data, we found that 94 professors had an average beauty rating of 4.590383 with a standard deviation of 1.601527. We conducted a one-sample t test to determine if the true average beauty rating differs from 5. The test yielded a test statistic of t = -2.4797 and a p-value of 0.0149494. At the 0.05 level of signifcance, we reject the null hypothesis. We conclude that the true population mean beauty rating is significantly lower than 5. We can be 95% confident that the true average beauty rating is in the interval (4.262359, 4.918407). Since this interval does not contain 5, it is not plausible that the true mean beauty rating is 5, indicating that the students did not successfully follow their instructions to “keep 5 in mind as average.”

##5
#[Test 1] test stat: -2.4797; p-value: 0.0149494; confidence interval: (4.262359, 4.918407)
#[Test 2] test stat: -2.4797; p-value: 0.0149494; confidence interval: (4.315944, 4.864822)
#[Test 3] test stat: -2.4797; p-value: 0.007474702; confidence interval: (4.315944, 4.864822)
#[Test 4] test stat: 0.54716; p-value: 0.5855794; confidence interval: (4.262359, 4.918407)

#[Test 2]
t.test(no_dups$bty_avg, mu = 5, conf.level = 0.90) #t-test for bty_avg, H0: mu = 5, conf level = 0.90
#Test statistic: -2.4797

2 * pt(-2.4797, df=93) #p-value of test statistic=-2.4797 & df=93, two-sided alternative hypothesis
#p-value: 0.0149494

error <- qt(0.95, df=93) * (sd(no_dups$bty_avg) / sqrt(94))
#qt() obtains quantile that corresponds to the 95th percentile (for confidence interval) --> 1.661404
#error stores the margin of error
mean(no_dups$bty_avg) - error #to find lower bound of interval
mean(no_dups$bty_avg) + error #to find upper bound of interval
# Confidence interval: (4.315944, 4.864822)

#[Test 3]
t.test(no_dups$bty_avg, mu = 5, alternative="less") #t-test for bty_avg, H0: mu = 5, one-sided alternative hypothesis (<), conf level = 0.95
#Test statistic: -2.4797

pt(-2.4797, df=93) #p-value of test statistic=-2.4797 & df=93, one-sided alternative hypothesis
#p-value: 0.007474702

error <- qt(0.95, df=93) * (sd(no_dups$bty_avg) / sqrt(94))
#qt() obtains quantile that corresponds to the 95th percentile (for confidence interval) --> 1.661404
#error stores the margin of error
mean(no_dups$bty_avg) - error #to find lower bound of interval
mean(no_dups$bty_avg) + error #to find upper bound of interval
# Confidence interval: (4.315944, 4.864822)

#[Test 4]
t.test(no_dups$bty_avg, mu = 4.5) #t-test for bty_avg, H0: mu = 4.5, conf level = 0.95
#Test statistic: 0.54716

2 * pt(0.54716, df=93, lower.tail=F) #p-value of test statistic=0.54716 & df=93, two-sided alternative hypothesis
#p-value: 0.5855794

error <- qt(0.975, df=93) * (sd(no_dups$bty_avg) / sqrt(94))
#qt() obtains quantile that corresponds to the 97.5th percentile (for confidence interval) --> 1.985802
#error stores the margin of error
mean(no_dups$bty_avg) - error #to find lower bound of interval
mean(no_dups$bty_avg) + error #to find upper bound of interval
# Confidence interval: (4.262359, 4.918407)

# a)
# Changing the level of significance (i) does not affect the test statistic, (ii) does not affect the p-value, and (iii) affects the confidence interval: a higher level of significance decreases the confidence level, resulting in a smaller interval of values for the confidence interval (because less values in the interval causes us to be "less" confident that the interval contains the true mean).

# b)
# Changing the form of the alternative hypothesis (i) does not affect the test statistic, (ii) affects the p-value: a one-sided alternative hypothesis yields a lower p-value (half) than that of a two-sided alternative hypothesis (beacuse a two-sided alternative hypothesis requires us to multiply the p-value by 2 to account for both tails), and (iii) affects the confidence interval: a one-sided alternative hypothesis yields a smaller interval of values (beacuse a one-sided alternative hypothesis yields a smaller p-value than that of a two-sided alternative hypothesis).

# c)
# Changing the value being tested (H0) (i) affects the test-statistic, as a different H0 yields a different test statistic (relative to the sample mean) - an H0 value closer to the sample mean yields a smaller test statistic (in terms of absolute value), (ii) affects the p-value as the test statistic has changed (a higher test statistic (in terms of absolute value) yields a lower p-value), and (iii) does not affect the confidence interval.


##6
# a)
2 * pt(1, df=24, lower.tail=F) #p-value of test statistic=1 & df=24, two-sided alternative hypothesis
#p-value: 0.3272869
#Not significant at the alpha=0.05 level

# b)
2 * pt(2, df=24, lower.tail=F) #p-value of test statistic=2 & df=24, two-sided alternative hypothesis
#p-value: 0.05693985
#Not significant at the alpha=0.05 level

# c)
2 * pt(3, df=24, lower.tail=F) #p-value of test statistic=3 & df=24, two-sided alternative hypothesis
#p-value: 0.006205737
#Significant at the alpha=0.05 level

# d)
2 * pt(1, df=99, lower.tail=F) #p-value of test statistic=1 & df=99, two-sided alternative hypothesis
#p-value: 0.3197485
#Not significant at the alpha=0.05 level

# e)
2 * pt(2, df=99, lower.tail=F) #p-value of test statistic=2 & df=99, two-sided alternative hypothesis
#p-value: 0.04823969
#Significant at the alpha=0.05 level

# f)
2 * pt(3, df=99, lower.tail=F) #p-value of test statistic=3 & df=99, two-sided alternative hypothesis
#p-value: 0.003415508
#Significant at the alpha=0.05 level

#As the test statistic increases, the p-value decreases. Therefore, larger test statistics present more evidence against the null hypothesis. As the degrees of freedom increase, the p-value decreases. This is because as the degrees of freedom increase, the t distribution gets closer to a normal distribution. The same test statistic value with different degrees of freedom can result in a different conclusion for a specified level of significance (e.g., α = 0.05).


##7
# a)
qt(0.95, df=24) #qt() obtains quantile that corresponds to the specified percentile
#t-score: 1.710882

# b)
qt(0.975, df=24) #qt() obtains quantile that corresponds to the specified percentile
#t-score: 2.063899

# c)
qt(0.995, df=24) #qt() obtains quantile that corresponds to the specified percentile
#t-score: 2.79694

# d)
qt(0.95, df=99) #qt() obtains quantile that corresponds to the specified percentile
#t-score: 1.660391

# e)
qt(0.975, df=99) #qt() obtains quantile that corresponds to the specified percentile
#t-score: 1.984217

# f)
qt(0.995, df=99) #qt() obtains quantile that corresponds to the specified percentile
#t-score: 2.626405

#As the confidence level increases, the t-score increases. This means that higher confidence levels result in wider confidence intervals. As the degrees of freedom increase, the t-score decreases. This means that for the same confidence level, larger degrees of freedom result in narrower confidence intervals.


## PART 2
#Import yrbss2013.csv data set
yrbss2013 <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Emory/S2019/QTM 100/Lab/Datasets/yrbss2013.csv")

##8
# a)
hist(yrbss2013$height_m) #creates histogram of yrbss2013$height_m
#The histogram of yrbss2013$height_m shows a roughly normal distribution.

install.packages("Rmisc") #installs Rmisc package that contains summarySE() function
library(Rmisc) #calls Rmisc library
summarySE(data = yrbss2013, measurevar = "height_m") #shows summary statistics of yrbss2013$height_m
#True population MEAN: 1.68729; True population SD: 0.1029791

# b)
# The assumptions regarding sampling distributions are satisified for valid inference.
# SRS: The sample from this population was chosen at random.
# Normality: The histogram of yrbss2013$height_m shows a roughly normal distribution.
# Independence: The sample size (n=20) is less than 10% of the population.

# c) 
# We are testing H0: mu = 1.68729 versus Ha: mu != 1.68729. In the hypothesis test, we run the risk of committing a Type I error because in reality the null hypothesis is actually true. The targeted Type I error rate is 0.05 (5%) and the targeted confidence interval coverage is 95%. Because sampling distribution assumptions are satisfied, we expect the observed Type I error rate and confidence interval coverage from simulation results to equal the targeted levels.

# d)
#code for performing a one-sample t-test on repeated samples from a single quantitative variable
inference.means<-function(variable,sample.size,alpha,num.reps){
  
  samp.est<-rep(NA,num.reps)
  stdev<-rep(NA,num.reps)
  se.xbar<-rep(NA,num.reps)
  test.stat<-rep(NA,num.reps)
  p.val<-rep(NA,num.reps)
  decision<-rep(NA,num.reps)
  lcl<-rep(NA,num.reps)
  ucl<-rep(NA,num.reps)
  capture<-rep(NA,num.reps)
  true.mean<-mean(variable)
  
  for(i in 1:num.reps){
    samp<-sample(variable,sample.size)
    samp.est[i]<-mean(samp)
    stdev[i]<-sd(samp)
    se.xbar[i]<-stdev[i]/sqrt(sample.size)
    test.stat[i]<-(samp.est[i]-true.mean)/se.xbar[i]
    df<-sample.size-1
    p.val[i]<-2*pt(abs(test.stat[i]),df,lower.tail=FALSE)
    t.score<-qt(1-alpha/2,df)
    lcl[i]<-samp.est[i]-t.score*se.xbar[i]
    ucl[i]<-samp.est[i]+t.score*se.xbar[i]
    
    decision[i]<-ifelse(p.val[i]<=alpha,"reject Ho","fail to reject Ho")
    capture[i]<-ifelse(lcl[i]<=true.mean & ucl[i]>=true.mean,"yes","no")
  }
  
  results<-data.frame(samp.est=round(samp.est,4),
                      test.stat=round(test.stat,4),
                      p.val=round(p.val,4),
                      decision=decision,
                      lcl=round(lcl,4),
                      ucl=round(ucl,4),
                      capture=capture)
  return(results)
}

#call method from above with parameters and store in variable height_m_test
height_m_test <- inference.means(yrbss2013$height_m, 20, 0.05, 100)

hist(height_m_test$samp.est) #creates a histogram of height_m_test$samp.est
#The histogram shows a normal distribution for the sample means.

hist(height_m_test$test.stat) #creates a histogram of height_m_test$test.stat
#The histogram shows a normal distribution for the t test statistics.

hist(height_m_test$p.val) #creates a histogram of height_m_test$p.val
#The histogram shows a multimodal distribution for the p-values (no clear pattern).

summary(height_m_test$capture) #creates a summary of height_m_test$capture (how many "reject" and "fail to reject" H0)
#The percent of samples that commit an error in hypothesis test results is 6%.

# e)
#code for plotting confidence interval results
plot.ci<-function(results,true.val){
  par(mar=c(4, 1, 2, 1), mgp=c(2.7, 0.7, 0),xpd=T)
  k <- length(results$lcl)
  xR <- c(min(results$lcl),max(results$ucl))
  yR <- c(0, 41*k/40)
  plot(xR, yR, type='n', xlab='', ylab='', axes=FALSE)
  cols<-ifelse(results$capture=="yes","white","firebrick2")
  segments(results$lcl,1:k,results$ucl,1:k,col=cols,lwd=4)
  points(results$samp.est,1:k,pch=20,col="black")
  segments(results$lcl,1:k,results$ucl,1:k,col="black")
  segments(true.val,0-42/40,true.val,42*k/40, lty=2, col="royalblue3")
  axis(1)
  text(true.val,yR[2],paste("true =",round(true.val,4)),col="royalblue3",pos=3)
}

#call method from above with parameters
plot.ci(height_m_test, mean(yrbss2013$height_m))
#94% of samples have confidence intervals that capture the true parameter value.
#The confidence intervals seem to have varying lengths.
#The confidence intervals seem to be contain the true mean in some position of their interval (94%), thus demonstrating that this is not a random scatter.
#The confidence intervals appear to provide reasonable bounds for the mean.

# f)
#Yes, the inferential methods are performing as expected based on the targeted Type I error rate and confidence interval coverage.
#Expected vs Observed Type I error rate: 5% vs 6%
#Expected vs Observed confidence interval coverage: 95% vs 94%


##9
# a)
hist(yrbss2013$days_smoke) #creates histogram of yrbss2013$days_smoke
#The histogram of yrbss2013$days_smoke shows a right-skewed distribution.

install.packages("Rmisc") #installs Rmisc package that contains summarySE() function
library(Rmisc) #calls Rmisc library
summarySE(data = yrbss2013, measurevar = "days_smoke") #shows summary statistics of yrbss2013$days_smoke
#True population MEAN: 1.496227; True population SD: 5.700752

# b)
# The assumptions regarding sampling distributions are NOT satisified for valid inference.
# SRS: (satisfied) The sample from this population was chosen at random.
# Normality: (not satisfied) The histogram of yrbss2013$days_smoke does NOT show a normal distribution, and the sample size of n=20 is too low for the CLT to be applied.
# Independence: (satisfied) The sample size (n=20) is less than 10% of the population.

# c)
# We are testing H0: mu = 1.496227 versus Ha: mu != 1.496227. In the hypothesis test, we run the risk of committing a Type I error because in reality the null hypothesis is actually true. The targeted Type I error rate is 0.05 (5%) and the targeted confidence interval coverage is 95%. Because sampling distribution assumptions are not satisfied, we expect the observed Type I error rate and confidence interval coverage from simulation results to not equal the targeted levels.

# d)
#code for performing a one-sample t-test on repeated samples from a single quantitative variable
inference.means<-function(variable,sample.size,alpha,num.reps){
  
  samp.est<-rep(NA,num.reps)
  stdev<-rep(NA,num.reps)
  se.xbar<-rep(NA,num.reps)
  test.stat<-rep(NA,num.reps)
  p.val<-rep(NA,num.reps)
  decision<-rep(NA,num.reps)
  lcl<-rep(NA,num.reps)
  ucl<-rep(NA,num.reps)
  capture<-rep(NA,num.reps)
  true.mean<-mean(variable)
  
  for(i in 1:num.reps){
    samp<-sample(variable,sample.size)
    samp.est[i]<-mean(samp)
    stdev[i]<-sd(samp)
    se.xbar[i]<-stdev[i]/sqrt(sample.size)
    test.stat[i]<-(samp.est[i]-true.mean)/se.xbar[i]
    df<-sample.size-1
    p.val[i]<-2*pt(abs(test.stat[i]),df,lower.tail=FALSE)
    t.score<-qt(1-alpha/2,df)
    lcl[i]<-samp.est[i]-t.score*se.xbar[i]
    ucl[i]<-samp.est[i]+t.score*se.xbar[i]
    
    decision[i]<-ifelse(p.val[i]<=alpha,"reject Ho","fail to reject Ho")
    capture[i]<-ifelse(lcl[i]<=true.mean & ucl[i]>=true.mean,"yes","no")
  }
  
  results<-data.frame(samp.est=round(samp.est,4),
                      test.stat=round(test.stat,4),
                      p.val=round(p.val,4),
                      decision=decision,
                      lcl=round(lcl,4),
                      ucl=round(ucl,4),
                      capture=capture)
  return(results)
}

#call method from above with parameters and store in variable days_smoke_test
days_smoke_test <- inference.means(yrbss2013$days_smoke, 20, 0.05, 100)

hist(days_smoke_test$samp.est) #creates a histogram of days_smoke_test$samp.est
#The histogram shows a right-skewed distribution for the sample means.

hist(days_smoke_test$test.stat) #creates a histogram of days_smoke_test$test.stat
#The histogram shows a left-skewed distribution for the t test statistics.

hist(days_smoke_test$p.val) #creates a histogram of days_smoke_test$p.val
#The histogram shows a left-skewed distribution for the p-values, with a high peak on the left and a possible second peak on the right.

summary(days_smoke_test$capture) #creates a summary of days_smoke_test$capture (how many "reject" and "fail to reject" H0)
#The percent of samples that commit an error in hypothesis test results is 30%.

# e)
#code for plotting confidence interval results
plot.ci<-function(results,true.val){
  par(mar=c(4, 1, 2, 1), mgp=c(2.7, 0.7, 0),xpd=T)
  k <- length(results$lcl)
  xR <- c(min(results$lcl),max(results$ucl))
  yR <- c(0, 41*k/40)
  plot(xR, yR, type='n', xlab='', ylab='', axes=FALSE)
  cols<-ifelse(results$capture=="yes","white","firebrick2")
  segments(results$lcl,1:k,results$ucl,1:k,col=cols,lwd=4)
  points(results$samp.est,1:k,pch=20,col="black")
  segments(results$lcl,1:k,results$ucl,1:k,col="black")
  segments(true.val,0-42/40,true.val,42*k/40, lty=2, col="royalblue3")
  axis(1)
  text(true.val,yR[2],paste("true =",round(true.val,4)),col="royalblue3",pos=3)
}

#call method from above with parameters
plot.ci(days_smoke_test, mean(yrbss2013$days_smoke))
#70% of samples have confidence intervals that capture the true parameter value.
#Many confidence intervals do not contain the true mean.

# f)
#No, the inferential methods are not performing as expected based on the targeted Type I error rate and confidence interval coverage.
#Expected vs Observed Type I error rate: 5% vs 30%
#Expected vs Observed confidence interval coverage: 95% vs 70%


##10
# height_m
# [n=20] Assumptions satisifed: yes | Observed Type 1 error rate: 5.01% | Observed confidence interval coverage: 94.99% | Valid inference: yes
# [n=50] Assumptions satisifed: yes | Observed Type 1 error rate: 4.91% | Observed confidence interval coverage: 95.09% | Valid inference: yes
# [n=100] Assumptions satisifed: yes | Observed Type 1 error rate: 4.72% | Observed confidence interval coverage: 95.28% | Valid inference: yes

# days_smoke
# [n=20] Assumptions satisifed: no | Observed Type 1 error rate: 32.01% | Observed confidence interval coverage: 67.99% | Valid inference: no
# [n=50] Assumptions satisifed: yes | Observed Type 1 error rate: 12.79% | Observed confidence interval coverage: 87.21% | Valid inference: no
# [n=100] Assumptions satisifed: yes | Observed Type 1 error rate: 13.26% | Observed confidence interval coverage: 86.74% | Valid inference: no

#code for performing a one-sample t-test on repeated samples from a single quantitative variable
inference.means<-function(variable,sample.size,alpha,num.reps){
  
  samp.est<-rep(NA,num.reps)
  stdev<-rep(NA,num.reps)
  se.xbar<-rep(NA,num.reps)
  test.stat<-rep(NA,num.reps)
  p.val<-rep(NA,num.reps)
  decision<-rep(NA,num.reps)
  lcl<-rep(NA,num.reps)
  ucl<-rep(NA,num.reps)
  capture<-rep(NA,num.reps)
  true.mean<-mean(variable)
  
  for(i in 1:num.reps){
    samp<-sample(variable,sample.size)
    samp.est[i]<-mean(samp)
    stdev[i]<-sd(samp)
    se.xbar[i]<-stdev[i]/sqrt(sample.size)
    test.stat[i]<-(samp.est[i]-true.mean)/se.xbar[i]
    df<-sample.size-1
    p.val[i]<-2*pt(abs(test.stat[i]),df,lower.tail=FALSE)
    t.score<-qt(1-alpha/2,df)
    lcl[i]<-samp.est[i]-t.score*se.xbar[i]
    ucl[i]<-samp.est[i]+t.score*se.xbar[i]
    
    decision[i]<-ifelse(p.val[i]<=alpha,"reject Ho","fail to reject Ho")
    capture[i]<-ifelse(lcl[i]<=true.mean & ucl[i]>=true.mean,"yes","no")
  }
  
  results<-data.frame(samp.est=round(samp.est,4),
                      test.stat=round(test.stat,4),
                      p.val=round(p.val,4),
                      decision=decision,
                      lcl=round(lcl,4),
                      ucl=round(ucl,4),
                      capture=capture)
  return(results)
}

#call method from above with parameters and store in a variable

#height_m (n=20)
height_m_20 <- inference.means(yrbss2013$height_m, 20, 0.05, 10000)
summary(height_m_20$capture) #creates a summary of height_m_20$capture (how many "reject" and "fail to reject" H0)

#height_m (n=100)
height_m_100 <- inference.means(yrbss2013$height_m, 100, 0.05, 10000)
summary(height_m_100$capture) #creates a summary of height_m_100$capture (how many "reject" and "fail to reject" H0)

#days_smoke (n=20)
days_smoke_20 <- inference.means(yrbss2013$days_smoke, 20, 0.05, 10000)
summary(days_smoke_20$capture) #creates a summary of days_smoke_20$capture (how many "reject" and "fail to reject" H0)

#days_smoke (n=50)
days_smoke_50 <- inference.means(yrbss2013$days_smoke, 50, 0.05, 10000)
summary(days_smoke_50$capture) #creates a summary of days_smoke_50$capture (how many "reject" and "fail to reject" H0)

#days_smoke (n=100)
days_smoke_100 <- inference.means(yrbss2013$days_smoke, 100, 0.05, 10000)
summary(days_smoke_100$capture) #creates a summary of days_smoke_100$capture (how many "reject" and "fail to reject" H0)


##11
# a)
# Increasing the sample size for height_m does "fix" our results, as the Type I error rate is lowered and the percent of confidence intervals that capture the true parameter value is increased.
# This may be due to the fact that increasing sample sizes allows our data to be closer to a "normal" distribution -- that is, a larger sample size yields more accurate results (of the true population) than that of a smaller sample size.

# b)
# When assumptions are satisfied for days_smoke, we do NOT always have "valid" inference. 
# This demonstrates that although we may use the CLT to satisfy assumptions for valid statistical inference, the results may not always yield a valid inference.
# Thus, we must always exercise caution even if assumptions required for valid statistical inferences have been fulfilled.




