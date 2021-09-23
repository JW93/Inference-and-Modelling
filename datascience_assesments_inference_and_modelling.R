setwd("C:/Users/Johannes/Documents/Johannes/Data Science")
library("gtools")
library("tidyverse")
library("dslabs")
library("dplyr")
library("ggplot2")

#DATACAMP ASSESMENT
#Write a line of code that calculates the standard error se of a sample 
#average when you poll 25 people in the population. Generate a sequence of 
#100 proportions of Democrats p that vary from 0 (no Democrats) to 1 (all
#Democrats).
#Plot se versus p for the 100 different proportions.

N <- 25
p <- seq(0, 1, length.out=100)
se <- sqrt(p*(1-p)/N)
plot(p, se)

#Using the same code as in the previous exercise, create a for-loop that 
#generates three plots of p versus se when the sample sizes equal N=25, 
#N=100, and N=1000.

p <- seq(0, 1, length = 100)
sample_sizes <- c(25, 100, 1000)
for(N in sample_sizes) {
  se <- sqrt(p*(1-p)/N)
  plot(p, se, ylim=c(0, .1))
}

#Say the actual proportion of Democratic voters is p=0.45. In this case, 
#the Republican party is winning by a relatively large margin of d=-0.1
# or a 10% margin of victory. What is the standard error of the spread
#2X_bar-1 in this case?

N <- 25
p <- 0.45
2*sqrt(p*(1-p)/N)

#Write function called take_sample that takes the proportion of Democrats
#p and the sample size N as arguments and returns the sample average of 
#Democrats (1) and Republicans (0).
#Calculate the sample average if the proportion of Democrats equals 0.45 
#and the sample size is 100.

take_sample <- function(p){
  samp <- sample(c(1,0), size=N, replace=TRUE, prob=c(p, 1-p))
  mean(samp)
}
set.seed(1)
p <- 0.45
N <- 100
sapply(p, take_sample)

#Assume the proportion of Democrats in the population p equals 0.45 and 
#that your sample size N is 100 polled voters. The take_sample function 
#you defined previously generates our estimate, X_bar.
#Replicate the random sampling 10,000 times and calculate p-X_bar for 
#each random sample. Save these differences as a vector called errors.
#Find the average of errors and plot a histogram of the distribution.

p <- 0.45
N <- 100
B <- 10000
set.seed(1)
errors <- replicate(B, {
  X <- take_sample(p,N)
  p-X
})
mean(errors)

#The error p-X_bar is a random variable. In practice, the error is not 
#observed because we do not know the actual proportion of Democratic 
#voters,p. However, we can describe the size of the error by constructing
#a simulation.
#What is the average size of the error if we define the size by taking 
#the absolute value?

p <- 0.45
N <- 100
B <- 10000
set.seed(1)
errors <- replicate(B, p - take_sample(p, N))
mean(abs(errors))

#The standard error is related to the typical size of the error we make
#when predicting. We say size because, as we just saw, the errors are 
#centered around 0. In that sense, the typical error is 0. For 
#mathematical reasons related to the central limit theorem, we actually 
#use the standard deviation of errors rather than the average of the 
#absolute values.
#As we have discussed, the standard error is the square root of the
#average squared distance (X_bar-p)^2. The standard deviation is defined
#as the square root of the distance squared.
#Calculate the standard deviation of the spread.

p <- 0.45
N <- 100
B <- 10000
set.seed(1)
errors <- replicate(B, p - take_sample(p, N))
mean(errors^2)
sqrt(mean(errors^2))

#The theory we just learned tells us what this standard deviation is 
#going to be because it is the standard error of X_bar.
#Estimate the standard error given an expected value of 0.45 and a 
#sample size of 100.

p <- 0.45
N <- 100
sqrt(p*(1-p)/N)

#In practice, we don't know p, so we construct an estimate of the 
#theoretical prediction based by plugging in X_bar for p. Calculate the 
#standard error of the estimate: SE_Bar(X_bar)

p <- 0.45
N <- 100
set.seed(1)
X <- sample(c(1,0), size=N, replace=TRUE, prob=c(p, 1-p))
X_bar <- mean(X)
sqrt(X_bar*(1-X_bar)/N)

#Make a qq-plot of the errors you generated previously to see if they 
#follow a normal distribution.

p <- 0.45
N <- 100
B <- 10000
set.seed(1)
errors <- replicate(B, p - take_sample(p, N))
qqnorm(errors); qqline(errors)

#If p=0.45 and N=100, use the central limit theorem to estimate the 
#probability that X_bar>0.5?

p <- 0.45
N <- 100
1-pnorm(.5, p, sqrt(p*(1-p)/N))

#Assume you are in a practical situation and you don't know p. Take a 
#sample of size N=100 and obtain a sample average of X_bar=0.51.
#What is the CLT approximation for the probability that your error size 
#is equal or larger than 0.01?

N <-100

# Define `X_hat` as the sample average
X_hat <- 0.51

# Define `se_hat` as the standard error of the sample average
se_hat <- sqrt(X_hat*(1-X_hat)/N)

1 - pnorm(.01, 0, se_hat) + pnorm(-0.01, 0, se_hat)

#For the following exercises, we will use actual poll data from the 2016
#election. The exercises will contain pre-loaded data from the dslabs 
#package.
library(dslabs)
data("polls_us_election_2016")
#We will use all the national polls that ended within a few weeks before
#the election.
#Assume there are only two candidates and construct a 95% confidence 
#interval for the election night proportion p.

# Load the data
data(polls_us_election_2016)
head(polls_us_election_2016)
# Generate an object `polls` that contains data filtered for polls that ended on or after October 31, 2016 in the United States
polls <- polls_us_election_2016 %>% filter(enddate>= "2016-10-31" & state=="U.S.")

# How many rows does `polls` contain? Print this value to the console.
nrow(polls)

# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.

head(polls)
N <- 2220
polls$samplesize[1]
# For the first poll in `polls`, assign the estimated percentage of Clinton voters to a variable called `X_hat`. Print this value to the console.
X_hat <- .47
X_hat
# Calculate the standard error of `X_hat` and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- sqrt(X_hat*(1-X_hat)/N)
se_hat
# Use `qnorm` to calculate the 95% confidence interval for the proportion of Clinton voters. Save the lower and then the upper confidence interval to a variable called `ci`.
qnorm(0.95, X_hat, se_hat)

ci <- c(X_hat-qnorm(0.975)*se_hat, X_hat+qnorm(0.975)*se_hat)
ci

#Create a new object called pollster_results that contains the pollster's
#name, the end date of the poll, the proportion of voters who declared a
#vote for Clinton, the standard error of this estimate, and the lower and
#upper bounds of the confidence interval for the estimate.

# The `polls` object that filtered all the data by date and nation has already been loaded. Examine it using the `head` function.
head(polls)

# Create a new object called `pollster_results` that contains columns for pollster name, end date, X_hat, se_hat, lower confidence interval, and upper confidence interval for each poll.

pollster_results <- polls %>% 
  mutate(X_hat=rawpoll_clinton/100, se_hat=sqrt(X_hat*(1-X_hat)/samplesize), lower=X_hat-qnorm(0.975)*se_hat, upper=X_hat+qnorm(0.975)*se_hat) %>% 
  select(pollster, enddate, X_hat, se_hat, lower, upper)
head(pollster_results)

#The final tally for the popular vote was Clinton 48.2% and Trump 46.1%.
#Add a column called hit to pollster_results that states if the confidence
#interval included the true proportion p=0.482 or not. What proportion 
#of confidence intervals included p?

# The `pollster_results` object has already been loaded. Examine it using the `head` function.
head(pollster_results)

# Add a logical variable called `hit` that indicates whether the actual value exists within the confidence interval of each poll. Summarize the average `hit` result to determine the proportion of polls with confidence intervals include the actual value. Save the result as an object called `avg_hit`.
avg_hit <- pollster_results %>% 
  mutate(hit=ifelse(0.482>=lower & 0.482<=upper, 1, 0)) %>% 
  summarise(avg=mean(hit))
head(avg_hit)
avg_hit

#A much smaller proportion of the polls than expected produce confidence
#intervals containing p. Notice that most polls that fail to include p 
#are underestimating. The rationale for this is that undecided voters 
#historically divide evenly between the two main candidates on election 
#day.
#In this case, it is more informative to estimate the spread or the 
#difference between the proportion of two candidates d, or 
#0.482 - 0.461 =0.021 for this election.
#Assume that there are only two parties and that d=2p-1. Construct a 95%
#confidence interval for difference in proportions on election night.

# Add a statement to this line of code that will add a new column named `d_hat` to `polls`. The new column should contain the difference in the proportion of voters.
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") %>% 
  mutate(d_hat=rawpoll_clinton-rawpoll_trump)


# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls$samplesize[1]
N
# Assign the difference `d_hat` of the first poll in `polls` to a variable called `d_hat`. Print this value to the console.
d_hat <- polls$d_hat[1]/100
d_hat
# Assign proportion of votes for Clinton to the variable `X_hat`.
X_hat <- (d_hat+1)/2

# Calculate the standard error of the spread and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- 2*sqrt(X_hat*(1-X_hat)/N)


# Use `qnorm` to calculate the 95% confidence interval for the difference in the proportions of voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(d_hat-qnorm(.975)*se_hat, d_hat+qnorm(.975)*se_hat)
ci

#Create a new object called pollster_results that contains the pollster's
#name, the end date of the poll, the difference in the proportion of 
#voters who declared a vote either, and the lower and upper bounds of
#the confidence interval for the estimate.

# The subset `polls` data with 'd_hat' already calculated has been loaded. Examine it using the `head` function.
head(polls)

# Create a new object called `pollster_results` that contains columns for pollster name, end date, d_hat, lower confidence interval of d_hat, and upper confidence interval of d_hat for each poll.
pollster_results <- polls %>% mutate(X_hat=(d_hat+1)/2, 
                                     se_hat=2*sqrt(X_hat*(1-X_hat)/samplesize), 
                                     lower=d_hat-qnorm(.975)*se_hat, upper=d_hat+qnorm(.975)*se_hat) %>% 
  select(pollster, enddate, d_hat, lower, upper)

#What proportion of confidence intervals for the difference between the
#proportion of voters included d, the actual difference in election day?

# The `pollster_results` object has already been loaded. Examine it using the `head` function.
head(pollster_results)

# Add a logical variable called `hit` that indicates whether the actual value (0.021) exists within the confidence interval of each poll. Summarize the average `hit` result to determine the proportion of polls with confidence intervals include the actual value. Save the result as an object called `avg_hit`.
avg_hit <- pollster_results %>% 
  mutate(hit=ifelse(0.021>=lower & 0.021<=upper, 1, 0)) %>% 
  summarise(avg=mean(hit))

#Exercise 8. Comparing to actual results by pollster
#Although the proportion of confidence intervals that include the actual
#difference between the proportion of voters increases substantially, it
#is still lower that 0.95. In the next chapter, we learn the reason for 
#this. 
#To motivate our next exercises, calculate the difference between each 
#poll's estimate d and the actual d=0.021. Stratify this difference, or
#error, by pollster in a plot.

# The `polls` object has already been loaded. Examine it using the `head` function.
head(polls)

# Add variable called `error` to the object `polls` that contains the difference between d_hat and the actual difference on election day. Then make a plot of the error stratified by pollster.
polls %>% mutate(error=0.021-d_hat) %>% ggplot(aes(d_hat, error)) +
  geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Exercise 9. Comparing to actual results by pollster - multiple polls
#Remake the plot you made for the previous exercise, but only for pollsters 
#that took five or more polls.
#You can use dplyr tools group_by and n to group data by a variable of 
#interest and then count the number of observations in the groups. The 
#function filter filters data piped into it by your specified condition.
#For example:
  data %>% group_by(variable_for_grouping) 
%>% filter(n() >= 5)

  # The `polls` object has already been loaded. Examine it using the `head` function.
  head(polls)
  
  # Add variable called `error` to the object `polls` that contains the difference between d_hat and the actual difference on election day. Then make a plot of the error stratified by pollster, but only for 
  #pollsters who took 5 or more polls.
  polls %>% mutate(error=0.021-d_hat) %>% group_by(pollster) %>% 
    filter(n()>=5) %>% ggplot(aes(d_hat, error)) + geom_point()
  
#Exercise 1 - Heights Revisited
#We have been using urn models to motivate the use of probability models.
#However, most data science applications are not related to data obtained
#from urns. More common are data that come from individuals. Probability
#plays a role because the data come from a random sample. The random sample
#is taken from a population and the urn serves as an analogy for the 
#population.
#Let's revisit the heights dataset. For now, consider x to be the heights
#of all males in the data set. Mathematically speaking, x is our population. 
#Using the urn analogy, we have an urn with the values of x in it.

#What are the population average and standard deviation of our population?
  
# Load the 'dslabs' package and data contained in 'heights'
library(dslabs)
data(heights)
names(heights)
# Make a vector of heights from all males in the population
x <- heights %>% filter(sex == "Male") %>%
    .$height
  
# Calculate the population average. Print this value to the console.
mean(x)
  
# Calculate the population standard deviation. Print this value to the console.
sd(x)

#Exercise 2 - Sample the population of heights
#Call the population average computed above muand the standard deviation
#sigma. Now take a sample of size 50, with replacement, and construct an
#estimate for mu and sigma.

# The vector of all male heights in our population `x` has already been loaded for you. You can examine the first six elements using `head`.
head(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `X` as a random sample from our population `x`
X <- sample(x, N, replace=T)

# Calculate the sample average. Print this value to the console.
mean(X)

# Calculate the sample standard deviation. Print this value to the console.
sd(X)

#Exercise 4 - Confidence Interval Calculation
#We will use X_bar as our estimate of the heights in the population from 
#our sample size N. We know from previous exercises that the standard estimate
#of our error X_bar-mu is sigma/sqrtN.
#Construct a 95% confidence interval for mu.

# The vector of all male heights in our population `x` has already been loaded for you. You can examine the first six elements using `head`.
head(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `X` as a random sample from our population `x`
X <- sample(x, N, replace = TRUE)

# Define `se` as the standard error of the estimate. Print this value to the console.
se <- sd(X)/sqrt(N)
se

# Construct a 95% confidence interval for the population average based on our sample. Save the lower and then the upper confidence interval to a variable called `ci`.

ci <- c(mean(X)-qnorm(.975)*se, mean(X)+qnorm(0.975)*se)

#Exercise 5 - Monte Carlo Simulation for Heights
#Now run a Monte Carlo simulation in which you compute 10,000 confidence
#intervals as you have just done. What proportion of these intervals 
#include mu?

# Define `mu` as the population average
mu <- mean(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `B` as the number of times to run the model
B <- 10000

# Define an object `res` that contains a logical vector for simulated intervals that contain mu
res <- replicate(B, {
  X <-sample(x, N, replace=TRUE)
  interval <- c(mean(X)-qnorm(.975)*sd(X)/sqrt(N), mean(X)+qnorm(.975)*sd(X)/sqrt(N))
  between(mu, mean(X)-qnorm(.975)*sd(X)/sqrt(N), mean(X)+qnorm(.975)*sd(X)/sqrt(N))
})

# Calculate the proportion of results in `res` that include mu. Print this value to the console.
mean(res)

#Exercise 6 - Visualizing Polling Bias
#In this section, we used visualization to motivate the presence of pollster
#bias in election polls. Here we will examine that bias more rigorously.
#Lets consider two pollsters that conducted daily polls and look at national
#polls for the month before the election.
#Is there a poll bias? Make a plot of the spreads for each poll.

# Load the libraries and data you need for the following exercises
library(dslabs)
library(dplyr)
library(ggplot2)
data("polls_us_election_2016")

# These lines of code filter for the polls we want and calculate the spreads
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 

# Make a boxplot with points of the spread for each pollster
polls %>% ggplot(aes(rawpoll_clinton/100, rawpoll_trump/100)) + geom_boxplot()+ geom_point()

#Exercise 13 - Compute the Estimates
#The answer to the previous question depends on sigma_1 and sigma_2, 
#which we don't know. We learned that we can estimate these values using 
#the sample standard deviation.
#Compute the estimates of sigma_1 and 2.

# The `polls` data have already been loaded for you. Use the `head` function to examine them.
head(polls)

# Create an object called `sigma` that contains a column for `pollster` and a column for `s`, the standard deviation of the spread
sigma <- polls %>% group_by(pollster) %>% 
  summarise(s=sd(spread))


# Print the contents of sigma to the console
sigma

#Exercise 15 - Calculate the 95% Confidence Interval of the Spreads

#We have constructed a random variable that has expected value b2-b1, 
#the pollster bias difference. If our model holds, then this random variable
#has an approximately normal distribution. The standard error of this random 
#variable depends on sigma_1 and sigma_2, but we can use the sample standard
#deviations we computed earlier. We have everything we need to answer our initial
#question: is b2-b1 different from 0?
#Construct a 95% confidence interval for the difference b2 and b1. Does this 
#interval contain zero?

# The `polls` data have already been loaded for you. Use the `head` function to examine them.
head(polls)
names(polls)
# Create an object called `res` that summarizes the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>% group_by(pollster) %>% summarise(avg=mean(spread), s=sd(spread), number_polls=n())
head(res)
res

# Store the difference between the larger average and the smaller in a variable called `estimate`. Print this value to the console.
estimate <- res$avg[2]-res$avg[1]
estimate


# Store the standard error of the estimates as a variable called `se_hat`. Print this value to the console.

se_hat <- sqrt(res$s[2]^2/res$number_polls[2] + res$s[1]^2/res$number_polls[1])


# Calculate the 95% confidence interval of the spreads. Save the lower and then the upper confidence interval to a variable called `ci`.

ci <- c(estimate-qnorm(0.975)*se_hat, estimate+qnorm(0.975)*se_hat)

#Exercise 16 - Calculate the P-value
#The confidence interval tells us there is relatively strong pollster 
#effect resulting in a difference of about 5%. Random variability does 
#not seem to explain it.
#Compute a p-value to relay the fact that chance does not explain the 
#observed pollster effect.

# We made an object `res` to summarize the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>% group_by(pollster) %>% 
  summarize(avg=mean(spread), s=sd(spread), N=n()) 

# The variables `estimate` and `se_hat` contain the spread estimates and standard error, respectively.
estimate <- res$avg[2] - res$avg[1]
se_hat <- sqrt(res$s[2]^2/res$N[2] + res$s[1]^2/res$N[1])

# Calculate the p-value
2*(1-(pnorm(estimate/se_hat, 0, 1)))

#Exercise 17 - Comparing Within-Poll and Between-Poll Variability
#We compute statistic called the t-statistic by dividing our estimate of
# b2-b1 by its estimated standard error:
#Y_bar2-Y_bar1/sqrt(s2^2/N2+s1^2/N1)
#Later we learn will learn of another approximation for the distribution
#of this statistic for values of N1 and N2 that aren't large enough for the CLT.
#Note that our data has more than two pollsters. We can also test for pollster 
#effect using all pollsters, not just two. The idea is to compare the 
#variability across polls to variability within polls. We can construct 
#statistics to test for effects and approximate their distribution. The 
#area of statistics that does this is called Analysis of Variance or ANOVA.
#We do not cover it here, but ANOVA provides a very useful set of tools 
#to answer questions such as: is there a pollster effect?
#Compute the average and standard deviation for each pollster and 
#examine the variability across the averages and how it compares to the
#variability within the pollsters, summarized by the standard deviation.

# Execute the following lines of code to filter the polling data and calculate the spread
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-15" &
           state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ungroup()

# Create an object called `var` that contains columns for the pollster, mean spread, and standard deviation. Print the contents of this object to the console.
var <- polls %>% group_by(pollster) %>% summarise(avg=mean(spread), s=sd(spread))
var

##Bayesian Statistics
#Exercise 2 - Recalculating the SIDS Statistics
#Let's assume that there is in fact a genetic component to SIDS and the 
#the probability of P(2nd_case_SIDS|1st_case_SIDS)=1/100, is much higher 
#than 1 in 8,500.
#What is the probability of both of Sally Clark's sons dying of SIDS?

# Define `Pr_1` as the probability of the first son dying of SIDS
Pr_1 <- 1/8500

# Define `Pr_2` as the probability of the second son dying of SIDS
Pr_2 <- 1/100

# Calculate the probability of both sons dying of SIDS. Print this value to the console.
Pr_1*Pr_2

##Exercise 4 - Calculate the Probability
#Assume that the probability of a murderer finding a way to kill her two
#children without leaving evidence of physical harm is:
#P(2childrendeadwithnoevdenceofharm|motherismurderer)=.5
#Assume that the murder rate among mothers is 1 in 1,000,000. 
#P(motherismurderer)=1/10^6
#According to Bayes' rule, what is the probability of:
#P(motherismurderer|2childrendeadwithnoevdenceofharm)?
# Define `Pr_1` as the probability of the first son dying of SIDS
Pr_1 <- 1/8500

# Define `Pr_2` as the probability of the second son dying of SIDS
Pr_2 <- 1/100

# Define `Pr_B` as the probability of both sons dying of SIDS
Pr_B <- Pr_1*Pr_2

# Define Pr_A as the rate of mothers that are murderers
Pr_A <- 1/1000000

# Define Pr_BA as the probability that two children die without evidence of harm, given that their mother is a murderer
Pr_BA <- 0.50

# Define Pr_AB as the probability that a mother is a murderer, given that her two children died with no evidence of physical harm. Print this value to the console.
Pr_AB <- (Pr_BA*Pr_A)/Pr_B
Pr_AB

##Exercise 6 - Back to Election Polls
#Florida is one of the most closely watched states in the U.S. election
#because it has many electoral votes and the election is generally close.
#Create a table with the poll spread results from Florida taken during
#the last days before the election using the sample code.

#The CLT tells us that the average of these spreads is approximately 
#normal. Calculate a spread average and provide an estimate of the 
#standard error.

# Load the libraries and poll data
library(dplyr)
library(dslabs)
data(polls_us_election_2016)

# Create an object `polls` that contains the spread of predictions for each candidate in Florida during the last polling days
polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Examine the `polls` object using the `head` function
head(polls)

# Create an object called `results` that has two columns containing the average spread (`avg`) and the standard error (`se`). Print the results to the console.
results <- polls %>% summarize(mean=mean(spread), se=sd(spread)/sqrt(n()))
results

#Exercise 8 - Estimate the Posterior Distribution
#The CLT tells us that our estimate of the spread d_hat has a normal 
#distribution with expected value d and standard deviation sigma, which
#we calculated in a previous exercise. Use the formulas for the 
#posterior distribution to calculate the expected value of the posterior
#distribution if we set mu=0 and tau=.01.

# The results` object has already been loaded. Examine the values stored: `avg` and `se` of the spread
results

# Define `mu` and `tau`
mu <- 0
tau <- 0.01

# Define a variable called `sigma` that contains the standard error in the object `results`
sigma <- results %>% pull(se)

# Define a variable called `Y` that contains the average in the object `results`
Y <- results %>% pull(avg)

# Define a variable `B` using `sigma` and `tau`. Print this value to the console.
B <- sigma^2/(sigma^2+tau^2)

# Calculate the expected value of the posterior distribution
B*mu+(1-B)*Y

##Exercise 9 - Standard Error of the Posterior Distribution
#Compute the standard error of the posterior distribution.

# Here are the variables we have defined
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

# Compute the standard error of the posterior distribution. Print this value to the console.
sqrt(1/(1/sigma^2+1/tau^2))

##Exercise 10- Constructing a Credible Interval
#Using the fact that the posterior distribution is normal, create an 
#interval that has a 95% of occurring centered at the posterior expected
#value. Note that we call these credible intervals.

# Here are the variables we have defined in previous exercises
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))

# Construct the 95% credible interval. Save the lower and then the upper
#confidence interval to a variable called `ci`.
ci <- c(B*mu+(1-B)*Y-qnorm(.975)*se, B*mu+(1-B)*Y+qnorm(.975)*se)

##Exercise 11 - Odds of Winning Florida
#According to this analysis, what was the probability that Trump wins 
#Florida?
# Assign the expected value of the posterior distribution to the variable `exp_value`
exp_value <- B*mu + (1-B)*Y 

# Assign the standard error of the posterior distribution to the variable `se`
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))

# Using the `pnorm` function, calculate the probability that the actual spread was less than 0 (in Trump's favor). Print this value to the console.
pnorm(0, exp_value, se)

##Exercise 12 - Change the Priors
#We had set the prior variance tau to 0.01, reflecting that these races 
#are often close.
#Change the prior variance to include values ranging from 0.005 to 0.05 
#and observe how the probability of Trump winning Florida changes by 
#making a plot.
# Define the variables from previous exercises
mu <- 0
sigma <- results$se
Y <- results$avg

# Define a variable `taus` as different values of tau
taus <- seq(0.005, 0.05, len = 100)

# Create a function called `p_calc` that generates `B` and calculates the probability of the spread being less than 0
p_calc <- function(tau){
  B <- sigma^2/(sigma^2+tau^2)
  pnorm(0, B*mu+(1-B)*Y, sqrt(1/(1/sigma^2 + 1/tau^2)))
}


# Create a vector called `ps` by applying the function `p_calc` across values in `taus`
ps <- p_calc(taus)

# Plot `taus` on the x-axis and `ps` on the y-axis
plot(taus, ps)

##Exercise 1 - Confidence Intervals of Polling Data
#For each poll in the polling data set, use the CLT to create a 95% 
#confidence interval for the spread. Create a new table called cis that 
#contains columns for the lower and upper limits of the confidence 
#intervals.

# Load the libraries and data
library(dplyr)
library(dslabs)
data("polls_us_election_2016")

# Create a table called `polls` that filters by  state, date, and reports the spread
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Create an object called `cis` that has the columns indicated in the instructions
cis <- polls %>% 
  mutate(X_hat=(spread +1)/2, se=2*sqrt(X_hat*(1-X_hat)/samplesize), lower=spread-qnorm(.975)*se, upper=spread+qnorm(.975)*se) %>% 
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)

##Exercise 2 - Compare to Actual Results
#You can add the final result to the cis table you just created using 
#the left_join function as shown in the sample code.
#Now determine how often the 95% confidence interval includes the actual
#result.

# Add the actual results to the `cis` data set
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of confidence intervals that contain the actual value. Print this object to the console.
p_hits <- ci_data %>% mutate(hit=ifelse(actual_spread>=lower & actual_spread<=upper, 1, 0)) %>%
  summarize(mean(hit))

##Exercise 3 - Stratify by Pollster and Grade
#Now find the proportion of hits for each pollster. Show only pollsters 
#with at least 5 polls and order them from best to worst. Show the number 
#of polls conducted by each pollster and the FiveThirtyEight grade of 
#each pollster.

# The `cis` data have already been loaded for you
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")
head(ci_data)
# Create an object called `p_hits` that summarizes the proportion of hits for each pollster that has at least 5 polls.
p_hits <- ci_data %>% group_by(pollster) %>% filter(n()>=5) %>% 
  mutate(hit=ifelse(actual_spread>=lower & actual_spread<=upper, 1, 0)) %>% 
  summarise(proportion_hits=mean(hit), n=n(), grade=grade[1]) %>%
  arrange(desc(proportion_hits))
head(p_hits)

##Exercise 4 - Stratify by State
#Repeat the previous exercise, but instead of pollster, stratify by 
#state. Here we can't show grades.

# The `cis` data have already been loaded for you
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of hits for each state that has more than 5 polls.
p_hits <- ci_data %>% group_by(state) %>% filter(n()>=5) %>% 
  mutate(hit=ifelse(actual_spread>=lower & actual_spread<=upper, 1, 0)) %>%
  summarise(proportion_hits=mean(hit), n=n()) %>% arrange(desc(proportion_hits))

##Exercise 5- Plotting Prediction Results
#Make a barplot based on the result from the previous exercise.

# The `p_hits` data have already been loaded for you. Use the `head` function to examine it.
head(p_hits)

# Make a barplot of the proportion of hits for each state
p_hits %>% mutate(state=reorder(state, proportion_hits)) %>% 
  ggplot(aes(state, proportion_hits)) + geom_bar(stat="identity") +
  coord_flip()

##Exercise 6 - Predicting the Winner
#Even if a forecaster's confidence interval is incorrect, the overall 
#predictions will do better if they correctly called the right winner.
#Add two columns to the cis table by computing, for each poll, the 
#difference between the predicted spread and the actual spread, and 
#define a column hit that is true if the signs are the same.

# The `cis` data have already been loaded. Examine it using the `head` function.
head(cis)

# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
errors <- cis %>% mutate(error=spread-actual_spread, hit=sign(spread)==sign(actual_spread))

# Examine the last 6 rows of `errors`
tail(errors)

##Exercise 7 - Plotting Prediction Results
#Create an object called p_hits that contains the proportion of instances
#when the sign of the actual spread matches the predicted spread for states
#with 5 or more polls.
#Make a barplot based on the result from the previous exercise that shows
#the proportion of times the sign of the spread matched the actual result
#for the data in p_hits.

# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
errors <- cis %>% mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))

# Create an object called `p_hits` that summarizes the proportion of hits for each state that has 5 or more polls
p_hits <- errors %>% group_by(state) %>% filter(n()>=5) %>% 
  summarise(proportion_hits=mean(hit), n=n())


# Make a barplot of the proportion of hits for each state
p_hits %>% ggplot(aes(state, proportion_hits)) + 
  geom_bar(stat="identity") + coord_flip()


##Exercise 8 - Plotting the Errors
#In the previous graph, we see that most states' polls predicted the correct 
#winner 100% of the time. Only a few states polls' were incorrect more than 
#25% of the time. Wisconsin got every single poll wrong. In Pennsylvania
#and Michigan, more than 90% of the polls had the signs wrong.
#Make a histogram of the errors. What is the median of these errors?

# The `errors` data have already been loaded. Examine them using the `head` function.
head(errors)

# Generate a histogram of the error
hist(errors$error)

# Calculate the median of the errors. Print this value to the console.
median(errors$error)

##Exercise 9- Plot Bias by State
#We see that, at the state level, the median error was slightly in favor
#of Clinton. The distribution is not centered at 0, but at 0.037. This 
#value represents the general bias we described in an earlier section.
#Create a boxplot to examine if the bias was general to all states or if
#it affected some states differently. Filter the data to include only pollsters 
#with grades B+ or higher.

# The `errors` data have already been loaded. Examine them using the `head` function.
head(errors)

# Create a boxplot showing the errors by state for polls with grades B+ or higher
errors %>% filter(grade %in% c("A+","A","A-","B+") | is.na(grade)) %>%
  mutate(state = reorder(state, error)) %>%
  ggplot(aes(state, error)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_boxplot() + 
  geom_point()


##Exercise 10 - Filter Error Plot
#Some of these states only have a few polls. Repeat the previous exercise
#to plot the errors for each state, but only include states with five good
#polls or more.

# The `errors` data have already been loaded. Examine them using the `head` function.
head(errors)

# Create a boxplot showing the errors by state for states with at least 5 polls with grades B+ or higher
errors %>% filter(grade %in% c("A+", "A", "A-", "B+") | is.na(grade)) %>% 
  group_by(state) %>% filter(n()>=5) %>% ungroup() %>% mutate(state=reorder(state, error)) %>%
  ggplot(aes(state, error)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_boxplot() + geom_point()

##Exercise 1 - Using the t-Distribution
#We know that, with a normal distribution, only 5% of values are more 
#than 2 standard deviations away from the mean.
#Calculate the probability of seeing t-distributed random variables being 
#more than 2 in absolute value when the degrees of freedom are 3.

?pt
# Calculate the probability of seeing t-distributed random variables being more than 2 in absolute value when 'df = 3'.
1-pt(2, df=3)+pt(-2, df=3)

##Exercise 2 - Plotting the t-distribution
#Now use sapply to compute the same probability for degrees of freedom 
#from 3 to 50.
#Make a plot and notice when this probability converges to the normal 
#distribution's 5%.

# Generate a vector 'df' that contains a sequence of numbers from 3 to 50
df <- seq(3, 50)
df
# Make a function called 'pt_func' that calculates the probability that a value is more than |2| for any degrees of freedom 
pt_func <- function(df){
  1-pt(2, df)+pt(-2,df)
}

# Generate a vector 'probs' that uses the `pt_func` function to calculate the probabilities
probs <- sapply(df, pt_func)
probs
# Plot 'df' on the x-axis and 'probs' on the y-axis
plot(df,probs)

##Exercise 3 - Sampling From the Normal Distribution
#In a previous section, we repeatedly took random samples of 50 heights 
#from a distribution of heights. We noticed that about 95% of the samples 
#had confidence intervals spanning the true population mean.
#Re-do this Monte Carlo simulation, but now instead of N=50 use N=15. 
#Notice what happens to the proportion of hits.

?replicate
?sample
??between

# Load the neccessary libraries and data
library(dslabs)
library(dplyr)
data(heights)

# Use the sample code to generate 'x', a vector of male heights
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Create variables for the mean height 'mu', the sample size 'N', and the number of times the simulation should run 'B'
mu <- mean(x)
N <- 15
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate a logical vector 'res' that contains the results of the simulations
res <- replicate(B, {
  x <- sample(x, N, replace=T)
  interval_l <- mean(x)-qnorm(.975)*(sd(x)/sqrt(N))
  interval_u <- mean(x)+qnorm(.975)*(sd(x)/sqrt(N))
  between(mu, interval_l, interval_u)
})

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
mean(res)

##Exercise 4 - Sampling from the t-Distribution

#N=15 is not that big. We know that heights are normally distributed, so 
#the t-distribution should apply. Repeat the previous Monte Carlo simulation
#using the t-distribution instead of using the normal distribution to 
#construct the confidence intervals.
#What are the proportion of 95% confidence intervals that span the actual
#mean height now?

# The vector of filtered heights 'x' has already been loaded for you. Calculate the mean.
mu <- mean(x)

# Use the same sampling parameters as in the previous exercise.
set.seed(1)
N <- 15
B <- 10000

# Generate a logical vector 'res' that contains the results of the simulations using the t-distribution
res <- replicate(B, {
  x <- sample(x, N, replace=T)
  interval_l <- mean(x)-qt(.975, N-1)*sd(x)/sqrt(N)
  interval_u <- mean(x)+qt(.975, N-1)*sd(x)/sqrt(N)
  between(mu, interval_l, interval_u)
})

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
mean(res)

##Exercise 1 - Comparing Proportions of Hits
#In a previous exercise, we determined whether or not each poll predicted 
#the correct winner for their state in the 2016 U.S. presidential election. 
#Each poll was also assigned a grade by the poll aggregator. Now we're 
#going to determine if polls rated A- made better predictions than polls
#rated C-.
#In this exercise, filter the errors data for just polls with grades A-
#and C-. Calculate the proportion of times each grade of poll predicted
#the correct winner.

??spread
# The 'errors' data have already been loaded. Examine them using the `head` function.
head(errors)

# Generate an object called 'totals' that contains the numbers of good and bad predictions for polls rated A- and C-
totals <- errors %>% filter(grade %in% c("A-", "C-")) %>% group_by(grade, hit) %>% summarise(num=n()) %>% spread(grade, num)



# Print the proportion of hits for grade A- polls to the console
totals[[2,3]]/sum(totals[3])

# Print the proportion of hits for grade C- polls to the console
totals[[2,2]]/sum(totals[2])

##Exercise 2 - Chi-squared Test
#We found that the A- polls predicted the correct winner about 80% of the
#time in their states and C- polls predicted the correct winner about 86% 
#of the time.
#Use a chi-squared test to determine if these proportions are different.

# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals)

# Perform a chi-squared test on the hit data. Save the results as an object called 'chisq_test'.
chisq_test <- totals %>% select(-hit) %>% chisq.test()

chisq_test



# Print the p-value of the chi-squared test to the console
chisq_test$p.value

##Exercise 3 - Odds Ratio Calculation
#It doesn't look like the grade A- polls performed significantly differently
#than the grade C- polls in their states.
#Calculate the odds ratio to determine the magnitude of the difference
#in performance between these two grades of polls.

# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals)

# Generate a variable called `odds_C` that contains the odds of getting the prediction right for grade C- polls
odds_C <- (totals[[2,2]] / sum(totals[[2]])) / 
  (totals[[1,2]] / sum(totals[[2]]))



# Generate a variable called `odds_A` that contains the odds of getting the prediction right for grade A- polls
odds_A <- (totals[[2,3]] / sum(totals[[3]])) / 
  (totals[[1,3]] / sum(totals[[3]]))



# Calculate the odds ratio to determine how many times larger the odds ratio is for grade A- polls than grade C- polls
odds_A/odds_C
