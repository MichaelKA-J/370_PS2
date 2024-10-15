#-------------------------------------------------------------------------------
# Drew Van Kuiken
# 09.24.2024
# PS2 Submission Script Shell
# Goal: Provide an example script that students can use to submit their homework. 
# Demonstrate how to write a script in general. 
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Prep 
#-------------------------------------------------------------------------------
# functions
## --- First-Price Sealed Bid Optimal Bidding Function

set.seed(123)

# arguments are v (valuations) and n (number of bidders)
# note that v CAN be a vector of length greater than 1
b_i = function(v,n,Fv=function(x){pexp(x,rate=1/10)},
               xs=seq(0,max(v),0.00001),...){
  
  f = function(x){Fv(x,...)^(n-1)} # define function to integrate
  
  ## ---- set up objects needed for integration
  xs  = sort(xs)   # guarantee x grid is sorted
  dxs = diff(xs)   # create dxs 
  xN  = length(xs) # set number of points in x grid
  fx  = f(xs)      # calculate f(x) for all xs in grid
  
  ## ---- estimate F(x) = int_{x0}^{x} f(t) dt
  Fxs = (fx[1:(xN-1)]+fx[2:xN]) # create the terms in the trap rule
  Fxs = cumsum(Fxs*dxs/2)       # sum up each value of integral
  Fxs = c(0,Fxs)                # add that int_{x0}^{x0} f(t) dt is 0
  
  ## ---- interpolate F(x) and create numerator 
  xids  = findInterval(v,xs)             # get ids for which interval v is in 
  alpha = (v - xs[xids])/dxs[xids]       # weights between interval points
  numer = Fxs[xids]*alpha                # weight F(x) on the LHS
  numer = numer + Fxs[xids+1]*(1-alpha)  # weight F(x) on the RHS
  numer[is.na(numer)] = Fxs[length(Fxs)] # fix points out of bounds
  
  denom    = f(v)         # create denominator 
  bidshave = numer/denom  # create bid shave 
  
  # if the denominator is essentially 0, return v, otherwise shave bid
  ifelse(denom < .Machine$double.eps,v,v - bidshave)
}
# note: normally, we would store all of the functions we need in a separate 
# script (in this case, ps2_funcs.R). we could then use the source command to 
# run that script from top to bottom. that way, we would have the functions saved
# in our global environment. since this homework is designed around writing functions,
# it will be easier for the grader if you don't use the source framework here. 
# for this homework, include functions you write in response to a question in the 
# body of your script, and not up here

# load packages
install.packages("readxl")
library(readxl)

# load datasets
HT_data = read_excel("/cloud/project/370Homework/HTdata_labels.xlsx")

#-------------------------------------------------------------------------------
# Logic
#-------------------------------------------------------------------------------

## Subsetting Data

# 1
HT_data_Zone2 = HT_data[HT_data$`Zone 2`==1, ]
'
Subset from HTdata,
all observations where "Zone 2" = 1,
include all columns
*Backticks must be used to extract a column with spaces in the name
'

# 2
HT_data_Delta45 = HT_data[HT_data$`Delta`>=0.04 & HT_data$`Delta`<=0.05, ]
'
Subset from HTdata,
all observations where Delta is between 0.04 and 0.05,
include all columns
'

# 3
HT_data_Con = HT_data[HT_data$`Species Concentration`>0.5, ]

# 4
HT_data_Bidders23 = HT_data[HT_data$`Bidders`==2 | HT_data$`Bidders`==3, ]
'
Subset from HTdata,
all observations with 2 bidders or 3 bidders,
include all columns
'

# 5
HT_data_4Bidders1982 = HT_data[HT_data$`Bidders`==4 & HT_data$`Year`==82, ] 

## If Statements and Functions

# 1
my_abs = function(x) {
  x = (x^2)^0.5
  return(x)
}
my_abs(3)

# 2
my_abs_vec = function(x) {
  x = (x^2)^0.5
  return(x)
}

x = c(-5:5)
my_abs(x)

#3
my_sign = function(x) {
  if (x>0) {
    x = 1
  } 
  else if (x<0) {
    x = -1
  }
  else {
    x = 0
  }
  return(x)
}

my_sign(-3)
my_sign(3)
my_sign(0)

#4
my_sign_vec = function(x) {
  result = vapply(x, my_sign, FUN.VALUE = numeric(1))
  return(result)
}

x = c(-4:4)
my_sign_vec(x)

#5
CRRA = function(c, eta) {
  first_op = function(c, eta) {
    first_op_result = (c^(1 - eta) - 1) / (1 - eta)
    return(first_op_result)
  }
  
  if ((eta >= 0) & (eta != 1)) {
    result = vapply(c, first_op, eta = eta, FUN.VALUE = numeric(1))
  } else if (eta == 1) {
    result = log(c)
  }
  
  return(result)
}

c = c(1:5)
eta = 6
CRRA(c,eta)

#6
my_funct = function(x) {
  ifelse(x<0, x^2 + 2*x + abs(x), 
         ifelse(x>=2, x^2 + 4*x - 14, 
                x^2 + 3 + log(x+1)))
}

#7
my_mat = matrix(c(rnorm(20,0,10), rnorm(20,-1,10)), nrow=20, ncol=2)

mean_rows = apply(my_mat, 1, mean) 
median_rows = apply(my_mat, 1, median)
min_rows = apply(my_mat, 1, min)
max_rows = apply(my_mat, 1, max)
sd_rows = apply(my_mat, 1, sd)

mean_cols = apply(my_mat, 2, mean)
median_cols = apply(my_mat, 2, median)
min_cols = apply(my_mat, 2, min)
max_cols = apply(my_mat, 2, max)
sd_cols = apply(my_mat, 2, sd)

#-------------------------------------------------------------------------------
# Wage Gap
#-------------------------------------------------------------------------------

#1
#What is the data generating process?
'
My process to simulate the population data is as follows:

Define the population size N (10,000).

Create a population of size N with roughly half male, half female.
This is done using the sample function, sampling 10000 times from a 
vector with two genders with 50/50 probability.

Assign jobs (high-pay/low-pay) to each member of the population,
with males having a 50% chance of high-pay jobs, 
and females having a 25% of high-pay jobs.
Using ifelse with two sample functions is used to randomly assign jobs with 
appropriate probabilities for each gender.

Assign wages to individuals based on their jobs-
individuals with low-pay jobs have a wage of $10
and individuals with high-pay jobs have a wage of $15.
Ifelse is used to assign the correct wage based on job type.

Combine the simulated gender, jobs, and wages into a dataframe
so that there are 10,000 rows, 1 for each individual,
and three columns for gender, job, and wages.
'

#What are the data?
'
We are trying to simulate a dataset of information on 
the gender, job, and wage of 10,000 random individuals.
The simulated data are stored in a dataframe with the following variables:
Gender is a categorical variable, either male or female.
Job is also a categoriacl variable, either high-paying or low-paying.
Wage is a numeric variable, that can either be 10 or 15.
What this will look like is 10,000 observations, 
with each row representing one individual,
and columns for gender, job, and wage respectively.

Additionally, we will simulate this dataset 10,000 times, 
and for each rep, we will run three linear regressions:
1. wage on gender
2. wage on job
3. wage on both job and gender.
For each rep, we will store the estimated coefficients as in matrices.
Finally, we will find the average gender coefficient from the regressions
of wage on gender and the regressions of wage on both job and gender.
'

#Define Parameters
Nrep = 10000
low_wage = 10
high_wage = 15

#Preallocation
'
Preallocate empty matrices to store the 
coefficients for each regression
'
reg_1_mat = matrix(0, nrow = 10000, ncol = 2)
reg_2_mat = matrix(0, nrow = 10000, ncol = 2)
reg_3_mat = matrix(0, nrow = 10000, ncol = 3)

#Data Generation
sim_dem = function() {
  #simulate the population once
  N = 10000
  
  #use sample to simulate the randomness of the explanatory variable
  genders = sample(c("male", "female"), size = N, replace = TRUE, prob = c(0.5, 0.5))
  
  #Determining Jobs
  #size must be sum(genders=="male")
  '
  Ifelse does not apply iteratively, so setting size=1
  will result in every element equal to male being replaced
  by that single random value.
  In this case,
  ifelse selects all elements equal to male,
  then replaces them with a series of random values.
  '
  jobs = ifelse(
    genders == "male",
    sample(c("low", "high"), size = sum(genders=="male"), replace = TRUE, prob = c(0.5, 0.5)),
    sample(c("low", "high"), size = sum(genders=="female"), replace = TRUE, prob = c(0.75, 0.25))
  )
  
  wages = ifelse(jobs == "low", low_wage, high_wage)
  
  wage_sim_data = data.frame(
    gender = genders,
    job = jobs,
    wage = wages,
    stringsAsFactors = FALSE
  )
  return(wage_sim_data)
}


#Monte Carlo Loop
'
simulate date and estimate coefficients 10,000 times
store the estimates from each iteration
'
for(rep in 1:Nrep) {
  sim_dem_rep = sim_dem() #Generate Artificial Data
  
  coef_reg_1 = coef(lm(wage ~ gender, sim_dem_rep))
  coef_reg_2 = coef(lm(wage ~ job, sim_dem_rep))
  coef_reg_3 = coef(lm(wage ~ gender + job, sim_dem_rep))
  
  reg_1_mat[rep, ] = coef_reg_1[1:2]
  reg_2_mat[rep, ] = coef_reg_2[1:2]
  reg_3_mat[rep, ] = coef_reg_3[1:3]
}

#Checking my work
head(reg_1_mat)
head(reg_2_mat)
head(reg_3_mat)

#3

#Look at the mean of the coefficients in each regression across models
#average intercept for regressions of wage on gender
mean(reg_1_mat[, 1])
#average gender coefficient for regressions of wage on gender
mean(reg_1_mat[, 2])

#average intercept for regressions of wage on job
mean(reg_2_mat[, 1])
#average gender coefficient for regressions of wage on job
mean(reg_2_mat[, 2])

#average intercept for regressions of wage on gender and job
mean(reg_3_mat[, 1])
#average gender coefficient for regressions of wage on gender and job
mean(reg_3_mat[, 2])
#average job coefficient for regressions of wage on gender and job
mean(reg_3_mat[, 3])

#Comparing the mean of the sex coefficients in the two models that control for sex
mean_gender_coef_model_1 = mean(reg_1_mat[, 2])
mean_gender_coef_model_3 = mean(reg_3_mat[, 2])
mean_gender_coef_model_1
mean_gender_coef_model_3

#What does this tell us about controlling for occupation in this model?
'
The mean of the gender coefficient across reps is about 1.25
but the mean of the gender coefficient across reps is close to 0
when we control for high/low paying occupation.
This suggests that differences in wages observed between men and women
are mostly due to the fact that men and women hold different paying jobs
'

#What kind of bias does adding controls for occupation give rise to?
'
Adding controls for occupations reveals that there was in fact,
omitted variables bias in the initial regression of wage on gender.
This is ommitted variables bias 
because we left out an important factor (whether a job is high paying)
that is correlated with both 
the outcome (wages) and the explanatory variable (gender)
'  
#-------------------------------------------------------------------------------
# Auction Simulation
#-------------------------------------------------------------------------------

