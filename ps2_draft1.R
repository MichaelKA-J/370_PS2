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

#-------------------------------------------------------------------------------
# Auction Simulation
#-------------------------------------------------------------------------------

