#################################### GLOBAL OPTIONS #################################### 
#### LIBRARIES ####
library(truncnorm)
library(tictoc)
library(sandwich)
library(matrixStats)
library(data.table)
library(lmtest)
library(fixest)
library(here)
library(knitr)
library(microbenchmark)
library(Rfast)          # fast matrix inversion
library(diffudist)      # easy way to use Rcpp for matrix mult: eigenMatMult, eigenMapMatMult #Ack!: this has been de-listed from CRAN
#library(RcppEigen)              # another way to get those matrix multiplication function
library(collapse)       # fast matrix subsetting: fsubset
library(testthat)
library(lessR)
library(lme4)
library(umx)
library(multcomp)
library(labelled)
library(tidyverse)
library(doRNG)

# parallelization -- only for Unix-based systems
if (Sys.info()["sysname"] != "Windows") {
  library(doMC) # Only for UNIX operating systems (e.g., macOS)
  doMC::registerDoMC(cores = detectCores())
  foreach::getDoParWorkers()
}


# Function specifications
# n1 - number of treated units
# n2 - number of comparison units
# periods - number of periods
# post.time - time post-intervention period starts
# trt - treated effect for treated units in post-intervention period
# vio.type - type of violation of parallel trends, "None", "Linear", "Sudden jump"
# vio.linear.slope - slope of linear violation if vio.type = "Linear"
# vio.sudden.jump.per - period of sudden jump if vio.type = "Sudden jump"
# vio.sudden.jump - size of sudden jump if vio.type = "Sudden jump"
# sigma2 - variance of error term
# hetero - if T, heteroskedasticity by unit is present
# autocorr - if T, autocorrelation is present
# simulate data
make_data_up = function(n1, n2, periods, post.time, trt = 2, 
                        vio.type = "None", vio.linear.slope = .5, 
                        vio.sudden.jump.per = NA, vio.sudden.jump = 2, 
                        sigma2=1, hetero = F, autocorr = F) {
  
  if(post.time > periods || post.time < 3) return("The intervention must start within the specified period (post.time <= periods) and no earlier than time 3.")
  
  ## set up data frame
  n = n1 + n2
  states = rep(1:n, each = periods)
  year = rep(1:periods, times = n)
  
  # state FEs
  state_err = rep(rnorm(n), each = periods)
  
  # clustered SEs
  sigma2_cluster = rep(rnorm(n, mean = sigma2, sd = sqrt(.33*sigma2)), each = periods)
  sigma2_cluster[sigma2_cluster<0] = .1
  
  ## sudden jump violations
  sj.vio = ifelse(is.na(vio.sudden.jump.per), sample(2:(post.time-1), 1),
                  vio.sudden.jump.per)
  
  # preserve values
  hetero_flag <- hetero
  sigma2_base <- sigma2
  
  # put together in data frame
  temp.dat = data.frame(unit.ID = states, year, state_err, vio.type, hetero, sigma2_cluster, autocorr) %>%           
    mutate(group=ifelse(unit.ID%in%(1:as.numeric(n1)),"Treatment","Control"),
           unit.ID=factor(unit.ID),
           post=(year>=post.time),
           factoryear=factor(year),
           trtpost=post&(group=="Treatment"),
           
           # set violations
           vio = ifelse(vio.type=="Linear", # set linear violation
                        (group=="Treatment")*(year*vio.linear.slope), 0), 
           vio = ifelse(vio.type=="Sudden jump", # set monotonic violation
                        (group=="Treatment")*(year>=sj.vio)*vio.sudden.jump,
                        vio),
           
           # error setup
           sigma2 = ifelse(hetero, sigma2_cluster, sigma2),
           
           # errors (could be cleaner, but it works)
           indiv.error = ifelse(
             autocorr,
             as.vector(sapply(1:n, function(a) {
               
               # Use unit-specific variance only when hetero == TRUE
               unit_sigma <- if (hetero_flag) {
                 sigma2_cluster[(a - 1) * periods + 1]
               } else {
                 sigma2_base
               }
               
               arima.sim(
                 model = list(ar = c(.2)),
                 n = periods,
                 rand.gen = function(n) rnorm(n, sd = sqrt(unit_sigma))
               )
             })),
             rnorm(length(states), sd = sqrt(sigma2))
           ),
           # create outcome
           outcome = trt*trtpost + vio + state_err + indiv.error)
  
  # return data frame
  return(temp.dat)
  
}
