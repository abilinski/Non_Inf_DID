###################################### SETUP ###################################### 
# Content generated: Table 4

here::i_am("1_Simulations/1_Scripts/2_empirical_simulations.R")

# source global options
source("global_options.R")
library(modi) # for weighted variance

# source NI tests
source("0_Functions/matrix_functions.R")

###################################### DATA ANALYSIS ###################################### 
set.seed(2138)

# data
load(here("1_Simulations", "0_Data", "regression_data.RData"))

# explanatory variables: from Laura
exp_vars = c("factor(age)","factor(year)","factor(month)","trend","factor(fipstate)",
             "female","hispanic", "white", "asian", "other", "mar", "student",
             "fpl_ratio", "fpl_ratio_2", "ue", "ue_treat")

# rename violation trend
dat.for.reg = haven::as_factor(dat.for.reg) %>% mutate(vio.trend = fedelig*trend,
                                     pre.month = factor(ifelse(month.index<15 & fedelig, month.index, 0)),
                                     fipstate = as.numeric(fipstate))

# make expanded model formula
form_e = as.formula(paste("anyhi", "~", paste(c("fedelig",
                                              "enact.trt.month","impl.trt.month", "vio.trend",
                                              "factor(trend)","factor(fipstate)",
                                              paste0("age",c(17,18,20:25,27:29)),
                                              exp_vars[c(6:15)] # omitted ue_treat per note in main text
                                              ),collapse = " + ")))

# make expanded model formula for simulation
form_e_sim = as.formula(paste("preds_now", "~", paste(c("fedelig",
                                                "enact.trt.month","impl.trt.month", 
                                                "factor(trend)","factor(fipstate)","vio.trend",
                                                paste0("age",c(17,18,20:25,27:29)), 
                                                exp_vars[c(6:15)]  # omitted ue_treat per note in main text
),collapse = " + ")))

# make reduced model for simulation
form_r_sim = as.formula(paste("preds_now", "~", paste(c("fedelig",
                                              "enact.trt.month","impl.trt.month",
                                              "factor(trend)","factor(fipstate)",
                                              paste0("age",c(17,18,20:25,27:29)),
                                              exp_vars[c(6:15)]   # omitted ue_treat per note in main text
                                              ),collapse = " + ")))

# run regression
out = feols(form_e, data = dat.for.reg, weights = ~weight)

# extract effect indicators
vars = which(grepl("impl", names(out$coefficients)))-1
true_val = mean(out$coefficients[vars+1])
true_val

# extract residuals
dat.for.reg$resid = out$residuals
dat.for.reg$pred =  predict(out, newdata = dat.for.reg %>% mutate(vio.trend = 0))
                                   
# put in data frame & calculate group variance
d = dat.for.reg %>% mutate(resid_var_all = weighted.var(resid, w = weight)) %>%
  group_by(fipstate) %>% mutate(resid_var2 = weighted.var(resid, w = weight))

# set up parameters for simulation
trial = expand_grid(vio = c(0, .011)/24, norm = c(F, T)) %>% mutate(i = row_number())
trials = 15000
rerun = F

###################################### SIMULATIONS ###################################### 

if(rerun==T){
  # data table to store output
  keep = data.table()
  
  # unique units
  g = unique(d$fipstate)
  
  #trial = trial %>% filter(!norm)
  
  # run nrep times
  for(i in 1:nrow(trial)){
    tic()
    res = foreach(j=1:trials, .combine=function(x,y)rbindlist(list(x,y)))%dorng% {
    
      # normal-based imputation
      if(trial$norm[i]){
        
        # rename
        d_run = d
        
        # check errors
        #d_run$res = rnorm(nrow(d_run), sd = sqrt(d_run$resid_var2))
        #d_run %>% group_by(fipstate) %>% summarize(sd(res), sqrt(resid_var2[1]))
        
        # heteroskedastic errors
        # uses cluster-level residual SE
        d_run$preds_now = d_run$pred + rnorm(nrow(d_run), sd = sqrt(d_run$resid_var2)) + trial$vio[i]*d_run$vio.trend
       
        
        }else{
        # select clusters
        clusters = sample(g, length(g), replace = T)
        
        # find units
        these = c()
        new_id = c()
        for(z in 1:length(clusters)){
          k =  which(d$fipstate == clusters[z])
          these = c(these, k)
          new_id = c(new_id, rep(z, length(k)))
        }
  
        # select clusters
        d_run = d[these,]
        
        # rename units
        d_run$fipstate = new_id
        d_run = d_run %>% group_by(fipstate) %>%
          mutate(err_sgn_var = rbinom(n(), prob = .5, size = 1),  # draw binomial RV
                 err_sgn = ifelse(err_sgn_var[1], -1, 1),         # use sign of the first for the whole cluster (hacky but fine)
                 preds_now = pred + err_sgn*resid + trial$vio[i]*vio.trend)
        }
      
      # run model
      run = run_NI_test(data = d_run, form1 = form_r_sim, form2 = form_e_sim, lincom_vars = vars,
                         robust = T, cluster = "fipstate", weight = T, z = qnorm(.95))
      
      # return output
      return_vars = data.table(id = ifelse(trial$norm[i], "Heteroskedastic normal", "Wild cluster-resampling bootstrap"),
                                           theta = trial$vio[i], diff = run[[1]], ci_l = run[[3]][1], ci_u = run[[3]][2], 
                               tx_r = run[[4]], tx_e = run[[5]], t_stat_r = run[[8]], t_stat_e = run[[9]])
      
  
      return(return_vars)
    }
    keep = rbindlist(list(res, keep), fill = T)
    toc()
  }
  
  # save file
  save(keep, file = here("1_Simulations/2_Output", "empirical_sims_2025.RData"))
}

# load file
load(here("1_Simulations/2_Output","empirical_sims_2025.RData"))

# summarize results
tbl = bind_rows(keep) %>%
  group_by(theta*24, id) %>% 
  mutate(n = n()) %>%
  summarize(mean(n), 
            
  # power
  pwr5_2sided = round(mean(ci_l > -0.05 & ci_u < .05)*100),
  pwr2 = round(mean(ci_u < .021)*100),
  #pwr2_2sided = round(mean(ci_l > -0.021 & ci_u < .021)*100),
  pwr1.1 = round(mean(ci_u < .01)*100),

  # treatment effects - reduced
  att_r = round(mean(tx_r)*100, 1),

  # conditional on passing
  tx_r_mean = round((mean(tx_r)-true_val)/true_val*100),
  tx_r5_2side_mean = round(100*mean(tx_r[ci_l > -0.05 & ci_u < .05])/true_val-100*(mean(tx_r)-true_val)/true_val)-100,
  tx_r2_mean = round(100*mean(tx_r[ci_u < .021])/true_val-100*(mean(tx_r)-true_val)/true_val)-100,  
  tx_r1_mean = round(100*mean(tx_r[ci_u < .01])/true_val-100*(mean(tx_r)-true_val)/true_val)-100,  

  # treatment effects - expanded
  att_e = round(mean(tx_e)*100, 1),

  # conditional on passing
  tx_e_mean =  round((mean(tx_e)-true_val)/true_val*100),
  tx_e5_2side_mean =  round(100*mean(tx_e[ci_l > -0.05 & ci_u < .05])/true_val)-100,
  tx_e2_mean = round(100*mean(tx_e[ci_u < .021])/true_val)-100,
  tx_e1_mean = round(100*mean(tx_e[ci_u < .01])/true_val)-100, 
  
  # conditional on failing
  tx_e5_2side_mean2 =  round(100*mean(tx_e[!(ci_l > -0.05 & ci_u < .05)])/true_val)-100,
  tx_e2_mean2 = round(100*mean(tx_e[!(ci_u < .021)])/true_val)-100,
  tx_e1_mean2 = round(100*mean(tx_e[!(ci_u < .01)])/true_val)-100, 
)

# Generate Table 4
t(tbl)
kable(t(tbl), format = "latex")
