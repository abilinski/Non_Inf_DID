###################################### SETUP ###################################### 
# Content generated: Table 2

here::i_am("1_Simulations/1_Scripts/1_synthetic_simulations.R")
source("global_options.R")

# source NI tests
source("0_Functions/matrix_functions.R")

###################################### MAKE DATA ######################################

# power for a treatment effect
get_tx_effect_power = function(tx_eff, i, df, n = 50000){
  
  out = foreach(j=1:n, .combine=function(x,y)rbindlist(list(x,y)))%dopar% {
    
    # make data
    d = make_data_up(n1 = df$n1[i], n2 = df$n2[i], 
                     periods = df$npre[i] + df$npost[i], 
                     vio.linear.slope = df$vio.linear.slope[i],
                     post.time = df$npre[i]+ 1, trt = tx_eff, 
                     vio.type = df$vio.type[i], sigma2 = df$sigma2[i], 
                     hetero = df$hetero[i], autocorr = df$autocorr[i]) %>% 
      mutate(Group = group=="Treatment",
             interaction = ifelse(group=="Treatment", year, 0),
             trtpost = as.numeric(trtpost))
    
    # evaluate if a go
    robust = run_NI_test(data = d, form1 = outcome~trtpost + factor(unit.ID) + factor(factoryear), 
                         form2 = outcome~trtpost + factor(unit.ID) + factor(factoryear),
                         lincom_vars = 1,
                         robust = T, cluster = "unit.ID", weight = F,
                         null_reduced = F, return_all = F, z = qnorm(.975))
    
    return(data.table(sig = abs(robust[[8]])>qnorm(.975)))
  }
  
  # return values
  return(mean(out$sig))
  
}

###################################### RUN TESTS ###################################### 

# Make data frame with basic simulations
df = expand_grid(
  n1 = c(30), n2 = c(30),
  npre = c(20), npost = c(5, 10), sigma2 = c(5), vio.type = "None", pwr = c(.8), hetero = c(T), autocorr = c(T, F))

# Check power for different treatment effects
#get_tx_effect_power(.986, 1, df)
#get_tx_effect_power(.827, 2, df)
#get_tx_effect_power(.775, 3, df)
#get_tx_effect_power(.635, 4, df)
df$tx_eff = c(.986, .827, .775, .635)

####************************** RUN SIMULATIONS **************************####

# set parameters
set.seed(2138)
trials = 1000
rerun = F

if(rerun==T){
  # add in violations
  df = df %>% dplyr::select(-vio.type) %>% 
     expand_grid(model = c("Linear", "Event study"),
                vio.type = c("None", "Linear")) %>%
    mutate(
      tx.eff = tx_eff,
      vio.linear.slope_cat = ifelse(vio.type=="None", 0, 0.5), # calibrate violation to induce bias = 50% of tx effect (see Prop 1)
      chk = sapply(1:n(), function(a) mean((npre[a]+1):(npre[a]+npost[a])) - mean(1:npre[a])),
      val = tx_eff/chk,
      vio.linear.slope = vio.linear.slope_cat*val, id = row_number()) %>% 
    filter(npost==5)
  
  # store results
  res_list = list()
  
  # run simulations
  for(i in 1:nrow(df)){
    tic()
  out = foreach(j=1:trials, .combine=function(x,y)rbindlist(list(x,y)))%dorng% {
    
  
    if(df$model[i]=="Linear"){
      form2_val= outcome~trtpost + factor(unit.ID) + factor(factoryear) + interaction
    }else{form2_val = outcome~relevel(factor(event_study), ref = "0") + factor(unit.ID) + factor(factoryear)}
    
    # make data
    d = make_data_up(n1 = df$n1[i], n2 = df$n2[i], 
                     periods = df$npre[i] + df$npost[i], 
                     vio.linear.slope = df$vio.linear.slope[i],
                     post.time = df$npre[i]+ 1, trt = df$tx_eff[i], 
                     vio.type = df$vio.type[i], sigma2 =df$sigma2[i], 
                     hetero = df$hetero[i], autocorr = df$autocorr[i]) %>% 
      mutate(Group = group=="Treatment",
             interaction = ifelse(group=="Treatment", year, 0),
             last_pre = max(year[post==FALSE]),
             trtpost = factor(ifelse(group=="Treatment" & post, year-last_pre, 0)),
             ind = last_pre-year,
             event_study = ifelse(Group, ind, 0))
  
      # run linear model
     robust = run_NI_test(data = d, form1 = outcome~trtpost + factor(unit.ID) + factor(factoryear), 
                           form2 = form2_val,
                           lincom_vars = 1:df$npost[i],
                           robust = T, cluster = "unit.ID", 
                           weight = F,
                           null_reduced = F, return_all = F, z = qnorm(.975))
  
    
     # return outputs
     return(data.table(diff = robust[[1]], se = robust[[2]][[1]], ci_l = robust[[3]][1], ci_u = robust[[3]][2], tx_r = robust[[4]], tx_e = robust[[5]],
                       var_r = robust[[6]], var_e = robust[[7]], t_stat_r = robust[[8]], t_stat_e = robust[[9]], cov_chk = robust[[10]]))
     
    }
    res_list[[i]] = out[, id := i]
    toc()
  }
  
  # store and save output  
  res2 = rbindlist(res_list) %>% left_join(df %>% mutate(i = row_number()), c("id" = "i"))
  save(res2, file = here("1_Simulations", "2_Output", "synthetic_sims_2025.RData"))
}

###################################### DISPLAY RESULTS ###################################### 

# load in the data
load(here("1_Simulations", "2_Output", "synthetic_sims_2025.RData"))

# calculate summary statistics
summ_stats = res2 %>% 
  expand_grid(rule_out = c(.2, 1)) %>%
  mutate(se = se,
         ci_u = diff + se*qnorm(.95),       # threshold chosen for NI tests
         ci_l = diff - se*qnorm(.95),
         rule_out_val = rule_out*tx_eff) %>%
  group_by(autocorr, model, vio.linear.slope_cat, rule_out) %>%
  summarize(n = n(), 
            
            # calculate power
            pwr_non_inf_linear = round(mean(ci_u < rule_out_val)*100),
            pwr_non_inf_linear_two_sided = round(mean(ci_l > -1*rule_out_val & ci_u < rule_out_val)*100),
            pwr_reduced = round(mean(abs(t_stat_r.V1)>qnorm(.975))*100),
            pwr_expanded = round(mean(abs(t_stat_e.V1)>qnorm(.975))*100),

            # check n
            n_non_inf_linear = sum(ci_u < rule_out_val),

            # check bias and conditional bias
            att_r = round((mean(tx_r)-mean(tx_eff))/mean(tx_eff)*100),
            att_r_cond = ifelse(n_non_inf_linear < 500, NA, round(mean(tx_r[ci_u < rule_out_val])/mean(tx_eff)*100 - (mean(tx_r)-mean(tx_eff))/mean(tx_eff)*100 - 100)),
            
            att_e = round((mean(tx_e)-mean(tx_eff))/mean(tx_eff)*100),
            att_e_cond = ifelse(n_non_inf_linear < 500, NA, round(mean(tx_e[ci_u < rule_out_val])/mean(tx_eff)*100-(mean(tx_e)-mean(tx_eff))/mean(tx_eff)*100-100)),
            att_e_cond2 = ifelse(n-n_non_inf_linear < 500, NA, round(mean(tx_e[ci_u > rule_out_val])/mean(tx_eff)*100-(mean(tx_e)-mean(tx_eff))/mean(tx_eff)*100-100))
            
  ) %>% mutate(vio.linear.slope_cat = vio.linear.slope_cat*100, rule_out = rule_out*100)

# View output
View(summ_stats  %>% arrange(autocorr, desc(model), vio.linear.slope_cat, -rule_out, -n_non_inf_linear))

# Generate Table 2
kable(summ_stats %>% ungroup() %>% arrange(autocorr, desc(model), vio.linear.slope_cat, -rule_out) %>% dplyr::select(-n_non_inf_linear, -n),
      format = "latex")


