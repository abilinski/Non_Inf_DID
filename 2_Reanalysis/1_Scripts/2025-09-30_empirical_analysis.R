# Analyses of the ACA dependent coverage provisions using SIPP data, USING MODIFIED PAPER SPECIFICATIONS
# By: Laura A. Hatfield
# Sept 30, 2025
# Replication of re-analyses in:
#   Hatfield LA and Bilinski A. (2025)
#   Nothing to See Here? A non-inferiority approach to evaluation of parallel trends.
#   Statistics in Medicine.
# Where are a re-analysis of: 
#   Akosa Antwi, Y., Moriya, A. S., & Simon, K. (2013). 
#   Effects of Federal Policy to Insure Young Adults: Evidence from the 2010 Affordable Care Act's Dependent-Coverage Mandate. 
#   American Economic Journal: Economic Policy, 5(4), 1-28. 
# Inputs: 
#   1) 0_source_files/augmented.dta
#   OR
#   2) 2_Output/dependent_coverage.RData
#   AND OPTIONALLY
# Outputs: 
#   1) "YYYY-MM-DD_model_fits.RData"
# Prerequisites: 
#   1) open ../Public.Rproj first (to ensure directory structure works)
#   2) install NonInfParTren package from (use devtools)
here::i_am("2_Reanalysis/1_Scripts/2025-09-30_empirical_analysis.R")
#devtools::install_github("laura-hatfield/NonInfParTren")
rm(list = ls()); gc()
library(NonInfParTren) # Note: uses online package, not local functions. You will need to reset environment if you have run simulation scripts.
library(tidyverse)
theme_set(theme_minimal())

## Set this tp FALSE to use already processed data files
recreate.data <- T
## Set this to FALSE to use already fit model results
rerun.models <- T

## Names of variables for model construction
# insurance types
instype = c('anyhi','emphi_dep','emphi','indiv','govhi')
inslabels = c('Any','Dependent','Employer','Individual','Government')

# First post-period: months from enactment to just before implementation (March 2010 to Sept 2010)
# Second post-period: implementation to the end of the study (Oct 2010 to Nov 2011)
# Fedelig : age>=19 and age<26
# elig_mar10 & elig_oct10 : interactions of post-reform dummies and fedelig
# trend : (year-2008)*12 +(month-8)
# These are all the additional regressors;
# From the footnote of Table 2 in Akosa Antwi:
# Other regressors are 
#   an indicator for the period after ACA enactment but before implementation, 
#   an indicator for the period after ACA implementation, 
#   an indicator for each year of age, 
#   year-specific fixed effects,
#   month-specific fixed effects,
#   time trend, 
#   state fixed effects, 
#   gender, 
#   race/ethnicity, 
#   marital status, 
#   student status, 
#   household income as a share of federal poverty line and its squared term, 
#   monthly unemployment at state level,
#   interaction of unemployment and an indicator for treatment group
exp_vars = c("factor(age)","factor(year)","factor(month)","trend","factor(fipstate)",
             "female","hispanic", "white", "asian", "other", "mar", "student",
             "fpl_ratio", "fpl_ratio_2", "ue", "ue_treat")
# Each state gets its own linear trend, but some states aren't in SIPP data
st_trends = paste("st_trend_", c(1:2, 4:6, 8:13, 15:42, 44:51, 53:55), sep = "")
years = c("y2009", "y2010")
months = paste("month", c(2:12), sep = "")


# Make nice labels for the months
# Aug 2008 to Nov 2011- 40 months total
month.id.labels <- as_tibble(expand.grid(1:12,2008:2011)) %>%
  # Data starts in Aug 2008
  filter(!(((Var1<8)&(Var2==2008))|(Var1==12)&(Var2==2011))) %>%
  mutate(month.id=paste(Var2,Var1,sep="-"),
         month.index=1:40)

#### Process data ####
## Optionally: recreate the analytical data sets
if (recreate.data){
  # Start with the data distributed by Akosa Antwi
  data <-  haven::read_dta(here::here("2_Reanalysis/0_Source_files/data", "augmented.dta")) %>%
    # Normalize the Weights to sum to one 
    mutate(weight = p_weight/sum(p_weight)) %>%
    # Exclude one extremely influential data point (due to fpl and fpl^2 in model)
    filter(fpl_ratio<70)
  
  ## Make monthly data sets
  ## Average monthly coverage and unemployment by age group
  month.dat <- data %>% 
    mutate(month.id=factor(paste(year,as.numeric(month),sep="-"),levels=month.id.labels$month.id),
           age_grp=factor(ifelse(age%in%16:18,"16-18",ifelse(age%in%19:25,"19-25","27-29")))) %>% 
    group_by(trend,month.id,age_grp) %>%
    summarize(across(all_of(c(instype,"ue")),~weighted.mean(.x,weights=weight))) %>%
    pivot_longer(anyhi:ue) %>%
    mutate(name=factor(name,levels=c(instype,'ue'),labels=c(inslabels,'Unemployment')),
           value=ifelse(name=="Unemployment",value,value*100))
  
  ## Differential average monthly coverage by treatment group
  temp <- data %>% 
    group_by(trend,fedelig) %>%
    summarize_at(all_of(instype),~weighted.mean(.x,weight)*100) %>%
    mutate(fedelig=factor(fedelig,levels=c(0,1),labels=c('ctrl','trt'))) %>%
    arrange(trend,fedelig) %>%
    group_by(trend) %>%
    # subtract trt minus control
    mutate_at(all_of(instype),~ .x-lag(.x)) %>%
    # just take these differences
    filter(fedelig=='trt') %>%
    dplyr::select(trend,all_of(instype)) %>%
    pivot_longer(all_of(instype)) %>%
    arrange(name,trend) %>%
    group_by(name)
  
  ## Normalize by the last pre-intervention month (18)
  month.dat.diff <- left_join(temp,temp %>% slice(19) %>% 
                                dplyr::select(-trend) %>%
                                dplyr::rename(ref.value=value),
                              by="name") %>%
    mutate(normed.diff=value-ref.value) %>%
    mutate(name=factor(name,levels=instype,labels=inslabels))
  rm(temp)
  
  ## Make data for regression 
  # Create the post-period month dummies needed for the proper treatment effect estimation
  # in the models that include differential linear trends:
  effects = c("mar_sep10", "after_oct10", "fedelig", "elig_mar10", "elig_oct10")
  
  dat.for.reg <- data %>% dplyr::select(c(all_of(instype),all_of(effects),age_group,"st_with_law",
                                          "age","year","y2009","y2010","month","fipstate","trend",
                                          paste0("age",c(16:18,19:25,27:29)),
                                          "mar_sep10","after_oct10",all_of(exp_vars[6:16]), all_of(st_trends), all_of(months),
                                          weight,p_weight)) %>%
    mutate(month.id=factor(paste(year,as.numeric(month),sep="-"))) %>%
    left_join(month.id.labels,by="month.id") %>%
    # Indicators for each month of the enactment and implementation periods
    # separately for treated and comparison groups
    mutate(fipstate=factor(fipstate),
           enact.trt.month=factor(month.index*elig_mar10),
           enact.month=factor(month.index*mar_sep10),
           impl.trt.month=factor(month.index*elig_oct10),
           impl.month=factor(month.index*after_oct10)) %>%
    arrange(fipstate,year)
  
  ## Save all of these for use in the packaged function testing
  save(month.dat.diff,dat.for.reg,file=here::here("2_Reanalysis/2_Output/dependent_coverage.RData"))
} else {
  load(here("2_Reanalysis/2_Output/dependent_coverage.RData"))
}

#### Fit models ####
if (rerun.models) {
  ## Empty object to hold the results
  suest.results <- tibble(NULL)
  # Loop over dependent variables
  for (depvar in instype){
    # Loop over included control variables (starting with just sex and race/ethnicity)
    for (i in 10:16){
      # Reduced model
      base.model = as.formula(paste(depvar,"~",paste(c("factor(fipstate)","factor(trend)",
                                                       "enact.trt.month","impl.trt.month",
                                                       paste0("age",c(17,18,19,20:25,27:29)),
                                                       exp_vars[c(6:i)]),collapse="+")))
      
      # Expanded model with differential linear trends
      exp.model <- update.formula(base.model,"~ . + fedelig:trend")
      
      # Call to the package version of the function:
      out = run_NI_test(data=dat.for.reg, 
                        reduced =base.model, expanded=exp.model, lincom_var='impl.trt', 
                        robust = TRUE, cluster = "fipstate", weight = "weight", 
                        null_reduced = FALSE, alpha = .05)
      
      # Format and combine results
      suest.results <- bind_rows(suest.results,
                                 tibble(model="linear trend",
                                        depvar=depvar,
                                        controls=paste(exp_vars[c(6:i)],collapse=", "),
                                        diff.est=out$diff,
                                        diff.se=out$se[1],
                                        diff.lb=out$CI[1],
                                        diff.ub=out$CI[2],
                                        red.est=out$tx_r,
                                        red.se=sqrt(out$v_r[1,1]),
                                        exp.est=out$tx_e,
                                        exp.se=sqrt(out$v_e[1,1])) %>%
                                   mutate(red.lb=red.est-qnorm(.975)*red.se,
                                          red.ub=red.est+qnorm(.975)*red.se,
                                          exp.lb=exp.est-qnorm(.975)*exp.se,
                                          exp.ub=exp.est+qnorm(.975)*exp.se))
    } # end of loop over control variables
  } # end of loop over dependent variables
  save(suest.results,file=here::here("2_Reanalysis/2_Output", paste0(format(Sys.Date(),"%Y-%m-%d"),"_suest_results.RData")))
} else {
  # Otherwise, load the old fits:
  load("2_Reanalysis/2_Output/2025-09-30_suest_results.RData")
}
#write.csv(suest.results %>% filter(controls==c(paste0(exp_vars[6:15],collapse=", "))), file = "suest_res_v2.csv")

## Format the regression results for display in tables
reg.results.table <- suest.results %>%
  # Put them all on percentage point scale
  mutate(dplyr::across(.cols=4:15,.fns=~round(.x*100,1))) %>%
  # Make nice display summaries
  mutate(reduced=paste0(red.est," (",red.lb,", ",red.ub,")"),
         expanded=paste0(exp.est," (",exp.lb,", ",exp.ub,")"),
         difference=paste0(diff.est," (",diff.lb,", ",diff.ub,")"),
         # Add rule-out columns
         rule.out.smaller=ifelse((abs(diff.lb)<2.1) & (abs(diff.ub) < 2.1),"Yes","No"),
         # Threshold with sign matching expected treatment effect (positive for any health insurance and dependent coverage, negative for others)
         rule.out.smaller.signed=ifelse(depvar%in%c('anyhi','emphi_dep'),
                                        ifelse((diff.lb < 2.1) & (diff.ub < 2.1),"Yes","No"),
                                        ifelse((diff.lb > -2.1)& (diff.ub > -2.1),"Yes","No")),
         rule.out.larger=ifelse((abs(diff.lb)<5.3) & (abs(diff.ub) < 5.3),"Yes","No"),
         # Nice labels for the outcome variables
         outcome=factor(depvar,levels=instype,labels=inslabels))


#### Test for trends ####
ptt.results <- NULL
for (depvar in instype){
  # Loop over sets of control variables
  for (i in 10:16){
    form = as.formula(paste(depvar,"~",paste(c("fedelig","trend:fedelig",
                                               "factor(fipstate)","factor(trend)",
                                               paste0("age",c(17,18,20:25,27:29)),
                                               exp_vars[6:i]),collapse="+")))
    fit.pre <- fixest::feols(form,data=filter(dat.for.reg,(enact.month==0)&(impl.month==0)),
                             vcov=~fipstate,weights=~weight)
    ptt.results <- bind_rows(ptt.results,
                             c(model="PTT w/controls",
                               depvar=depvar,
                               controls=paste0(exp_vars[6:i],collapse=", "),
                               fit.pre$coeftable['fedelig:trend',c('Estimate','Std. Error','Pr(>|t|)')]))
  }
}
## Put these on a PPT scale so they don't have so many decimals
ptt.res.for.display <- ptt.results %>%
  mutate(est=as.numeric(Estimate)*100,
         se=100*as.numeric(`Std. Error`),
         lb=est-qnorm(.975)*se,
         ub=est+qnorm(.975)*se,
         Trt.effect=paste0(round(est,2)," (",round(lb,2),", ",round(ub,2),")"),
         depvar=factor(depvar,levels=instype,labels=inslabels),
         Rule.out.small = ifelse((abs(lb) < .11) & (abs(ub) < .11),"Yes","No"),
         Rule.out.large = ifelse((abs(lb) <.27) & (abs(ub) < 0.27),"Yes","No"))



#### Table 3 ####
## Reduced and expanded results from model excluding the unemployment*treat interaction term
knitr::kable(reg.results.table %>% filter(controls==c(paste0(exp_vars[6:15],collapse=", "))) %>%
               dplyr::select(outcome,reduced,expanded,difference,rule.out.smaller,rule.out.smaller.signed,rule.out.larger),format="latex")

# For comparison, using all the control variables
knitr::kable(reg.results.table %>% filter(controls==c(paste0(exp_vars[6:16],collapse=", "))) %>%
               dplyr::select(outcome,reduced,expanded,difference,rule.out.smaller,rule.out.smaller.signed,rule.out.larger),format="latex")


#### Figure S2 ####
## Average monthly coverage and unemployment by age group
ggplot(month.dat,aes(x=month.id,y=value,group=age_grp)) + geom_line(aes(col=age_grp)) +
  geom_smooth(aes(col=age_grp),se=F) +
  scale_y_continuous("Percent") +
  scale_x_discrete("Year and month",breaks=month.id.labels$month.id[seq(1,48,by=4)]) +
  theme(axis.text.x = element_text(angle=45,hjust=1)) + geom_vline(xintercept=c('2010-3','2010-9'),lty=2)+
  facet_wrap(~name,scale="free_y") + theme(legend.position="bottom") +
  scale_color_discrete("Age group")
ggsave(here::here("2_Reanalysis/2_Output","trends_in_outcomes_and_unemployment_by_age_group.eps"),width=6,height=6)

#### Figure S3 #### 
## Differential average monthly coverage (trt vs combined control group)
ggplot(month.dat.diff,aes(x=trend,y=normed.diff)) + 
  geom_line() +
  geom_smooth(se=F) + facet_wrap(~name) +
  geom_vline(xintercept=c(19,26),lty=2) + geom_hline(yintercept=0)+ 
  scale_y_continuous("Difference (trt - ctrl) in percent covered") +
  scale_x_discrete("Time (months)")
ggsave(here::here("2_Reanalysis/2_Output","unadjusted_diff_outcome_trends.eps"),width=6.5,height=4.5)

#### Figure S4 ####
## Parallel trends tests estimates from models with various control variable sets
ggplot(filter(ptt.res.for.display,model=="PTT w/controls"),aes(x=depvar)) + 
  geom_pointrange(aes(y=est,ymin=lb,ymax=ub,col=controls),position=position_dodge(width=.5)) +
  geom_hline(yintercept=0) + theme(legend.position="bottom") + 
  guides(col=guide_legend(ncol=1)) + scale_color_brewer(palette="Set1") +
  xlab("") + ylab("Estimated differential pre-period slope")
ggsave(here::here("2_Reanalysis/2_Output","pre-period_differential_trend_estimates_by_control_vars.png"),width=6,height=6)

#### Table S4 #### (change rounding to 2 decimals)
## For comparison to Akosa Antwi, results with all the controls but with our model specification
knitr::kable(reg.results.table %>% filter(controls==paste0(exp_vars[6:16],collapse=", ")) %>%
               mutate(repl.res=paste0(red.est," (",red.se,")")) %>%
               dplyr::select(outcome,repl.res),format="latex")




