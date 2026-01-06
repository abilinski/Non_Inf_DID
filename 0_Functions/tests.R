#### TESTS FOR matrix_functions.R ####
here::i_am("0_Functions/tests.R")
source("global_options.R")
source("0_Functions/matrix_functions.R")

# Runs tests of matrix functions
run_tests = function(){

  ####********************** SETUP  ********************** #### 
  # number of rows in X
  n = 2000
  
  # pull coefficients in reduced and expanded models
  p = sample(10:20, 1)
  q = sample(1:5, 1)
  
  # sample to test
  lincom_test = sample(p, 3, replace = F)
  
  # pull true coefficients
  betas = matrix(runif(p + q, -10, 10), ncol = 1)
  
  # pull correlated X's
  X = Rfast::rmvnorm(n = n, mu = rep(0, p + q), sigma = .9*diag(p + q) +matrix(.1, p+ q, p + q))
  
  # generate Y with heteroskedastic errors
  y_mean = X%*%betas 
  Y = y_mean+rnorm(n = n, mean = y_mean/mean(y_mean))
  
  # make data frame
  df = data.frame(Y,X) %>%
    mutate(
      # generate weights
      weight_i = runif(n, 0,100),
      weight = weight_i/sum(weight_i),
      
      # make clusters
      id = rep(1:100, each = n/100)
    )
  
  # make formulas
  vars = names(df)
  form1_val = as.formula(paste(vars[1], "~", paste(vars[2:(p+1)], collapse = "+")))
  form2_val = as.formula(paste(vars[1], "~", paste(vars[2:(p+q+1)], collapse = "+")))
  stata_form1 = paste("regress", vars[1], paste(vars[2:(p+1)], collapse = " "))
  stata_form2 = paste("regress", vars[1], paste(vars[2:(p+q+1)], collapse = " "))
  lincom_glht =  paste(1/length(lincom_test), "*(", paste("X", lincom_test, sep = "", collapse = "+"), ")=0", sep = "")
  
  # standard regressions
  lm1 = lm(form1_val, data = df); lm1_coeff = lm1$coefficients
  lm2 = lm(form2_val, data = df); lm2_coeff = lm2$coefficients
  
  # weighted regression
  lm1.w = lm(form1_val, data = df, weight = weight); lm1_coeff.w = lm1.w$coefficients
  lm2.w = lm(form2_val, data = df, weight = weight); lm2_coeff.w = lm2.w$coefficients
  
  ####********************** TESTS  **********************#### 
  #### IID, no weights ####
  iid = run_NI_test(data = df, form1 = form1_val, form2 = form2_val,
                    lincom_vars = lincom_test,
                    robust = F, cluster = NULL, weight = F,
                    null_reduced = T, return_all = T, z = qnorm(.975))
  
  # check coefficients against lm
  coeff1_chk = all.equal(iid[[4]], lm1_coeff)==T
  coeff2_chk = all.equal(iid[[5]], lm2_coeff)==T
  
  # check variance against lm
  vcov1_chk = all.equal(iid[[6]], vcov(lm1), check.attributes = F)==T
  vcov2_chk = all.equal(iid[[7]], vcov(lm2), check.attributes = F)==T
  
  # check linear combos and t-statistics
  # reduced
  lht = summary(glht(lm1, linfct = lincom_glht))
  r_coeff_chk = all.equal(iid[[9]], lht$test$coefficients, check.attributes = F)
  r_var_chk = all.equal(as.numeric(iid[[11]]), lht$test$sigma^2, check.attributes = F)
  r_t_chk = all.equal(as.numeric(iid[[13]]), lht$test$tstat, check.attributes = F)

  # expanded
  lht_e = summary(glht(lm2, linfct = lincom_glht))
  e_coeff_chk = all.equal(iid[[10]], lht_e$test$coefficients, check.attributes = F)
  e_var_chk = all.equal(as.numeric(iid[[12]]), lht_e$test$sigma^2, check.attributes = F)
  e_t_chk = all.equal(as.numeric(iid[[14]]), lht_e$test$tstat, check.attributes = F)
  
  # all checks
  chk_iid = coeff1_chk & coeff2_chk & vcov1_chk & vcov2_chk & r_coeff_chk & r_var_chk & r_t_chk & e_coeff_chk & e_var_chk & e_t_chk
  
  #### IID, NO WEIGHTS, NULL IS EXPANDED ####
  # checks for cov = var when you reset null
  # which residuals to use for covariance?
  # does not really matter, even in finite sample
  iid_null_exp = run_NI_test(data = df, form1 = form1_val, form2 = form2_val,
                             lincom_vars = lincom_test,
                             robust = F, cluster = NULL, weight = F,
                             null_reduced = F, return_all = T, z = qnorm(.975))
  
  chk_iid_null_exp = all.equal(iid_null_exp[[6]], iid_null_exp[[8]])==T 
  
  #### IID WEIGHTS ####
  iid_weights = run_NI_test(data = df, form1 = form1_val, form2 = form2_val,
                            lincom_vars = lincom_test,
                            robust = F, cluster = NULL, weight = T,
                            null_reduced = T, return_all = T, z = qnorm(.975))
  
  # check coefficients against lm
  coeff1_chk = all.equal(iid_weights[[4]], lm1_coeff.w)==T
  coeff2_chk = all.equal(iid_weights[[5]], lm2_coeff.w)==T
  
  # check variance against lm
  vcov1_chk = all.equal(iid_weights[[6]], vcov(lm1.w), check.attributes = F)==T
  vcov2_chk = all.equal(iid_weights[[7]], vcov(lm2.w), check.attributes = F)==T
  
  # check linear combos and t-statistics
  # reduced
  lht = summary(glht(lm1.w, linfct = lincom_glht))
  r_coeff_chk = all.equal(iid_weights[[9]], lht$test$coefficients, check.attributes = F)
  r_var_chk = all.equal(as.numeric(iid_weights[[11]]), lht$test$sigma^2, check.attributes = F)
  r_t_chk = all.equal(as.numeric(iid_weights[[13]]), lht$test$tstat, check.attributes = F)
  
  # expanded
  lht_e = summary(glht(lm2.w, linfct = lincom_glht))
  e_coeff_chk = all.equal(iid_weights[[10]], lht_e$test$coefficients, check.attributes = F)
  e_var_chk = all.equal(as.numeric(iid_weights[[12]]), lht_e$test$sigma^2, check.attributes = F)
  e_t_chk = all.equal(as.numeric(iid_weights[[14]]), lht_e$test$tstat, check.attributes = F)
  
  # all checks
  chk_iid_weight = coeff1_chk & coeff2_chk & vcov1_chk & vcov2_chk & r_coeff_chk & r_var_chk & r_t_chk & e_coeff_chk & e_var_chk & e_t_chk
  
  #### ROBUST SEs, NO WEIGHTS ####
  robust = run_NI_test(data = df, form1 = form1_val, form2 = form2_val,
                       lincom_vars = lincom_test,
                       robust = T, cluster = NULL, weight = F,
                       null_reduced = T, return_all = T, z = qnorm(.975))
  
  # check coefficients against lm
  coeff1_chk = all.equal(robust[[4]], lm1_coeff)==T
  coeff2_chk = all.equal(robust[[5]], lm2_coeff)==T
  
  # calculate robust errors
  mat1 = model.matrix(lm1); Vr = solve(t(mat1)%*%mat1); res1 =residuals(lm1)
  vcov1_robust = (nrow(df))/(nrow(df)-1)*Vr%*%t(mat1)%*%diag(res1^2)%*%mat1%*%Vr
  mat2 = model.matrix(lm2); Ve = solve(t(mat2)%*%mat2); res2 = residuals(lm2)
  vcov2_robust = (nrow(df))/(nrow(df)-1)*Ve%*%t(mat2)%*%diag(res2^2)%*%mat2%*%Ve
  cov_robust = (nrow(df))/(nrow(df)-1)*Vr%*%t(mat1)%*%diag(res1*res2)%*%mat2%*%Ve
  
  # check variance against lm
  vcov1_chk = all.equal(robust[[6]], vcov1_robust, check.attributes = F)==T
  vcov2_chk = all.equal(robust[[7]], vcov2_robust, check.attributes = F)==T
  cov_chk = all.equal(robust[[8]], cov_robust, check.attributes = F)==T
  
  # repeat by checking with stata
  # make stata version of vcov matrix
  stata_code = paste(stata_form1, "\n",
                     "est store m1 \n",
                     stata_form2, "\n", 
                     "est store m2 \n
        
        suest m1 m2 \n
  
        matrix define C = e(V) \n
        drop _all \n
        svmat double C")
  vcov_stata = stata(stata_code,
                     data.in = df, data.out = T, stata.echo = F)
  
  # convert to matrix
  vcov_stata_mat = data.matrix(vcov_stata)
  rownames(vcov_stata_mat) = names(vcov_stata)
  
  # reorder by stata ordering
  vcov_stata_mat2 = umx_reorder(vcov_stata_mat, newOrder = names(vcov_stata)[c((p+1), 1:p, (2*p+q+3), (p+3):(2*p+q+2))])
  
  vcov1_chk_stata = all.equal(robust[[6]], vcov_stata_mat2[1:(p+1), 1:(p+1)], check.attributes = F)==T
  vcov2_chk_stata = all.equal(robust[[7]], vcov_stata_mat2[(p+2):(2*p+q+2), (p+2):(2*p+q+2)], check.attributes = F)==T
  cov_chk_stata = all.equal(robust[[8]], vcov_stata_mat2[1:(p+1), (p+2):(2*p+q+2)], check.attributes = F)==T
  
  # all checks
  chk_robust = coeff1_chk & coeff2_chk & vcov1_chk & vcov2_chk & cov_chk & vcov1_chk_stata & vcov2_chk_stata & cov_chk_stata
  
  #### CLUSTER SEs, NO WEIGHTS ####
  cluster_robust = run_NI_test(data = df, form1 = form1_val, form2 = form2_val,
                               lincom_vars = lincom_test,
                               robust = T, cluster = "id", weight = F,
                               null_reduced = T, return_all = T, z = qnorm(.975))
  
  # check covariates
  coeff1_chk = all.equal(cluster_robust[[4]], lm1_coeff)==T
  coeff2_chk = all.equal(cluster_robust[[5]], lm2_coeff)==T
  
  # make stata version of vcov matrix
  stata_code = paste(stata_form1, "\n",
                     "est store m1 \n",
                     stata_form2, "\n", 
                     "est store m2 \n
        
        suest m1 m2, vce(cluster id) \n
  
        matrix define C = e(V) \n
        drop _all \n
        svmat double C")
  
  vcov_stata = stata(stata_code,
                     data.in = df, data.out = T, stata.echo = F)
  
  # convert to matrix
  vcov_stata_mat = data.matrix(vcov_stata)
  rownames(vcov_stata_mat) = names(vcov_stata)
  
  # reorder & correct by (n-p-1)*(n-1) b/c suest is a bit different from standard Stata clustering
  vcov_stata_mat2 = umx_reorder(vcov_stata_mat, newOrder = names(vcov_stata)[c((p+1), 1:p, (2*p+q+3), (p+3):(2*p+q+2))])
  
  # check variance against suest
  vcov1_chk = all.equal(cluster_robust[[6]], vcov_stata_mat2[1:(p+1), 1:(p+1)]/(n-p-1)*(n-1), check.attributes = F)==T
  vcov2_chk = all.equal(cluster_robust[[7]], vcov_stata_mat2[(p+2):(2*p+q+2), (p+2):(2*p+q+2)]/(n-p-q-1)*(n-1), check.attributes = F)==T
  cov_chk = all.equal(cluster_robust[[8]], vcov_stata_mat2[1:(p+1), (p+2):(2*p+q+2)], check.attributes = F)==T
  
  # check cluster 
  chk_cluster = coeff1_chk & coeff2_chk & vcov1_chk & vcov2_chk & cov_chk
  
  #### ROBUST SEs, WEIGHTS ####
  robust_weighted = run_NI_test(data = df, form1 = form1_val, form2 = form2_val,
                                lincom_vars = lincom_test,
                                robust = T, cluster = NULL, weight = T,
                                null_reduced = T, return_all = T, z = qnorm(.975))
  
  # run in feols
  # to check cluster + weighting
  feols_form1 = feols(form1_val, data = df, weights = ~weight, se = "hetero")
  feols_form2 = feols(form2_val, data = df, weights = ~weight, se = "hetero")
  
  # check variance covariance matrices
  # note a difference in finite sample correction (becomes tricker with simult est, so we omit, similar to suest)
  vcov1_chk = all.equal(robust_weighted[[6]], vcov(feols_form1)*(n-p-1)/(n-1), check.attributes = F)==T
  vcov2_chk = all.equal(robust_weighted[[7]], vcov(feols_form2)*(n-p-q-1)/(n-1), check.attributes = F)==T
  
  # run checks
  chk_robust_weighted = vcov1_chk & vcov2_chk
  
  #### CLUSTER SEs, WEIGHTS ####
  cluster_robust_weighted = run_NI_test(data = df, form1 = form1_val, form2 = form2_val,
                                        lincom_vars = lincom_test,
                                        robust = T, cluster = "id", weight = T,
                                        null_reduced = T, return_all = T, z = qnorm(.975))
  
  # run in feols
  # to check cluster + weighting
  # suest doesn't play nicely
  # so harder to check covariance
  feols_form1 = feols(form1_val, data = df, cluster = ~id, weights = ~weight)
  feols_form2 = feols(form2_val, data = df, cluster = ~id, weights = ~weight)
  
  # check variance covariance matrices
  vcov1_chk = all.equal(cluster_robust_weighted[[6]], vcov(feols_form1), check.attributes = F)==T
  vcov2_chk = all.equal(cluster_robust_weighted[[7]], vcov(feols_form2), check.attributes = F)==T
  
  # run checks
  chk_cluster_weighted = vcov1_chk & vcov2_chk
  
  #### TEST LINEAR COMBINATION ####
  
  # check that linear combos work as expected
  # we only do in one as we expect machinery to work consistently across models
  chk_lincom = all.equal(mean(robust[[4]][lincom_test+1])-mean(robust[[5]][lincom_test+1]), robust[[1]])
  
  # check on standard errors
  # by making a big matrix :)
  big_var = rbind(cbind(data.matrix(robust[[6]]), data.matrix(robust[[8]])), 
                  cbind(data.matrix(t(robust[[8]])), data.matrix(robust[[7]])))
  big_vec = rep(0, 2*p+q+2)
  big_vec[lincom_test+1]=1/length(lincom_test)
  big_vec[lincom_test+2+p]=-1/length(lincom_test)
  big_vec = matrix(big_vec, ncol = 1)
  
  # check the standard error
  chk_se = all.equal(sqrt(t(big_vec)%*%as.matrix(big_var)%*%big_vec), robust[[2]])
    
  # return output
  return(data.frame(chk_iid, chk_iid_null_exp, chk_iid_weight, chk_robust, chk_cluster, 
                    chk_robust_weighted, chk_cluster_weighted, chk_lincom, chk_se))
}

run_tests()

