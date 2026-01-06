#### RUN NON-INFERIORITY TESTS ####
here::i_am("0_Functions/matrix_functions.R")
source("global_options.R")

run_NI_test = function(data, form1 = outcome~trtpost + Group + factor(factoryear), 
                  form2 = outcome~interaction + trtpost + Group + factor(factoryear),
                  lincom_vars = 1,
                  robust = T, cluster = NULL, weight = F,
                  null_reduced = F, return_all = F, z = qnorm(.975)){
  
  # Extract sample size
  n = nrow(data)
  
  # Add a weight variable if none
  # Sets weights to 1
  # i.e., equal to unweighted
  if(!weight) data$weight = 1
  
  # Set cluster variable
  if(!is.null(cluster)) data$cluster = data[,cluster]
    
  # Fit the reduced model
  r = feols(form1, data=data,weights=~weight) # Run reduced model
  Xr = model.matrix(r)                            # Extract model matrix
  ur = r$residuals                             # Extract residuals
  Vr = r$cov.iid/r$sigma2                         # Extract unscaled covariance
  coeff_r = r$coefficients                        # Extract coefficients

  # Fit the expanded model  
  e = feols(form2, data=data,weights=~weight)       # Run expanded model
  Xe = model.matrix(e)                              # Extract model matrix 
  ue = e$residuals                                # Extract residuals
  Ve = e$cov.iid/e$sigma2                         # Extract unscaled covariance
  coeff_e = e$coefficients                        # Extract coefficients
  
  # extract parameter size
  p_r = ncol(Xr)
  p_e = ncol(Xe)
  
  # Fit variance covariance matrices
  
    # Select residuals to use for reduced model
    # If the null hypothesis is that the reduced model is true: ur
    # Otherwise: ue (see Supplement Lemma 3, Bilinski and Hatfield)
    if(null_reduced){
      ur_for_vcov_r = ur
    }else{
      ur_for_vcov_r = ue
      p_r = ncol(Xe)
    }
  
    # Estimate variance covariance matrices
    if(!robust & is.null(cluster)){
      # Homoskedastic i.i.d. errors
      vcov_r = Vr*mean(ur_for_vcov_r^2*data$weight)/(n-p_r)*n
      vcov_e = Ve*mean(ue^2*data$weight)/(n-p_e)*n
      cov = Vr*mean(ur_for_vcov_r*ue*data$weight)/(n-p_e)*n
    }else if(is.null(cluster)){
    # Heteroskedasticity robust SEs, without clustering
      
      # add weights
      ur_for_vcov_r = ur_for_vcov_r*data$weight
      ue = ue*data$weight
      
      # calculate variance covariance matrices
      vcov_r = n/(n-1)*eigenMatMult(eigenMatMult(Vr, eigenMatMult(t(Xr*ur_for_vcov_r^2), Xr)), Vr)
      vcov_e = n/(n-1)*eigenMatMult(eigenMatMult(Ve, eigenMatMult(t(Xe*ue^2), Xe)), Ve)
      cov =  n/(n-1)*eigenMatMult(eigenMatMult(Vr, eigenMatMult(t(Xr*ur_for_vcov_r*ue), Xe)), Ve)
    }else if(!is.null(cluster)){
      
      # add weights
      ur_for_vcov_r = ur_for_vcov_r*data$weight
      ue = ue*data$weight
      
      # set up for calculating over clusters
      g = unlist(unique(data$cluster))
      vcov_r_meat = list()
      vcov_e_meat = list() 
      cov_meat = list() 
      
      # calculate over clusters
      for(i in 1:length(g)){
        
        # subset observations
        these = which(data$cluster==g[i])
        Xr_sub = fsubset(Xr, these); ur_for_vcov_r_sub = matrix(fsubset(ur_for_vcov_r, these), ncol = 1)
        Xe_sub = fsubset(Xe, these); ue_sub = matrix(fsubset(ue, these), ncol = 1)
        
        # estimate inner matrices
        vcov_r_meat[[i]] = eigenMatMult(eigenMatMult(eigenMatMult(t(Xr_sub), ur_for_vcov_r_sub), t(ur_for_vcov_r_sub)), Xr_sub)
        vcov_e_meat[[i]] = eigenMatMult(eigenMatMult(eigenMatMult(t(Xe_sub), ue_sub), t(ue_sub)), Xe_sub)
        cov_meat[[i]] = eigenMatMult(eigenMatMult(eigenMatMult(t(Xr_sub), ur_for_vcov_r_sub), t(ue_sub)), Xe_sub)
      }
      
      # calculate over clusters
      vcov_r = eigenMatMult(eigenMatMult(Vr, Reduce("+", vcov_r_meat)), Vr)*length(g)/(length(g)-1)*(n-1)/(n-p_r) 
      vcov_e = eigenMatMult(eigenMatMult(Ve, Reduce("+", vcov_e_meat)), Ve)*length(g)/(length(g)-1)*(n-1)/(n-p_e) 
      cov = eigenMatMult(eigenMatMult(Vr, Reduce("+", cov_meat)), Ve)*length(g)/(length(g)-1)
    }
    
    # Estimate linear combinations
    vec_lincom1 = matrix(0, length(coeff_r), 1)
    vec_lincom1[(lincom_vars+1),] = 1/length(lincom_vars)
    vec_lincom2 = matrix(0, length(coeff_e), 1)
    vec_lincom2[(lincom_vars+1),] = -1/length(lincom_vars)
    
    # calculate difference
    tx_r = sum(coeff_r*vec_lincom1)
    tx_e = sum(-1*coeff_e[1:length(coeff_r)]*vec_lincom2[1:length(coeff_r)])
    diff = sum(coeff_r*vec_lincom1 + coeff_e[1:length(coeff_r)]*vec_lincom2[1:length(coeff_r)])
    
    # calculate standard error
    v_r = eigenMatMult(eigenMatMult(t(vec_lincom1), vcov_r),vec_lincom1)
    v_e = eigenMatMult(eigenMatMult(t(vec_lincom2),vcov_e),vec_lincom2)
    cov_lincom = 2*eigenMatMult(eigenMatMult(t(vec_lincom1),cov), vec_lincom2)
                                
    se = sqrt(v_r + v_e + cov_lincom)
       
    # statistical significance
    CI = c(diff-se*z, diff + se*z)
    tval_r = tx_r/sqrt(v_r)
    tval_e = tx_e/sqrt(v_e)
    
    # return all output (for testing)
    if(return_all){
      return(list(diff, se, CI, coeff_r, coeff_e, vcov_r, vcov_e, cov, tx_r, tx_e, v_r, v_e, tval_r, tval_e))
    }else{ # return outputs of interest
      return(list(diff, se, CI, tx_r, tx_e, v_r, v_e, tval_r, tval_e, cov_lincom))
    }
  
}
