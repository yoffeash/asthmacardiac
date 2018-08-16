

########################attempt at loop##########################
### create smaller dataset ###
cardiac_asthma_bio <- cardiac_asthma %>% dplyr::select(SID,feno,IGE,sput_eos_percent,peripheral_eos_percent,
                                                       PAA_ratio,RV_LV_epi_ratio,vol_epi_RV_BSAI,vol_epi_LV_BSAI,RV_LV_epi_vol_BSAI,
                                                       age_spiro,male,minority,WholeLung950,wholelungvolume_ht_m,preFEV1_Quanpct_pred,bmi,sysbp) %>% 
  rename(dxfeno=feno,dxIGE=IGE,dxsput_eos_percent=sput_eos_percent,dxperipheral_eos_percent=peripheral_eos_percent,
         dxPAA_ratio=PAA_ratio,dxRV_LV_epi_ratio=RV_LV_epi_ratio,dxvol_epi_RV_BSAI=vol_epi_RV_BSAI,dxvol_epi_LV_BSAI=vol_epi_LV_BSAI,dxRV_LV_epi_vol_BSAI=RV_LV_epi_vol_BSAI)

### position of variables ###
# outcome
out_start = 2
out_end = 5
out_nvar = out_end-out_start+1

out_variable=rep(NA, out_nvar)
out_beta=rep(NA, out_nvar)
out_se = rep(NA, out_nvar)
out_pvalue=rep(NA, out_nvar)

# exposure
exp_start=6
exp_end=11
exp_nvar=exp_end-exp_start+1

number=1

###loop###
dat <- cardiac_asthma_bio
for (i in out_start:out_end){
  outcome = colnames(dat)[i]
  for (j in exp_start:exp_end){
    exposure = colnames(dat)[j]
    model <- lm(get(outcome) ~ get(exposure) + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                  preFEV1_Quanpct_pred + bmi + sysbp,
                na.action = na.exclude,
                data=dat)
    
    beta = coef(model)
    CI25 = confint(model)[,1]
    CI975 = confint(model)[,2]
    pval = coef(summary(model))[,4]
    
    out_beta[number] = as.numeric(beta[2])
    out_CI25[number] = as.numeric(CI25[2])
    out_CI975[number] = as.numeric(CI975[2])
    out_pvalue[number] = as.numeric(pval[2])
    out_variable[number] = outcome
    number = number + 1
    
    exp_beta[number] = as.numeric(beta[2])
    exp_CI25[number] = as.numeric(CI25[2])
    exp_CI975[number] = as.numeric(CI975[2])
    exp_pvalue[number] = as.numeric(pval[2])
    exp_variable[number] = outcome
    number = number + 1
    
  }
}

outcome = data.frame(out_variable, out_beta, out_se, out_pvalue)
exposure = data.frame(exp_variable, exp_beta, exp_se, exp_pvalue)

outcome = outcome %>% 
  rename(
    variable = out_variable,
    beta = out_beta,
    se = out_se,
    pvalue = out_pvalue
  )
exposure = exposure %>% 
  rename(
    variable = exp_variable,
    beta = exp_beta,
    se = exp_se,
    pvalue = exp_pvalue
  )
all = rbind(outcome, exposure)
all = na.omit(all)

data = all %>% 
  mutate(
    type = substr(variable, 1, 2)
  ) %>% 
  spread(type, variable) %>% 
  rename(
    d = dx,
    i = ix
  ) %>% 
  mutate (
    beta = round(beta, 5),
    se = round(se, 5),
    pvalue = round(pvalue, 5)
  ) %>% 
  select(d, i, beta, se, pvalue)