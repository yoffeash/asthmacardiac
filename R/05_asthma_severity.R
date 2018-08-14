### asthma severity ###
cardiac_asthma$severe2_v3_f <- factor(cardiac_asthma$severe2_v3)
##########PAA Entire Cohort############################################################################################
contrasts(cardiac_asthma$severe2_v3_f) <- contr.treatment(3, base=1)
severity.PAA.normref <- lm(PAA_ratio ~ severe2_v3_f + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                             preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(severity.PAA.normref), 
      confint(severity.PAA.normref), 
      p = coef(summary(severity.PAA.normref))[,4])
## mild/moderate as reference
contrasts(cardiac_asthma$severe2_v3_f) <- contr.treatment(3, base=2)
severity.PAA.mmref <- lm(PAA_ratio ~ severe2_v3_f + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                           preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(severity.PAA.mmref), 
      confint(severity.PAA.mmref), 
      p = coef(summary(severity.PAA.mmref))[,4])

##############PAA in those with emphysema excluded##################################################################
## healthy control as reference
contrasts(cardiac_asthma$severe2_v3_f) <- contr.treatment(3, base=1)
severity.PAA.normref <- lm(PAA_ratio ~ severe2_v3_f + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                             preFEV1_Quanpct_pred + bmi + sysbp, data=subset(cardiac_asthma, emphysema==0))
cbind(effect = coef(severity.PAA.normref), 
      confint(severity.PAA.normref), 
      p = coef(summary(severity.PAA.normref))[,4])
## mild/moderate as reference
contrasts(cardiac_asthma$severe2_v3_f) <- contr.treatment(3, base=2)
severity.PAA.mmref <- lm(PAA_ratio ~ severe2_v3_f + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                           preFEV1_Quanpct_pred + bmi + sysbp, data=subset(cardiac_asthma, emphysema==0))
cbind(effect = coef(severity.PAA.mmref), 
      confint(severity.PAA.mmref), 
      p = coef(summary(severity.PAA.mmref))[,4])

##########RVLV Entire Cohort############################################################################################
contrasts(cardiac_asthma$severe2_v3_f) <- contr.treatment(3, base=1)
severity.RVLV.normref <- lm(RV_LV_epi_ratio ~ severe2_v3_f + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                              preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(severity.RVLV.normref), 
      confint(severity.RVLV.normref), 
      p = coef(summary(severity.RVLV.normref))[,4])
## mild/moderate as reference
contrasts(cardiac_asthma$severe2_v3_f) <- contr.treatment(3, base=2)
severity.RVLV.mmref <- lm(RV_LV_epi_ratio ~ severe2_v3_f + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                            preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(severity.RVLV.mmref), 
      confint(severity.RVLV.mmref), 
      p = coef(summary(severity.RVLV.mmref))[,4])

##############RVLV in those with emphysema excluded############################################################################################
## healthy control as reference
contrasts(cardiac_asthma$severe2_v3_f) <- contr.treatment(3, base=1)
severity.RVLV.normref <- lm(RV_LV_epi_ratio ~ severe2_v3_f + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                              preFEV1_Quanpct_pred + bmi + sysbp, data=subset(cardiac_asthma, emphysema==0))
cbind(effect = coef(severity.RVLV.normref), 
      confint(severity.RVLV.normref), 
      p = coef(summary(severity.RVLV.normref))[,4])
## mild/moderate as reference
contrasts(cardiac_asthma$severe2_v3_f) <- contr.treatment(3, base=2)
severity.RVLV.mmref <- lm(RV_LV_epi_ratio ~ severe2_v3_f + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                            preFEV1_Quanpct_pred + bmi + sysbp, data=subset(cardiac_asthma, emphysema==0))
cbind(effect = coef(severity.RVLV.mmref), 
      confint(severity.RVLV.mmref), 
      p = coef(summary(severity.RVLV.mmref))[,4])

##############LV entire cohort###########################################################################################################################################
## healthy control as reference
contrasts(cardiac_asthma$severe2_v3_f) <- contr.treatment(3, base=1)
severity.LV.normref <- lm(vol_epi_LV_BSAI ~ severe2_v3_f + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                            preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(severity.LV.normref), 
      confint(severity.LV.normref), 
      p = coef(summary(severity.LV.normref))[,4])
## mild/moderate as reference
contrasts(cardiac_asthma$severe2_v3_f) <- contr.treatment(3, base=2)
severity.LV.mmref <- lm(vol_epi_LV_BSAI ~ severe2_v3_f + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                          preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(severity.LV.mmref), 
      confint(severity.LV.mmref), 
      p = coef(summary(severity.LV.mmref))[,4])

##############LV in those with emphysema excluded#################################################################################################################
## healthy control as reference
contrasts(cardiac_asthma$severe2_v3_f) <- contr.treatment(3, base=1)
severity.LV.normref <- lm(vol_epi_LV_BSAI ~ severe2_v3_f + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                            preFEV1_Quanpct_pred + bmi + sysbp, data=subset(cardiac_asthma, emphysema==0))
cbind(effect = coef(severity.LV.normref), 
      confint(severity.LV.normref), 
      p = coef(summary(severity.LV.normref))[,4])
## mild/moderate as reference
contrasts(cardiac_asthma$severe2_v3_f) <- contr.treatment(3, base=2)
severity.LV.mmref <- lm(vol_epi_LV_BSAI ~ severe2_v3_f + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                          preFEV1_Quanpct_pred + bmi + sysbp, data=subset(cardiac_asthma, emphysema==0))
cbind(effect = coef(severity.LV.mmref), 
      confint(severity.LV.mmref), 
      p = coef(summary(severity.LV.mmref))[,4])

##############RV entire cohort#################################################################################################################
## healthy control as reference
contrasts(cardiac_asthma$severe2_v3_f) <- contr.treatment(3, base=1)
severity.RV.normref <- lm(vol_epi_RV_BSAI ~ severe2_v3_f + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                            preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(severity.RV.normref), 
      confint(severity.RV.normref), 
      p = coef(summary(severity.RV.normref))[,4])
## mild/moderate as reference
contrasts(cardiac_asthma$severe2_v3_f) <- contr.treatment(3, base=2)
severity.RV.mmref <- lm(vol_epi_RV_BSAI ~ severe2_v3_f + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                          preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(severity.RV.mmref), 
      confint(severity.RV.mmref), 
      p = coef(summary(severity.RV.mmref))[,4])

##############RV in those with emphysema excluded#################################################################################################################
## healthy control as reference
contrasts(cardiac_asthma$severe2_v3_f) <- contr.treatment(3, base=1)
severity.RV.normref <- lm(vol_epi_RV_BSAI ~ severe2_v3_f + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                            preFEV1_Quanpct_pred + bmi + sysbp, data=subset(cardiac_asthma, emphysema==0))
cbind(effect = coef(severity.RV.normref), 
      confint(severity.RV.normref), 
      p = coef(summary(severity.RV.normref))[,4])
## mild/moderate as reference
contrasts(cardiac_asthma$severe2_v3_f) <- contr.treatment(3, base=2)
severity.RV.mmref <- lm(vol_epi_RV_BSAI ~ severe2_v3_f + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                          preFEV1_Quanpct_pred + bmi + sysbp, data=subset(cardiac_asthma, emphysema==0))
cbind(effect = coef(severity.RV.mmref), 
      confint(severity.RV.mmref), 
      p = coef(summary(severity.RV.mmref))[,4])

##############TVV entire cohort#################################################################################################################
## healthy control as reference
contrasts(cardiac_asthma$severe2_v3_f) <- contr.treatment(3, base=1)
severity.TVV.normref <- lm(RV_LV_epi_vol_BSAI ~ severe2_v3_f + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                             preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(severity.TVV.normref), 
      confint(severity.TVV.normref), 
      p = coef(summary(severity.TVV.normref))[,4])

## mild/moderate as reference
contrasts(cardiac_asthma$severe2_v3_f) <- contr.treatment(3, base=2)
severity.TVV.mmref <- lm(RV_LV_epi_vol_BSAI ~ severe2_v3_f + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                           preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(severity.TVV.mmref), 
      confint(severity.TVV.mmref), 
      p = coef(summary(severity.TVV.mmref))[,4])

##############TVV in those with emphysema excluded#################################################################################################################
## healthy control as reference
contrasts(cardiac_asthma$severe2_v3_f) <- contr.treatment(3, base=1)
severity.TVV.normref <- lm(RV_LV_epi_vol_BSAI ~ severe2_v3_f + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                             preFEV1_Quanpct_pred + bmi + sysbp, data=subset(cardiac_asthma, emphysema==0))
cbind(effect = coef(severity.TVV.normref), 
      confint(severity.TVV.normref), 
      p = coef(summary(severity.TVV.normref))[,4])
## mild/moderate as reference
contrasts(cardiac_asthma$severe2_v3_f) <- contr.treatment(3, base=2)
severity.TVV.mmref <- lm(RV_LV_epi_vol_BSAI ~ severe2_v3_f + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                           preFEV1_Quanpct_pred + bmi + sysbp, data=subset(cardiac_asthma, emphysema==0))
cbind(effect = coef(severity.TVV.mmref), 
      confint(severity.TVV.mmref), 
      p = coef(summary(severity.TVV.mmref))[,4])
