### exacerbation analyses - PAA and RVLV only ###

########################################entire cohort#######################################################

####PAA, entire cohort, retrospective####
PAA.exac.retro.multi <- zeroinfl(WG_derived_num_exac12~PAA_ratio_d + age_spiro + male + minority + WholeLung950 + 
                                  wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score | PAA_ratio_d,
                                data=cardiac_asthma, dist="negbin", EM=TRUE)
summary(PAA.exac.retro.multi)
cbind(IRR = exp(coef(PAA.exac.retro.multi)), 
      exp(confint(PAA.exac.retro.multi)))[2,]

####RVLV, entire cohort, retrospective####
RVLV.exac.retro.multi <- zeroinfl(WG_derived_num_exac12~RV_LV_epi_ratio_dmed + age_spiro + male + minority + WholeLung950 + 
                                   wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score | RV_LV_epi_ratio_dmed,
                                 data=cardiac_asthma, dist="negbin", EM=TRUE)
summary(RVLV.exac.retro.multi)
cbind(IRR = exp(coef(RVLV.exac.retro.multi)), 
      exp(confint(RVLV.exac.retro.multi)))[2,]

####PAA, entire cohort, prospective####
PAA.exac.pro.multi <- zeroinfl(exac_total~PAA_ratio_d + age_spiro + male + minority + WholeLung950 + 
                                   wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score + retroexacerbation | PAA_ratio_d,
                                 data=cardiac_asthma, dist="negbin", offset=lyears, EM=TRUE)
summary(PAA.exac.pro.multi)
cbind(IRR = exp(coef(PAA.exac.pro.multi)), 
      exp(confint(PAA.exac.pro.multi)))[2,]

####RVLV, entire cohort, prospective####
RVLV.exac.pro.multi <- zeroinfl(exac_total~RV_LV_epi_ratio_dmed + age_spiro + male + minority + WholeLung950 + 
                                    wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score + retroexacerbation | RV_LV_epi_ratio_dmed,
                                  data=cardiac_asthma, dist="negbin", offset=lyears, EM=TRUE)
summary(RVLV.exac.pro.multi)
cbind(IRR = exp(coef(RVLV.exac.pro.multi)), 
      exp(confint(RVLV.exac.pro.multi)))[2,]

#####################################those without emphysema################################################
####PAA, those without emphysema, retrospective####
PAA.exac.retro.multi <- zeroinfl(WG_derived_num_exac12~PAA_ratio_d + age_spiro + male + minority + WholeLung950 + 
                                   wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score | PAA_ratio_d,
                                 data=subset(cardiac_asthma, emphysema==0), dist="negbin", EM=TRUE)
summary(PAA.exac.retro.multi)
cbind(IRR = exp(coef(PAA.exac.retro.multi)), 
      exp(confint(PAA.exac.retro.multi)))[2,]

####RVLV, those without emphysema, retrospective####
RVLV.exac.retro.multi <- zeroinfl(WG_derived_num_exac12~RV_LV_epi_ratio_dmed + age_spiro + male + minority + WholeLung950 + 
                                    wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score | RV_LV_epi_ratio_dmed,
                                  data=subset(cardiac_asthma, emphysema==0), dist="negbin", EM=TRUE)
summary(RVLV.exac.retro.multi)
cbind(IRR = exp(coef(RVLV.exac.retro.multi)), 
      exp(confint(RVLV.exac.retro.multi)))[2,]

####PAA, those without emphysema, prospective####
PAA.exac.pro.multi <- zeroinfl(exac_total~PAA_ratio_d + age_spiro + male + minority + WholeLung950 + 
                                 wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score + retroexacerbation | PAA_ratio_d,
                               data=subset(cardiac_asthma, emphysema==0), dist="negbin", offset=lyears, EM=TRUE)
summary(PAA.exac.pro.multi)
cbind(IRR = exp(coef(PAA.exac.pro.multi)), 
      exp(confint(PAA.exac.pro.multi)))[2,]

####RVLV, those without emphysema, prospective####
RVLV.exac.pro.multi <- zeroinfl(exac_total~RV_LV_epi_ratio_dmed + age_spiro + male + minority + WholeLung950 + 
                                  wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score + retroexacerbation | RV_LV_epi_ratio_dmed,
                                data=subset(cardiac_asthma, emphysema==0), dist="negbin", offset=lyears, EM=TRUE)
summary(RVLV.exac.pro.multi)
cbind(IRR = exp(coef(RVLV.exac.pro.multi)), 
      exp(confint(RVLV.exac.pro.multi)))[2,]

##################################those with severe asthma only##############################################################

####PAA, severe asthma only, retrospective####
PAA.exac.retro.multi <- zeroinfl(WG_derived_num_exac12~PAA_ratio_d + age_spiro + male + minority + WholeLung950 + 
                                   wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score | PAA_ratio_d,
                                 data=subset(cardiac_asthma, severe2_v3==3), dist="negbin", EM=TRUE)
summary(PAA.exac.retro.multi)
cbind(IRR = exp(coef(PAA.exac.retro.multi)), 
      exp(confint(PAA.exac.retro.multi)))[2,]

####RVLV, severe asthma only, retrospective####
RVLV.exac.retro.multi <- zeroinfl(WG_derived_num_exac12~RV_LV_epi_ratio_dmed + age_spiro + male + minority + WholeLung950 + 
                                    wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score | RV_LV_epi_ratio_dmed,
                                  data=subset(cardiac_asthma, severe2_v3==3), dist="negbin", EM=TRUE)
summary(RVLV.exac.retro.multi)
cbind(IRR = exp(coef(RVLV.exac.retro.multi)), 
      exp(confint(RVLV.exac.retro.multi)))[2,]

####PAA, severe asthma only, prospective####
PAA.exac.pro.multi <- zeroinfl(exac_total~PAA_ratio_d + age_spiro + male + minority + WholeLung950 + 
                                 wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score + retroexacerbation | PAA_ratio_d,
                               data=subset(cardiac_asthma, severe2_v3==3), dist="negbin", offset=lyears, EM=TRUE)
summary(PAA.exac.pro.multi)
cbind(IRR = exp(coef(PAA.exac.pro.multi)), 
      exp(confint(PAA.exac.pro.multi)))[2,]

####RVLV, severe asthma only, prospective####
RVLV.exac.pro.multi <- zeroinfl(exac_total~RV_LV_epi_ratio_dmed + age_spiro + male + minority + WholeLung950 + 
                                  wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score + retroexacerbation | RV_LV_epi_ratio_dmed,
                                data=subset(cardiac_asthma, severe2_v3==3), dist="negbin", offset=lyears, EM=TRUE)
summary(RVLV.exac.pro.multi)
cbind(IRR = exp(coef(RVLV.exac.pro.multi)), 
      exp(confint(RVLV.exac.pro.multi)))[2,]

################################those with severe asthma and without emphysema only###############################################
####PAA, those with severe asthma and without emphysema, retrospective####
PAA.exac.retro.multi <- zeroinfl(WG_derived_num_exac12~PAA_ratio_d + age_spiro + male + minority + WholeLung950 + 
                                   wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score | PAA_ratio_d,
                                 data=subset(cardiac_asthma, severe2_v3==3 & emphysema==0), dist="negbin", EM=TRUE)
summary(PAA.exac.retro.multi)
cbind(IRR = exp(coef(PAA.exac.retro.multi)), 
      exp(confint(PAA.exac.retro.multi)))[2,]

####RVLV, those with severe asthma and without emphysema, retrospective####
RVLV.exac.retro.multi <- zeroinfl(WG_derived_num_exac12~RV_LV_epi_ratio_dmed + age_spiro + male + minority + WholeLung950 + 
                                    wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score | RV_LV_epi_ratio_dmed,
                                  data=subset(cardiac_asthma, severe2_v3==3 & emphysema==0), dist="negbin", EM=TRUE)
summary(RVLV.exac.retro.multi)
cbind(IRR = exp(coef(RVLV.exac.retro.multi)), 
      exp(confint(RVLV.exac.retro.multi)))[2,]

####PAA, those with severe asthma and without emphysema, prospective####
PAA.exac.pro.multi <- zeroinfl(exac_total~PAA_ratio_d + age_spiro + male + minority + WholeLung950 + 
                                 wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score + retroexacerbation | PAA_ratio_d,
                               data=subset(cardiac_asthma, severe2_v3==3 & emphysema==0), dist="negbin", offset=lyears, EM=TRUE)
summary(PAA.exac.pro.multi)
cbind(IRR = exp(coef(PAA.exac.pro.multi)), 
      exp(confint(PAA.exac.pro.multi)))[2,]

####RVLV, those with severe asthma and without emphysema, prospective####
RVLV.exac.pro.multi <- zeroinfl(exac_total~RV_LV_epi_ratio_dmed + age_spiro + male + minority + WholeLung950 + 
                                  wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score + retroexacerbation | RV_LV_epi_ratio_dmed,
                                data=subset(cardiac_asthma, severe2_v3==3 & emphysema==0), dist="negbin", offset=lyears, EM=TRUE)
summary(RVLV.exac.pro.multi)
cbind(IRR = exp(coef(RVLV.exac.pro.multi)), 
      exp(confint(RVLV.exac.pro.multi)))[2,]


