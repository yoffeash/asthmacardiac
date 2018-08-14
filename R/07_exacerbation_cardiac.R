#### exacerbation cardiac #####
##############Exacerbation analyses using fixed effects with BSA standardized cardiac measures (standard zero inflated negative binomial) ########### 

##############Retrospective ##############
##############Entire Cohort ##############
## prediction of retrospective exacerbations based on LV size
LV.exac.retro.multi <- zeroinfl(WG_derived_num_exac12~vol_epi_LV_BSAI_dmed_inv + age_spiro + male + minority + WholeLung950 + 
                                  wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score | vol_epi_LV_BSAI_dmed_inv,
                                data=cardiac_asthma, dist="negbin", EM=TRUE)
summary(LV.exac.retro.multi)
cbind(IRR = exp(coef(LV.exac.retro.multi)), 
      exp(confint(LV.exac.retro.multi)))[2,]

## prediction of retrospective exacerbations based on RV size
RV.exac.retro.multi <- zeroinfl(WG_derived_num_exac12~vol_epi_RV_BSAI_dmed_inv + age_spiro + male + minority + WholeLung950 + 
                                  wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score | vol_epi_RV_BSAI_dmed_inv,
                                data=cardiac_asthma, dist="negbin", EM=TRUE)
summary(RV.exac.retro.multi)
cbind(IRR = exp(coef(RV.exac.retro.multi)), 
      exp(confint(RV.exac.retro.multi)))[2,]

## prediction of retrospective exacerbations based on total ventricular size
totV.exac.retro.multi <- zeroinfl(WG_derived_num_exac12~RV_LV_epi_vol_BSAI_dmed_inv + age_spiro + male + minority + WholeLung950 + 
                                    wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score | RV_LV_epi_vol_BSAI_dmed_inv,
                                  data=cardiac_asthma, dist="negbin", EM=TRUE)
summary(totV.exac.retro.multi)
cbind(IRR = exp(coef(totV.exac.retro.multi)), 
      exp(confint(totV.exac.retro.multi)))[2,]

##############Retrospective ##############
##############Those without emphysema (including all asthmatics) ##############

## prediction of retrospective exacerbations based on LV size
LV.exac.retro.noemph.multi <- zeroinfl(WG_derived_num_exac12~vol_epi_LV_BSAI_dmed_inv + age_spiro + male + minority + WholeLung950 + 
                                         wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score | vol_epi_LV_BSAI_dmed_inv,
                                       data=subset(cardiac_asthma, emphysema==0), dist="negbin", EM=TRUE)
summary(LV.exac.retro.noemph.multi)
cbind(IRR = exp(coef(LV.exac.retro.noemph.multi)), 
      exp(confint(LV.exac.retro.noemph.multi)))[2,]

## prediction of retrospective exacerbations based on RV size
RV.exac.retro.noemph.multi <- zeroinfl(WG_derived_num_exac12~vol_epi_RV_BSAI_dmed_inv + age_spiro + male + minority + WholeLung950 + 
                                         wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score | vol_epi_RV_BSAI_dmed_inv,
                                       data=subset(cardiac_asthma, emphysema==0), dist="negbin", EM=TRUE)
summary(RV.exac.retro.noemph.multi)
cbind(IRR = exp(coef(RV.exac.retro.noemph.multi)), 
      exp(confint(RV.exac.retro.noemph.multi)))[2,]

## prediction of retrospective exacerbations based on total ventricular size
totV.exac.retro.noemph.multi <- zeroinfl(WG_derived_num_exac12~RV_LV_epi_vol_BSAI_dmed_inv + age_spiro + male + minority + WholeLung950 + 
                                           wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score | RV_LV_epi_vol_BSAI_dmed_inv,
                                         data=subset(cardiac_asthma, emphysema==0), dist="negbin", EM=TRUE)
summary(totV.exac.retro.noemph.multi)
cbind(IRR = exp(coef(totV.exac.retro.noemph.multi)), 
      exp(confint(totV.exac.retro.noemph.multi)))[2,]


##############Retrospective ##############
##############In Severe Asthmatics ##############

## prediction of retrospective exacerbations based on LV size
LV.exac.retro.severe.multi <- zeroinfl(WG_derived_num_exac12~vol_epi_LV_BSAI_dmed_inv + age_spiro + male + minority + WholeLung950 + 
                                         wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score | vol_epi_LV_BSAI_dmed_inv,
                                       data=subset(cardiac_asthma, severe2_v3==3), dist="negbin", EM=TRUE)
summary(LV.exac.retro.severe.multi)
cbind(IRR = exp(coef(LV.exac.retro.severe.multi)), 
      exp(confint(totV.exac.retro.noemph.multi)))[2,]


## prediction of retrospective exacerbations based on RV size
RV.exac.retro.severe.multi <- zeroinfl(WG_derived_num_exac12~vol_epi_RV_BSAI_dmed_inv + age_spiro + male + minority + WholeLung950 + 
                                         wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score | vol_epi_RV_BSAI_dmed_inv,
                                       data=subset(cardiac_asthma, severe2_v3==3), dist="negbin", EM=TRUE)
summary(RV.exac.retro.severe.multi)
cbind(IRR = exp(coef(RV.exac.retro.severe.multi)), 
      exp(confint(RV.exac.retro.severe.multi)))[2,]

## prediction of retrospective exacerbations based on total ventricular size
totV.exac.retro.severe.multi <- zeroinfl(WG_derived_num_exac12~RV_LV_epi_vol_BSAI_dmed_inv + age_spiro + male + minority + WholeLung950 + 
                                           wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score | RV_LV_epi_vol_BSAI_dmed_inv,
                                         data=subset(cardiac_asthma, severe2_v3==3), dist="negbin", EM=TRUE)
summary(totV.exac.retro.severe.multi)
cbind(IRR = exp(coef(totV.exac.retro.severe.multi)), 
      exp(confint(totV.exac.retro.severe.multi)))[2,]

##############Retrospective ##############
##############In Severe Asthmatics without emphysema ##############

## prediction of exacerbations based on LV size
LV.exac.retro.severe.noemph.multi <- zeroinfl(WG_derived_num_exac12~vol_epi_LV_BSAI_dmed_inv + age_spiro + male + minority + WholeLung950 + 
                                                wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score | vol_epi_LV_BSAI_dmed_inv,
                                              data=subset(cardiac_asthma, severe2_v3==3 & emphysema==0), dist="negbin", EM=TRUE)
summary(LV.exac.retro.severe.noemph.multi)
cbind(IRR = exp(coef(LV.exac.retro.severe.noemph.multi)), 
      exp(confint(LV.exac.retro.severe.noemph.multi)))[2,]


## prediction of exacerbations based on RV size
RV.exac.retro.severe.noemph.multi <- zeroinfl(WG_derived_num_exac12~vol_epi_RV_BSAI_dmed_inv + age_spiro + male + minority + WholeLung950 + 
                                                wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score | vol_epi_RV_BSAI_dmed_inv,
                                              data=subset(cardiac_asthma, severe2_v3==3 & emphysema==0), dist="negbin", EM=TRUE)
summary(RV.exac.retro.severe.noemph.multi)
cbind(IRR = exp(coef(RV.exac.retro.severe.noemph.multi)), 
      exp(confint(RV.exac.retro.severe.noemph.multi)))[2,]

## prediction of exacerbations based on total ventricular size
totV.exac.retro.severe.noemph.multi <- zeroinfl(WG_derived_num_exac12~RV_LV_epi_vol_BSAI_dmed_inv + age_spiro + male + minority + WholeLung950 + 
                                                  wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score | RV_LV_epi_vol_BSAI_dmed_inv,
                                                data=subset(cardiac_asthma, severe2_v3==3 & emphysema==0), dist="negbin", EM=TRUE)
summary(totV.exac.retro.severe.noemph.multi)
cbind(IRR = exp(coef(totV.exac.retro.severe.noemph.multi)), 
      exp(confint(totV.exac.retro.severe.noemph.multi)))[2,]

##############Prospective ##############
##############Entire Cohort ##############

## prediction of prospective exacerbations based on LV size
LV.exac.pro.multi <- zeroinfl(exac_total~vol_epi_LV_BSAI_dmed_inv + age_spiro + male + minority + WholeLung950 + 
                                wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score + retroexacerbation
                              | vol_epi_LV_BSAI_dmed_inv,
                              data=cardiac_asthma, dist="negbin", offset=lyears, link="logit", EM=TRUE)
summary(LV.exac.pro.multi)
cbind(IRR = exp(coef(LV.exac.pro.multi)), 
      exp(confint(LV.exac.pro.multi)))[2,]

## prediction of prospective exacerbations based on RV size
RV.exac.pro.multi <- zeroinfl(exac_total~vol_epi_RV_BSAI_dmed_inv + age_spiro + male + minority + WholeLung950 + 
                                wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score + retroexacerbation
                              | vol_epi_RV_BSAI_dmed_inv,
                              data=cardiac_asthma, dist="negbin", offset=lyears, link="logit", EM=TRUE)
summary(RV.exac.pro.multi)
cbind(IRR = exp(coef(RV.exac.pro.multi)), 
      exp(confint(RV.exac.pro.multi)))[2,]

## prediction of prospective exacerbations based on total ventricular size
totV.exac.pro.multi <- zeroinfl(exac_total~RV_LV_epi_vol_BSAI_dmed_inv + age_spiro + male + minority + WholeLung950 + 
                                  wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score + retroexacerbation
                                | RV_LV_epi_vol_BSAI_dmed_inv,
                                data=cardiac_asthma, dist="negbin", offset=lyears, link="logit", EM=TRUE)
summary(totV.exac.pro.multi)
cbind(IRR = exp(coef(totV.exac.pro.multi)), 
      exp(confint(totV.exac.pro.multi)))[2,]

##############Prospective ##############
##############Those without emphysema ##############

## prediction of prospective exacerbations based on LV size
LV.exac.pro.noemph.multi <- zeroinfl(exac_total~vol_epi_LV_BSAI_dmed_inv + age_spiro + male + minority + WholeLung950 + 
                                wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score + retroexacerbation
                              | vol_epi_LV_BSAI_dmed_inv,
                              data=subset(cardiac_asthma, emphysema==0), dist="negbin", offset=lyears, link="logit", EM=TRUE)
summary(LV.exac.pro.noemph.multi)
cbind(IRR = exp(coef(LV.exac.pro.noemph.multi)), 
      exp(confint(LV.exac.pro.noemph.multi)))[2,]

## prediction of prospective exacerbations based on RV size
RV.exac.pro.noemph.multi <- zeroinfl(exac_total~vol_epi_RV_BSAI_dmed_inv + age_spiro + male + minority + WholeLung950 + 
                                wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score + retroexacerbation
                              | vol_epi_RV_BSAI_dmed_inv,
                              data=subset(cardiac_asthma, emphysema==0), dist="negbin", offset=lyears, link="logit", EM=TRUE)
summary(RV.exac.pro.noemph.multi)
cbind(IRR = exp(coef(RV.exac.pro.noemph.multi)), 
      exp(confint(RV.exac.pro.noemph.multi)))[2,]

## prediction of prospective exacerbations based on total ventricular size
totV.exac.pro.noemph.multi <- zeroinfl(exac_total~RV_LV_epi_vol_BSAI_dmed_inv + age_spiro + male + minority + WholeLung950 + 
                                  wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score + retroexacerbation
                                | RV_LV_epi_vol_BSAI_dmed_inv,
                                data=subset(cardiac_asthma, emphysema==0), dist="negbin", offset=lyears, link="logit", EM=TRUE)
summary(totV.exac.pro.noemph.multi)
cbind(IRR = exp(coef(totV.exac.pro.noemph.multi)), 
      exp(confint(totV.exac.pro.noemph.multi)))[2,]

##############Prospective ##############
##############Those with severe asthma ##############

## prediction of prospective exacerbations based on LV size
LV.exac.pro.severe.multi <- zeroinfl(exac_total~vol_epi_LV_BSAI_dmed_inv + age_spiro + male + minority + WholeLung950 + 
                                       wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score + retroexacerbation
                                     | vol_epi_LV_BSAI_dmed_inv,
                                     data=subset(cardiac_asthma, severe2_v3==3), dist="negbin", offset=lyears, link="logit", EM=TRUE)
summary(LV.exac.pro.severe.multi)
cbind(IRR = exp(coef(LV.exac.pro.severe.multi)), 
      exp(confint(LV.exac.pro.severe.multi)))[2,]

## prediction of prospective exacerbations based on RV size
RV.exac.pro.severe.multi <- zeroinfl(exac_total~vol_epi_RV_BSAI_dmed_inv + age_spiro + male + minority + WholeLung950 + 
                                       wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score + retroexacerbation
                                     | vol_epi_RV_BSAI_dmed_inv,
                                     data=subset(cardiac_asthma, severe2_v3==3), dist="negbin", offset=lyears, link="logit", EM=TRUE)
summary(RV.exac.pro.severe.multi)
cbind(IRR = exp(coef(RV.exac.pro.severe.multi)), 
      exp(confint(RV.exac.pro.severe.multi)))[2,]

## prediction of prospective exacerbations based on total ventricular size
totV.exac.pro.severe.multi <- zeroinfl(exac_total~RV_LV_epi_vol_BSAI_dmed_inv + age_spiro + male + minority + WholeLung950 + 
                                         wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score + retroexacerbation
                                       | RV_LV_epi_vol_BSAI_dmed_inv,
                                       data=subset(cardiac_asthma, severe2_v3==3), dist="negbin", offset=lyears, link="logit", EM=TRUE)
summary(totV.exac.pro.severe.multi)
cbind(IRR = exp(coef(totV.exac.pro.severe.multi)), 
      exp(confint(totV.exac.pro.severe.multi)))[2,]

##############Prospective ##############
##############severe asthma without emphysema ##############

## prediction of prospective exacerbations based on LV size
LV.exac.pro.severe.noemph.multi <- zeroinfl(exac_total~vol_epi_LV_BSAI_dmed_inv + age_spiro + male + minority + WholeLung950 + 
                                       wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score + retroexacerbation
                                     | vol_epi_LV_BSAI_dmed_inv,
                                     data=subset(cardiac_asthma, severe2_v3==3 & emphysema==0), dist="negbin", offset=lyears, link="logit", EM=TRUE)
summary(LV.exac.pro.severe.noemph.multi)
cbind(IRR = exp(coef(LV.exac.pro.severe.noemph.multi)), 
      exp(confint(LV.exac.pro.severe.noemph.multi)))[2,]

## prediction of prospective exacerbations based on RV size
RV.exac.pro.severe.noemph.multi <- zeroinfl(exac_total~vol_epi_RV_BSAI_dmed_inv + age_spiro + male + minority + WholeLung950 + 
                                       wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score + retroexacerbation
                                     | vol_epi_RV_BSAI_dmed_inv,
                                     data=subset(cardiac_asthma, severe2_v3==3 & emphysema==0), dist="negbin", offset=lyears, link="logit", EM=TRUE)
summary(RV.exac.pro.severe.noemph.multi)
cbind(IRR = exp(coef(RV.exac.pro.severe.noemph.multi)), 
      exp(confint(RV.exac.pro.severe.noemph.multi)))[2,]

## prediction of prospective exacerbations based on total ventricular size
totV.exac.pro.severe.noemph.multi <- zeroinfl(exac_total~RV_LV_epi_vol_BSAI_dmed_inv + age_spiro + male + minority + WholeLung950 + 
                                         wholelungvolume_ht_m + preFEV1_Quanpct_pred + bmi + sysbp + act_score + retroexacerbation
                                       | RV_LV_epi_vol_BSAI_dmed_inv,
                                       data=subset(cardiac_asthma, severe2_v3==3 & emphysema==0), dist="negbin", offset=lyears, link="logit", EM=TRUE)
summary(totV.exac.pro.severe.noemph.multi)
cbind(IRR = exp(coef(totV.exac.pro.severe.noemph.multi)), 
      exp(confint(totV.exac.pro.severe.noemph.multi)))[2,]


