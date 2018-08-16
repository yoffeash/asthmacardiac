### FENO and eosinophilia ###

### univariate correlations ###
###PAA ratio###
with(cardiac_asthma, cor.test(feno,PAA_ratio,method="pearson"))
with(cardiac_asthma, cor.test(IGE,PAA_ratio,method="pearson"))
with(cardiac_asthma, cor.test(sput_eos_percent,PAA_ratio,method="pearson"))
with(cardiac_asthma, cor.test(peripheral_eos_percent,PAA_ratio,method="pearson"))
###RVLV ratio###
with(cardiac_asthma, cor.test(feno,RV_LV_epi_ratio,method="pearson"))
with(cardiac_asthma, cor.test(IGE,RV_LV_epi_ratio,method="pearson"))
with(cardiac_asthma, cor.test(sput_eos_percent,RV_LV_epi_ratio,method="pearson"))
with(cardiac_asthma, cor.test(peripheral_eos_percent,RV_LV_epi_ratio,method="pearson"))
###RV - BSA normalized###
with(cardiac_asthma, cor.test(feno,vol_epi_RV_BSAI,method="pearson"))
with(cardiac_asthma, cor.test(IGE,vol_epi_RV_BSAI,method="pearson"))
with(cardiac_asthma, cor.test(sput_eos_percent,vol_epi_RV_BSAI,method="pearson"))
with(cardiac_asthma, cor.test(peripheral_eos_percent,vol_epi_RV_BSAI,method="pearson"))
###LV - BSA normalized###
with(cardiac_asthma, cor.test(feno,vol_epi_LV_BSAI,method="pearson"))
with(cardiac_asthma, cor.test(IGE,vol_epi_LV_BSAI,method="pearson"))
with(cardiac_asthma, cor.test(sput_eos_percent,vol_epi_LV_BSAI,method="pearson"))
with(cardiac_asthma, cor.test(peripheral_eos_percent,vol_epi_LV_BSAI,method="pearson"))
###Total Ventricular Volume - BSA normalized###
with(cardiac_asthma, cor.test(feno,RV_LV_epi_vol_BSAI,method="pearson"))
with(cardiac_asthma, cor.test(IGE,RV_LV_epi_vol_BSAI,method="pearson"))
with(cardiac_asthma, cor.test(sput_eos_percent,RV_LV_epi_vol_BSAI,method="pearson"))
with(cardiac_asthma, cor.test(peripheral_eos_percent,RV_LV_epi_vol_BSAI,method="pearson"))

### multivariable association ###
### PAA Ratio ###
# feno
PAA.feno <- lm(feno ~ PAA_ratio + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(PAA.feno), 
      confint(PAA.feno), 
      p = coef(summary(PAA.feno))[,4])
# IGE
PAA.IGE <- lm(IGE ~ PAA_ratio + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                 preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(PAA.IGE), 
      confint(PAA.IGE), 
      p = coef(summary(PAA.IGE))[,4])
# sputum eos
PAA.sput.eos <- lm(sput_eos_percent ~ PAA_ratio + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(PAA.sput.eos), 
      confint(PAA.sput.eos), 
      p = coef(summary(PAA.sput.eos))[,4])
# peripheral eos
PAA.peripheral.eos <- lm(peripheral_eos_percent ~ PAA_ratio + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                     preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(PAA.peripheral.eos), 
      confint(PAA.peripheral.eos), 
      p = coef(summary(PAA.peripheral.eos))[,4])
### RVLV Ratio ###
# feno
RVLV.feno <- lm(feno ~ RV_LV_epi_ratio + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                 preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(RVLV.feno), 
      confint(RVLV.feno), 
      p = coef(summary(RVLV.feno))[,4])
# IGE
RVLV.IGE <- lm(IGE ~ RV_LV_epi_ratio + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(RVLV.IGE), 
      confint(RVLV.IGE), 
      p = coef(summary(RVLV.IGE))[,4])
# sputum eos
RVLV.sput.eos <- lm(sput_eos_percent ~ RV_LV_epi_ratio + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                     preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(RVLV.sput.eos), 
      confint(RVLV.sput.eos), 
      p = coef(summary(RVLV.sput.eos))[,4])
# peripheral eos
RVLV.peripheral.eos <- lm(peripheral_eos_percent ~ RV_LV_epi_ratio + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                           preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(RVLV.peripheral.eos), 
      confint(RVLV.peripheral.eos), 
      p = coef(summary(RVLV.peripheral.eos))[,4])
### RV volume ###
# feno
RV.feno <- lm(feno ~ vol_epi_RV_BSAI + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                  preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(RV.feno), 
      confint(RV.feno), 
      p = coef(summary(RV.feno))[,4])
# IGE
RV.IGE <- lm(IGE ~ vol_epi_RV_BSAI + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                 preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(RV.IGE), 
      confint(RV.IGE), 
      p = coef(summary(RV.IGE))[,4])
# sputum eos
RV.sput.eos <- lm(sput_eos_percent ~ vol_epi_RV_BSAI + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                      preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(RV.sput.eos), 
      confint(RV.sput.eos), 
      p = coef(summary(RV.sput.eos))[,4])
# peripheral eos
RV.peripheral.eos <- lm(peripheral_eos_percent ~ vol_epi_RV_BSAI + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                            preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(RV.peripheral.eos), 
      confint(RV.peripheral.eos), 
      p = coef(summary(RV.peripheral.eos))[,4])
### LV volume ###
# feno
LV.feno <- lm(feno ~ vol_epi_LV_BSAI + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(LV.feno), 
      confint(LV.feno), 
      p = coef(summary(LV.feno))[,4])
# IGE
LV.IGE <- lm(IGE ~ vol_epi_LV_BSAI + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
               preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(LV.IGE), 
      confint(LV.IGE), 
      p = coef(summary(LV.IGE))[,4])
# sputum eos
LV.sput.eos <- lm(sput_eos_percent ~ vol_epi_LV_BSAI + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                    preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(LV.sput.eos), 
      confint(LV.sput.eos), 
      p = coef(summary(LV.sput.eos))[,4])
# peripheral eos
LV.peripheral.eos <- lm(peripheral_eos_percent ~ vol_epi_LV_BSAI + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                          preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(LV.peripheral.eos), 
      confint(LV.peripheral.eos), 
      p = coef(summary(LV.peripheral.eos))[,4])
### totV volume ###
# FENO
totV.feno <- lm(feno ~ RV_LV_epi_vol_BSAI + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(totV.feno), 
      confint(totV.feno), 
      p = coef(summary(totV.feno))[,4])
# IGE
totV.IGE <- lm(IGE ~ RV_LV_epi_vol_BSAI + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
               preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(totV.IGE), 
      confint(totV.IGE), 
      p = coef(summary(totV.IGE))[,4])
# sputum eos
totV.sput.eos <- lm(sput_eos_percent ~ RV_LV_epi_vol_BSAI + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                    preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(totV.sput.eos), 
      confint(totV.sput.eos), 
      p = coef(summary(totV.sput.eos))[,4])
# peripheral eos
totV.peripheral.eos <- lm(peripheral_eos_percent ~ RV_LV_epi_vol_BSAI + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                          preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(totV.peripheral.eos), 
      confint(totV.peripheral.eos), 
      p = coef(summary(totV.peripheral.eos))[,4])
