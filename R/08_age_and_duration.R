### relationship between PAA, RVLV and cardiac measures with age at onset and duration of illness ###
# Notes:
## no clear relationships
options(digits = 3)

### univariate correlations ###
###PAA ratio###
with(cardiac_asthma, cor.test(age_dx,PAA_ratio,method="pearson"))
with(cardiac_asthma, cor.test(duration_dx,PAA_ratio,method="pearson"))
with(cardiac_asthma, cor.test(age_spiro,PAA_ratio,method="pearson"))
###RVLV ratio###
with(cardiac_asthma, cor.test(age_dx,RV_LV_epi_ratio,method="pearson"))
with(cardiac_asthma, cor.test(duration_dx,RV_LV_epi_ratio,method="pearson"))
with(cardiac_asthma, cor.test(age_spiro,RV_LV_epi_ratio,method="pearson"))
###RV - BSA normalized###
with(cardiac_asthma, cor.test(age_dx,vol_epi_RV_BSAI,method="pearson"))
with(cardiac_asthma, cor.test(duration_dx,vol_epi_RV_BSAI,method="pearson"))
with(cardiac_asthma, cor.test(age_spiro,vol_epi_RV_BSAI,method="pearson"))
###LV - BSA normalized###
with(cardiac_asthma, cor.test(age_dx,vol_epi_LV_BSAI,method="pearson"))
with(cardiac_asthma, cor.test(duration_dx,vol_epi_LV_BSAI,method="pearson"))
with(cardiac_asthma, cor.test(age_spiro,vol_epi_LV_BSAI,method="pearson"))
###Total Ventricular Volume - BSA normalized###
with(cardiac_asthma, cor.test(age_dx,RV_LV_epi_vol_BSAI,method="pearson"))
with(cardiac_asthma, cor.test(duration_dx,RV_LV_epi_vol_BSAI,method="pearson"))
with(cardiac_asthma, cor.test(age_spiro,RV_LV_epi_vol_BSAI,method="pearson"))

####duration of disease in those without emphysema####
with(subset(cardiac_asthma, emphysema==0), cor.test(duration_dx,PAA_ratio,method="pearson")$p.value)
with(subset(cardiac_asthma, emphysema==0), cor.test(duration_dx,RV_LV_epi_ratio,method="pearson")$p.value)
with(subset(cardiac_asthma, emphysema==0), cor.test(duration_dx,vol_epi_RV_BSAI,method="pearson")$p.value)
with(subset(cardiac_asthma, emphysema==0), cor.test(duration_dx,vol_epi_LV_BSAI,method="pearson")$p.value)
with(subset(cardiac_asthma, emphysema==0), cor.test(duration_dx,RV_LV_epi_vol_BSAI,method="pearson")$p.value)

### univariate dichotomized at age 13 for onset ###
cardiac_asthma$child_onset_13_label <- ifelse(cardiac_asthma$child_onset_13 == 1, "Onset < 13", "Onset > 13")
###PAA Ratio###
cardiac_asthma %>% drop_na(PAA_ratio,child_onset_13_label) %>% 
  ggplot(aes(child_onset_13_label,PAA_ratio)) + 
  geom_boxplot(fill="gray") + 
  ylab("Pulmonary Artery to Artery Diameter Ratio") +
  xlab("Onset") +
  stat_compare_means(method="t.test")     # Add global p-value
###RVLV Ratio###
cardiac_asthma %>% drop_na(RV_LV_epi_ratio,child_onset_13_label) %>% 
  ggplot(aes(child_onset_13_label,RV_LV_epi_ratio)) + 
  geom_boxplot(fill="gray") + 
  ylab("Right Ventricular to Left Ventricular Volume Ratio") +
  xlab("Onset") +
  stat_compare_means(method="t.test")     # Add global p-value
###RV - BSA normalized###
cardiac_asthma %>% drop_na(vol_epi_RV_BSAI,child_onset_13_label) %>% 
  ggplot(aes(child_onset_13_label,vol_epi_RV_BSAI)) + 
  geom_boxplot(fill="gray") + 
  ylab("Right Ventricular Volume (BSA Normalized)") +
  xlab("Onset") +
  stat_compare_means(method="t.test")     # Add global p-value
###LV - BSA normalized###
cardiac_asthma %>% drop_na(vol_epi_LV_BSAI,child_onset_13_label) %>% 
  ggplot(aes(child_onset_13_label,vol_epi_LV_BSAI)) + 
  geom_boxplot(fill="gray") + 
  ylab("Left Ventricular Volume (BSA Normalized)") +
  xlab("Onset") +
  stat_compare_means(method="t.test")     # Add global p-value
###Total Ventricular Volume - BSA normalized###
cardiac_asthma %>% drop_na(RV_LV_epi_vol_BSAI,child_onset_13_label) %>% 
  ggplot(aes(child_onset_13_label,RV_LV_epi_vol_BSAI)) + 
  geom_boxplot(fill="gray") + 
  ylab("Total Ventricular Volume (BSA Normalized)") +
  xlab("Onset") +
  stat_compare_means(method="t.test")     # Add global p-value

### multivariable associations - continuous ###
## PAA ##
# age
PAA.age <- lm(PAA_ratio ~ age_dx + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                  preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(PAA.age), 
      confint(PAA.age), 
      p = coef(summary(PAA.age))[,4])
# duration
PAA.duration <- lm(PAA_ratio ~ duration_dx + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                  preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(PAA.duration), 
      confint(PAA.duration), 
      p = coef(summary(PAA.duration))[,4])
## RVLV ##
# age
RVLV.age <- lm(RV_LV_epi_ratio ~ age_dx + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                   preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(RVLV.age), 
      confint(RVLV.age), 
      p = coef(summary(RVLV.age))[,4])
# duration
RVLV.duration <- lm(RV_LV_epi_ratio ~ duration_dx + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                   preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(RVLV.duration), 
      confint(RVLV.duration), 
      p = coef(summary(RVLV.duration))[,4])
## RV - BSA normalized ##
# age
RV.age <- lm(vol_epi_RV_BSAI ~ age_dx + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                 preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(RV.age), 
      confint(RV.age), 
      p = coef(summary(RV.age))[,4])
# duration
RV.duration <- lm(vol_epi_RV_BSAI ~ duration_dx + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                 preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(RV.duration), 
      confint(RV.duration), 
      p = coef(summary(RV.duration))[,4])
## LV - BSA normalized ##
# age
LV.age <- lm(vol_epi_LV_BSAI ~ age_dx + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                 preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(LV.age), 
      confint(LV.age), 
      p = coef(summary(LV.age))[,4])
# duration
LV.duration <- lm(vol_epi_LV_BSAI ~ duration_dx + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                 preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(LV.duration), 
      confint(LV.duration), 
      p = coef(summary(LV.duration))[,4])
## totV - BSA normalized ##
# age
totV.age <- lm(RV_LV_epi_vol_BSAI ~ age_dx + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                   preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(totV.age), 
      confint(totV.age), 
      p = coef(summary(totV.age))[,4])
# duration
totV.duration <- lm(RV_LV_epi_vol_BSAI ~ duration_dx + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                   preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(totV.duration), 
      confint(totV.duration), 
      p = coef(summary(totV.duration))[,4])


####Multivariable associations - dichotomized for onset####
## PAA ##
# < 13
PAA.child <- lm(PAA_ratio ~ child_onset_13 + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                             preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(PAA.child), 
      confint(PAA.child), 
      p = coef(summary(PAA.child))[,4])
# < 18
PAA.child <- lm(PAA_ratio ~ child_onset + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                  preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(PAA.child), 
      confint(PAA.child), 
      p = coef(summary(PAA.child))[,4])
## RVLV ##
# < 13
RVLV.child <- lm(RV_LV_epi_ratio ~ child_onset_13 + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                  preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(RVLV.child), 
      confint(RVLV.child), 
      p = coef(summary(RVLV.child))[,4])
# < 18
RVLV.child <- lm(RV_LV_epi_ratio ~ child_onset + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                  preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(RVLV.child), 
      confint(RVLV.child), 
      p = coef(summary(RVLV.child))[,4])
## RV - BSA normalized ##
# < 13
RV.child <- lm(vol_epi_RV_BSAI ~ child_onset_13 + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                   preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(RV.child), 
      confint(RV.child), 
      p = coef(summary(RV.child))[,4])
# < 18
RV.child <- lm(vol_epi_RV_BSAI ~ child_onset + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                   preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(RV.child), 
      confint(RV.child), 
      p = coef(summary(RV.child))[,4])
## LV - BSA normalized ##
# < 13
LV.child <- lm(vol_epi_LV_BSAI ~ child_onset_13 + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                 preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(LV.child), 
      confint(LV.child), 
      p = coef(summary(LV.child))[,4])
# < 18
LV.child <- lm(vol_epi_LV_BSAI ~ child_onset + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                 preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(LV.child), 
      confint(LV.child), 
      p = coef(summary(LV.child))[,4])
## totV - BSA normalized ##
# < 13
totV.child <- lm(RV_LV_epi_vol_BSAI ~ child_onset_13 + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                 preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(totV.child), 
      confint(totV.child), 
      p = coef(summary(totV.child))[,4])
# < 18
totV.child <- lm(RV_LV_epi_vol_BSAI ~ child_onset + age_spiro + male + minority + WholeLung950 + wholelungvolume_ht_m + 
                 preFEV1_Quanpct_pred + bmi + sysbp, data=cardiac_asthma)
cbind(effect = coef(totV.child), 
      confint(totV.child), 
      p = coef(summary(totV.child))[,4])
  