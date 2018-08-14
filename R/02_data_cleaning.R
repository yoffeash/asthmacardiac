### data cleaning ###

## import data into r
cardiac_asthma <- read_excel("data/raw_data/SARPcardiaclong_2018_05_25.xlsx")

## add log years for scale factor
cardiac_asthma <- mutate(cardiac_asthma, lyears = log(years))

## height in CM
cardiac_asthma <- mutate(cardiac_asthma, height_cm=height_m*100)

## height in inches
cardiac_asthma <- mutate(cardiac_asthma, height_in=height_cm*0.393701)

## duration of disease
cardiac_asthma <- mutate(cardiac_asthma, duration_dx=age_spiro - age_dx)

## define childhood onset of disease
cardiac_asthma$child_onset <- ifelse(cardiac_asthma$age_dx<18,1,0)
cardiac_asthma$child_onset_13 <- ifelse(cardiac_asthma$age_dx<13,1,0)

## calcuated IBW using Devine
cardiac_asthma$IBW=0
if(cardiac_asthma$male==0 && cardiac_asthma$height_cm <= 152) {
  cardiac_asthma$IBW=45.5
} else if(cardiac_asthma$male==1 && cardiac_asthma$height_cm <= 152) {
  cardiac_asthma$IBW=50
} else if(cardiac_asthma$male==0 && cardiac_asthma$height_cm > 152) {
  cardiac_asthma$IBW=45.5+0.9*(cardiac_asthma$height_cm-152)
} else if(cardiac_asthma$male==1 && cardiac_asthma$height_cm > 152) {
  cardiac_asthma$IBW=50+0.9*(cardiac_asthma$height_cm-152)
}

## calculate BSA by Mosteller using IBW
cardiac_asthma <- mutate(cardiac_asthma, BSAI=sqrt(height_m*100*IBW/3600))

## normalize ventricular measures by BSAI
cardiac_asthma <- mutate(cardiac_asthma, vol_epi_RV_BSAI = vol_epi_RV/BSAI)
cardiac_asthma <- mutate(cardiac_asthma, vol_epi_LV_BSAI = vol_epi_lv/BSAI)
cardiac_asthma <- mutate(cardiac_asthma, RV_LV_epi_vol_BSAI = RV_LV_epi_vol/BSAI)


## dichotomize ventricular measures normalized by BSAI
cardiac_asthma <- mutate(cardiac_asthma, vol_epi_RV_BSAI_dmed = ntile(vol_epi_RV_BSAI, 2))
cardiac_asthma$vol_epi_RV_BSAI_dmed <- with(cardiac_asthma, ifelse(vol_epi_RV_BSAI_dmed==1, 0, 1))

cardiac_asthma <- mutate(cardiac_asthma, vol_epi_LV_BSAI_dmed = ntile(vol_epi_LV_BSAI, 2))
cardiac_asthma$vol_epi_LV_BSAI_dmed <- with(cardiac_asthma, ifelse(vol_epi_LV_BSAI_dmed==1, 0, 1))

cardiac_asthma <- mutate(cardiac_asthma, RV_LV_epi_vol_BSAI_dmed = ntile(RV_LV_epi_vol_BSAI, 2))
cardiac_asthma$RV_LV_epi_vol_BSAI_dmed <- with(cardiac_asthma, ifelse(RV_LV_epi_vol_BSAI_dmed==1, 0, 1))

## invert BSAI normalized ventricular dichotomization so that those who have a small ventricles are 1s and those who have big ventricles are 0
cardiac_asthma$vol_epi_RV_BSAI_dmed_inv <- ifelse(cardiac_asthma$vol_epi_RV_BSAI_dmed==1,0,1)
cardiac_asthma$vol_epi_LV_BSAI_dmed_inv <- ifelse(cardiac_asthma$vol_epi_LV_BSAI_dmed==1,0,1)
cardiac_asthma$RV_LV_epi_vol_BSAI_dmed_inv <- ifelse(cardiac_asthma$RV_LV_epi_vol_BSAI_dmed==1,0,1)

## define emphysema as PSE or CLE
cardiac_asthma$emphysema <- with(cardiac_asthma, ifelse(PSE==1 | CLE==1, 1, 0))