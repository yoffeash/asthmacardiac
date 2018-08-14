### baseline characteristics ###
#############Updated Table 1 #######################################################################################
##BSA based cardiac measurement##
stat.desc(cardiac_asthma$vol_epi_RV_BSAI)
stat.desc(cardiac_asthma$vol_epi_LV_BSAI)
stat.desc(cardiac_asthma$RV_LV_epi_vol_BSAI)

##heart rate##
stat.desc(cardiac_asthma$heart_rate)
length(na.omit(cardiac_asthma$heart_rate))

##age at diagnosis##
stat.desc(cardiac_asthma$age_dx)
length(na.omit(cardiac_asthma$age_dx))