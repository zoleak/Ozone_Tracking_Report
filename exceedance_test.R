




test<-NJ_area_daily_max
colnames(test)[252]<-"Max"



test3<-wide_to_long_NJ%>%
  dplyr::filter(AQI_value >70,!is.na(Date))%>%
  dplyr::group_by(`Site Name`)%>%
  dplyr::mutate(`Daily Exceedance` = n())#%>%
 # dplyr::distinct(`Site Name`,.keep_all= T)


show<-NJ_area_daily_max%>%
  #mutate(Exceed = rowSums(select(., 7:252) > 70, na.rm = TRUE))
  add_column(Exceed = rowSums(.[7:251] > 70,na.rm = TRUE), .after = "County")
 

#### has 1 extra added because of daily max row ; need to fix this
