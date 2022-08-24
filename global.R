### Read in Sites Data to plot sites on leaflet map ###
sites<-read_xlsx("Site_Locations.xlsx",col_names = T)%>%
  dplyr::distinct()
### Read in New Jersey County Shapefile ###
nj_counties<-st_read(getwd(),"New_Jersey_Counties")
### Change projection to work with leaflet map ###
nj_counties<-st_transform(nj_counties, crs="+init=epsg:4326")
### Read in PA County Shapefile ###
### Create vector of the counties we want so we can filter shapefile ###
tst<-c("BUCKS","CHESTER","PHILADELPHIA",
       "MONTGOMERY","DELAWARE")
pa_counties<-st_read(getwd(),"PaCounty2019_07")%>%
  dplyr::filter(COUNTY_NAM %in% tst)
### Change projection to work with leaflet map ###
pa_counties<-st_transform(pa_counties,crs="+init=epsg:4326")
### Read in CT County Shapefiles ###
### Create vector of the counties we want so we can filter shapefile ###
tst2<-c("Middlesex","Fairfield","New Haven")
ct_counties<-st_read(getwd(),"ct_county")%>%
  dplyr::filter(NAME10 %in% tst2)
### Change projection to work with leaflet map ###
ct_counties<-st_transform(ct_counties,crs="+init=epsg:4326")
### Read in Delaware county shapefile ###
del_counties<-st_read(getwd(),"delaware")
### Change projection to work with leaflet map ###
del_counties<-st_transform(del_counties,crs="+init=epsg:4326")
### Read in New York County Shapefile ###
### Create vector of the counties we wants so we can fitler shapefile ###
tst3<-c("New York","Suffolk","Bronx","Queens","Rockland","Richmond","Westchester")
ny_counties<-st_read(getwd(),"Counties")%>%
  dplyr::filter(NAME %in% tst3)
### Change projection to work with leaflet map ###
ny_counties<-st_transform(ny_counties,crs="+init=epsg:4326")
### Read in NJ ozone area daily max spreadsheet ###
NJ_area_daily_max<-read_xlsx("NJ Ozone 2022_KZ.xlsx",sheet = "DailyMax",skip = 3)%>%
  dplyr::slice(1:18)%>%
  setNames(., c("AQS Code","Latitude","State","Site Name","Elev (m)","County",
                format(as.Date(as.numeric(names(.)[-1:-6]), 
                               origin = '1899-12-30'), '%m/%d/%Y')))
### Make all date columns numeric ###
NJ_area_daily_max[,7:252] <- sapply(NJ_area_daily_max[,7:252],as.numeric)

NJ_area_daily_max_ex<-NJ_area_daily_max%>%
  dplyr::slice(1:17)%>%
  add_column(`Number of Days Exceeded` = rowSums(.[8:251] > 70, na.rm= TRUE), .after = "County")


### Make column name for last column that keeps reading in blank ### 253
colnames(NJ_area_daily_max_ex)[8]<-"Max"
### Get rid of lat column###
NJ_area_daily_max_ex<-NJ_area_daily_max_ex%>%
  dplyr::select(-Latitude)

### Make another dataframe that makes wide to long ###
wide_to_long_NJ<-NJ_area_daily_max%>%
  gather(Date,`Concentration (PPB)`,7:252)%>%
  dplyr::filter(!State == "NA",!Date == "NA",!`Concentration (PPB)` == "NA")%>%
  dplyr::mutate(Date = as.Date(Date,format = "%m/%d/%Y"))%>%
  dplyr::select(-Latitude)

### Read in NJ ozone area design values ###
NJ_design<-read_xlsx("NJ Ozone 2022_KZ.xlsx",sheet = "8Hr Rpt",skip = 3)%>%
  dplyr::select(1:11)%>%
  dplyr::slice(1:19)%>%
  dplyr::rename("State"="...2","Site"="...3",
                "Design Value 2020"="Design Values",
                "Design Value 2021"="...10",
                "Design Value 2022"="...11",
                "AQS ID"='2022',
                "4th Max (ppb)"="...5",
                "4th Max (ppb)"="...6",
                "4th Max (ppb)"="...7",
                "4th Max (ppb)"="...8")
#Needed to make proper table for downloadable excel file
NJ_design_ex<-read_xlsx("NJ_design_ex.xlsx",sheet = "8Hr Rpt",skip = 3)%>%
  dplyr::select(1:11)%>%
  dplyr::slice(1:19)%>%
  dplyr::rename("State"="...2","Site"="...3",
                "Design Value 2020"="Design Values",
                "Design Value 2021"="...10",
                "Design Value 2022"="...11",
                "AQS ID"='2022',
                "4th Max (ppb)"="...5",
                "4th Max (ppb)"="...6",
                "4th Max (ppb)"="...7",
                "4th Max (ppb)"="...8")



#NJ_design_plot<-read_xlsx("NJ_d_plot.xlsx",sheet = "8Hr Rpt",skip = 3)%>%
#  dplyr::select(1:11)%>%
#  dplyr::slice(1:19)%>%
#  dplyr::rename("State"="...2","Site"="...3",
#                "Design Value 2018"="Design Values",
#                "Design Value 2019"="...10",
#                "Design Value 2020"="...11",
#                "AQS ID"='2020')%>%
#  dplyr::rename("2018"="Design Value 2018",
#                "2019"="Design Value 2019",
#                "2020"="Design Value 2020")%>%
#  dplyr::select(Site,'2018','2019','2020')%>%
#  gather(Year,Design_Value,2:4)%>%
#  dplyr::mutate(Design_Value=as.numeric(Design_Value))
### Read in NJ ozone highest levels ###
NJ_highest_levels<-read_xlsx("NJ Ozone 2022_KZ.xlsx",sheet = "NJ-8Hr",skip = 2)%>%
  dplyr::select(5:16)%>%
  dplyr::rename("# of days > 70"="> 70","# of days > 75"="> 75",
                "# of days > 84"="> 84")%>%
  mutate_all(funs(replace_na(.,0)))
#dplyr::slice(1:19)
### Read in NY area daily max spreadsheet ###
NY_area_daily_max<-read_xlsx("NY-Area Ozone 2022 8hr_KZ.xlsx",sheet = "DailyMax",skip = 3)%>%
  dplyr::slice(1:27)%>%
  setNames(., c("AQS Code","Latitude","State","Site Name","Elev (m)","County",
               format(as.Date(as.numeric(names(.)[-1:-6]), 
                               origin = '1899-12-30'), '%m/%d/%Y')))
### Make all date columns numeric ###
NY_area_daily_max[,7:252] <- sapply(NY_area_daily_max[,7:252],as.numeric)

NY_area_daily_max_ex<-NY_area_daily_max%>%
  dplyr::slice(1:27)%>%
  add_column(`Number of Days Exceeded` = rowSums(.[8:251] > 70, na.rm= TRUE), .after = "County")

### Make column name for last column that keeps reading in blank ###
#colnames(NY_area_daily_max)[252]<-"Max"
colnames(NY_area_daily_max_ex)[8]<-"Max"

### Get rid of lat column###
NY_area_daily_max_ex<-NY_area_daily_max_ex%>%
  dplyr::select(-Latitude)

### Make another dataframe that makes wide to long ###
wide_to_long_NY<-NY_area_daily_max%>%
  gather(Date,`Concentration (PPB)`,7:252)%>%
  dplyr::filter(!State == "NA",!Date == "NA")%>%
  dplyr::mutate(Date = as.Date(Date,format = "%m/%d/%Y"))%>%
  dplyr::select(-Latitude)
### Read in NY area design values spreadsheet ###
NY_design<-read_xlsx("NY-Area Ozone 2022 8hr_KZ.xlsx",sheet = "8Hr Rpt",skip = 3)%>%
  dplyr::select(1:11)%>%
  dplyr::slice(1:31)%>%
  dplyr::rename("State"="...2","Site"="...3",
                "Design Value 2020"="Design Values",
                "Design Value 2021"="...10",
                "Design Value 2022"="...11",
                "AQS ID"='2022',
                "4th Max (ppb)"="...5",
                "4th Max (ppb)"="...6",
                "4th Max (ppb)"="...7",
                "4th Max (ppb)"="...8")
  

#Needed to make proper table for downloadable excel file
NY_design_ex<-read_xlsx("NY_design_ex.xlsx",sheet = "8Hr Rpt",skip = 3)%>%
  dplyr::select(1:11)%>%
  dplyr::slice(1:31)%>%
  dplyr::rename("State"="...2","Site"="...3",
                "Design Value 2020"="Design Values",
                "Design Value 2021"="...10",
                "Design Value 2022"="...11",
                "AQS ID"='2022',
                "4th Max (ppb)"="...5",
                "4th Max (ppb)"="...6",
                "4th Max (ppb)"="...7",
                "4th Max (ppb)"="...8")

#NY_design_plot<-read_xlsx("NY_d_plot.xlsx",sheet = "8Hr Rpt",skip = 3)%>%
#  dplyr::select(1:11)%>%
#  dplyr::slice(1:30)%>%
#  dplyr::rename("State"="...2","Site"="...3",
#                "Design Value 2018"="Design Values",
#                "Design Value 2019"="...10",
#                "Design Value 2020"="...11",
#                "AQS ID"='2020')%>%
#  dplyr::rename("2018"="Design Value 2018",
#                "2019"="Design Value 2019",
#                "2020"="Design Value 2020")%>%
#  dplyr::select(Site,'2018','2019','2020')%>%
#  gather(Year,Design_Value,2:4)%>%
#  dplyr::mutate(Design_Value=as.numeric(Design_Value))
### Read in NY area highest levels speadsheet ###
NY_highest_levels<-read_xlsx("NY-Area Ozone 2022 8hr_KZ.xlsx",sheet = "NY-8Hr",skip = 2)%>%
  dplyr::select(5:16)%>%
  dplyr::rename("# of days > 70"="> 70","# of days > 75"="> 75",
                "# of days > 84"="> 84")%>%
  mutate_all(funs(replace_na(.,0)))

### Read in PA ozone daily max spreadsheet ###
PA_area_daily_max<-read_xlsx("PA-Area Ozone 2022 8hr_KZ.xlsx",sheet = "DailyMax",skip = 3)%>%
  dplyr::slice(1:24)%>%
  setNames(., c("AQS Code","Latitude","State","Site Name","Elev (m)","County",
                format(as.Date(as.numeric(names(.)[-1:-6]), 
                               origin = '1899-12-30'), '%m/%d/%Y')))

### Make all date columns numeric ###
PA_area_daily_max[,7:252] <- sapply(PA_area_daily_max[,7:252],as.numeric)

PA_area_daily_max_ex<-PA_area_daily_max%>%
  dplyr::slice(1:23)%>%
  add_column(`Number of Days Exceeded` = rowSums(.[8:251] > 70, na.rm= TRUE), .after = "County")

### Make column name for last column that keeps reading in blank ###
#colnames(PA_area_daily_max)[252]<-"Max"
colnames(PA_area_daily_max_ex)[8]<-"Max"

### Get rid of lat column###
PA_area_daily_max_ex<-PA_area_daily_max_ex%>%
  dplyr::select(-Latitude)

### Make another dataframe that makes wide to long ###
wide_to_long_PA<-PA_area_daily_max%>%
  gather(Date,`Concentration (PPB)`,7:252)%>%
  dplyr::filter(!State == "NA",!Date == "NA")%>%
  dplyr::mutate(Date = as.Date(Date,format = "%m/%d/%Y"))%>%
  dplyr::select(-Latitude)
### Read in PA design values spreadsheet ###
PA_design<-read_xlsx("PA-Area Ozone 2022 8hr_KZ.xlsx",sheet = "8Hr Rpt",skip = 3)%>%
  dplyr::select(1:11)%>%
  dplyr::slice(1:25)%>%
  dplyr::rename("State"="...2","Site"="...3",
                "Design Value 2020"="Design Values",
                "Design Value 2021"="...10",
                "Design Value 2022"="...11",
                "AQS ID"='2022',
                "4th Max (ppb)"="...5",
                "4th Max (ppb)"="...6",
                "4th Max (ppb)"="...7",
                "4th Max (ppb)"="...8")

#Needed to make proper table for downloadable excel file
PA_design_ex<-read_xlsx("PA_design_ex.xlsx",sheet = "8Hr Rpt",skip = 3)%>%
  dplyr::select(1:11)%>%
  dplyr::slice(1:25)%>%
  dplyr::rename("State"="...2","Site"="...3",
                "Design Value 2020"="Design Values",
                "Design Value 2021"="...10",
                "Design Value 2022"="...11",
                "AQS ID"='2022',
                "4th Max (ppb)"="...5",
                "4th Max (ppb)"="...6",
                "4th Max (ppb)"="...7",
                "4th Max (ppb)"="...8")
#PA_design_plot<-read_xlsx("PA_d_plot.xlsx",sheet = "8Hr Rpt",skip = 3)%>%
#  dplyr::select(1:11)%>%
#  dplyr::slice(1:25)%>%
#  dplyr::rename("State"="...2","Site"="...3",
#                "Design Value 2018"="Design Values",
#                "Design Value 2019"="...10",
#                "Design Value 2020"="...11",
#                "AQS ID"='2020')%>%
#  dplyr::rename("2018"="Design Value 2018",
#                "2019"="Design Value 2019",
#                "2020"="Design Value 2020")%>%
#  dplyr::select(Site,'2018','2019','2020')%>%
#  gather(Year,Design_Value,2:4)%>%
#  dplyr::mutate(Design_Value=as.numeric(Design_Value))
### Read in PA Highest Level spreadsheet ###
PA_highest_levels<-read_xlsx("PA-Area Ozone 2022 8hr_KZ.xlsx",sheet = "PA-8Hr",skip = 2)%>%
  dplyr::select(5:16)%>%
  dplyr::rename("# of days > 70"="> 70","# of days > 75"="> 75",
                "# of days > 84"="> 84")%>%
  mutate_all(funs(replace_na(.,0)))

