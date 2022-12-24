#######################################################
### Merging the SEN-SPA Surveys  ######################
#######################################################
### this file will read in the most recent 
### versions of these surveys
### and save with the date.

rm(list=ls())

#####################################
# -- load packages and functions -- #
#####################################
country.file  <- paste0("<<<< FILEPATH REDACTED >>>>", "/SENEGAL/")
setwd(country.file)

# loading packages and useful functions
source("UsefulFunctions/Packages.R")
source("UsefulFunctions/expit_logit.R")

# getting the shapefiles for regions and departments
shape1<- shapefile(paste0(country.file,"Shapes/sdr_subnational_boundaries_2020-12-13/shps/sdr_subnational_boundaries"))
shape2<-  shapefile(paste0(country.file,"Shapes/adm2_3/sen_admbnda_adm2_1m_gov_ocha_20190426"))



loc<-paste0(country.file,"processed_data")

processed<-list.files(loc)[grep("Processed_Senegal",list.files(loc))]
processed_full<-processed

# combine the SPA surveys
dat<-read_csv(paste0(loc,processed[1]))
for(i in 2:length(processed_full)){
  tmp<-read_csv(paste0(loc,processed[i]))
  dat<-rbind(dat,tmp[,names(dat)])
  
}


#####################################
# -- fixing the department names -- #
#####################################

unique(dat$department)
unique(dat$department)[!unique(dat$department)%in%unique(tolower(shape2@data[,c("ADM2_FR")]))]


# Add Columns of logit transformed outcomes #
threshold<-0.00001
dat<-dat%>%
  filter(!is.na(department)) %>%
  mutate(
                  readiness = as.numeric(readiness),
                  se.readiness=as.numeric(se.readiness),
                  logit_readiness=logit_outcome(readiness,thresh=threshold),
                  var_logit_readiness= var_logit_outcome(readiness,se.readiness,thresh=threshold),
                  process_quality = as.numeric(process_quality),
                  se.process_quality=as.numeric(se.process_quality),
                  logit_process_quality=logit_outcome(process_quality,thresh=threshold),
                  var_logit_process_quality= var_logit_outcome(process_quality,se.process_quality,thresh=threshold)
                                                               
                                                               
)

###########################
# -- Save the new data -- #
###########################
write_csv(dat,paste0(loc,"All_SEN_",as.character(today()),".csv"))
