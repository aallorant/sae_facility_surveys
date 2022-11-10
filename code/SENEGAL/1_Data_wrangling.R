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
  # i<-8
  tmp<-read_csv(paste0(loc,processed[i]))
  dat<-rbind(dat,tmp[,names(dat)])
  
}


#####################################
# -- fixing the department names -- #
#####################################

unique(dat$department)
unique(dat$department)[!unique(dat$department)%in%unique(tolower(shape2@data[,c("ADM2_FR")]))]


# logit transformation for the mean and variance of the survey estimates
logit_outcome<-function(indicator,thresh){
  
  logit.indicator<-ifelse(!is.na(indicator),logit(indicator),NA)
  logit.indicator<-ifelse(indicator<thresh | indicator>(1-thresh),NA,logit.indicator)
  logit.indicator
}

var_logit_outcome<-function(indicator,se,thresh){
  var_logit<-(se^2)/(indicator^2*(1-indicator)^2)
  var_logit<-ifelse(se<thresh,NA, var_logit)
  var_logit
}


# Add Columns of logit transformed outcomes #
threshold<-0.00001
dat<-dat%>%
  filter(!is.na(department)) %>%
  mutate(
                  sara_index = as.numeric(sara_index),
                  se.sara_index=as.numeric(se.sara_index),
                  logit_sara_index=logit_outcome(sara_index,thresh=threshold),
                  var_logit_sara_index= var_logit_outcome(sara_index,se.sara_index,thresh=threshold),
                  fifteen.assess = as.numeric(fifteen.assess),
                  se.fifteen.assess=as.numeric(se.fifteen.assess),
                  logit_fifteen.assess=logit_outcome(fifteen.assess,thresh=threshold),
                  var_logit_fifteen.assess= var_logit_outcome(fifteen.assess,se.fifteen.assess,thresh=threshold)
                                                               
                                                               
)

###########################
# -- Save the new data -- #
###########################
write_csv(dat,paste0(loc,"All_SEN_",as.character(today()),".csv"))
