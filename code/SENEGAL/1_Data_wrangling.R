#######################################################
### Merging the SEN-SPA Surveys

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

# DHS #

loc<-paste0(country.file,"processed_data")

processed<-list.files(loc)[grep("Processed_Senegal",list.files(loc))]
processed_full<-processed

# combine the DHS
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


# Add Columns for SAE #
threshold<-0.00001
dat<-dat%>%
  filter(!is.na(department)) %>%
  mutate(main_symptom = as.numeric(main_symptom),
                  se.main_symptom=as.numeric(se.main_symptom),
                  logit_main_symptom=logit_outcome(main_symptom,thresh=threshold),
                  var_logit_main_symptom= var_logit_outcome(main_symptom,se.main_symptom,thresh=threshold),
                  int_cov = as.numeric(int_cov),
                  se.int_cov=as.numeric(se.int_cov),
                  logit_int_cov=logit_outcome(int_cov,thresh=threshold),
                  var_logit_int_cov= var_logit_outcome(int_cov,se.int_cov,thresh=threshold),
                  quality.basic = as.numeric(quality.basic),
                  se.quality.basic=as.numeric(se.quality.basic),
                  logit_quality.basic=logit_outcome(quality.basic,thresh=threshold),
                  var_logit_quality.basic= var_logit_outcome(quality.basic,se.quality.basic,thresh=threshold),
                  sara_index = as.numeric(sara_index),
                  se.sara_index=as.numeric(se.sara_index),
                  logit_sara_index=logit_outcome(sara_index,thresh=threshold),
                  var_logit_sara_index= var_logit_outcome(sara_index,se.sara_index,thresh=threshold),
                  fifteen.assess = as.numeric(fifteen.assess),
                  se.fifteen.assess=as.numeric(se.fifteen.assess),
                  logit_fifteen.assess=logit_outcome(fifteen.assess,thresh=threshold),
                  var_logit_fifteen.assess= var_logit_outcome(fifteen.assess,se.fifteen.assess,thresh=threshold),
                  three_danger_signs = as.numeric(three_danger_signs),
                  se.three_danger_signs=as.numeric(se.three_danger_signs),
                  logit_three_danger_signs=logit_outcome(three_danger_signs,thresh=threshold),
                  var_logit_three_danger_signs= var_logit_outcome(three_danger_signs,se.three_danger_signs,thresh=threshold),
                  assess.adeq = as.numeric(assess.adeq),
                  se.assess.adeq=as.numeric(se.assess.adeq),
                  logit_assess.adeq=logit_outcome(assess.adeq,thresh=threshold),
                  var_logit_assess.adeq= var_logit_outcome(assess.adeq,se.assess.adeq,thresh=threshold),
                  child_readiness = as.numeric(child_readiness),
                  se.child_readiness=as.numeric(se.child_readiness),
                  logit_child_readiness=logit_outcome(child_readiness,thresh=threshold),
                  var_logit_child_readiness= var_logit_outcome(child_readiness,se.child_readiness,thresh=threshold),
                   child_readiness.basic = as.numeric(child_readiness.basic),
                   se.child_readiness.basic=as.numeric(se.child_readiness.basic),
                   logit_child_readiness.basic=logit_outcome(child_readiness.basic,thresh=threshold),
                   var_logit_child_readiness.basic= var_logit_outcome(child_readiness.basic,se.child_readiness.basic,thresh=threshold)
                                                               
                                                               
)

###########################
# -- Save the new data -- #
###########################
write_csv(dat,paste0(loc,"All_SEN_",as.character(today()),".csv"))
