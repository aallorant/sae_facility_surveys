####################################################################################################
## Author:      Adrien Allorant
##
## Description: Read for each outcome and each combinations of managing authority and facility type
##              (including ALL) the CSV file including the values of the 3 criteria for the 7 models
##              and save the model with the highest performance across these criteria.
##              If criteria points to different models as best models, we use a majority rule.
##              
## Requires:    a CSV file per outcome x managing authority x facility type indicating the value
##              of each of the 3 criteria for the 7 models.
##
## Outputs:     a CSV file per outcome x managing authority x facility type indicating the best of the
##              7 models according to the 3 criteria.
####################################################################################################

rm(list=ls())

main_dir <- paste0("<<<< FILEPATH REDACTED >>>>")
setwd(main_dir)

#####################################
# -- load packages and functions -- #
source("UsefulFunctions/Packages.R")
source("UsefulFunctions/expit_logit.R")

################################################################################
################################################################################
############ Loop through the subsets of data and select a model ###############
################################################################################
################################################################################


indicators<-c("logit_readiness","logit_process_quality")

groups<-expand.grid(outcome=indicators)
groups_all<-data.frame(outcome=indicators)

results<-rbind(groups_all)
results$var<-paste0("var_",results$outcome)

#######################
# -- cycle through -- #

results$model<-results$time<-results$space<-NA

#######################
# -- cycle through -- #

for(j in 1:nrow(results)){
  
  tmp<-read_csv(paste0("output/ModelSelection/models","_",results$outcome[j],"_",results$mga[j],
                       "_",results$factype[j],".csv"))
  
  min_row_waic<-which(tmp$WAIC==min(tmp$WAIC))[1]
  min_row_dic<-which(tmp$DIC==min(tmp$DIC))[1]
  max_row_cpo<-which(tmp$sum_log_cpo==min(tmp$sum_log_cpo))[1]
  
  if(min_row_waic != max_row_cpo){
    min_row <- as.numeric(names(which.max(table(c(which(1:7 == min_row_waic),which(1:7 == min_row_dic), which(1:7 == max_row_cpo))))))
  } else
    min_row <- which(tmp$sum_log_cpo==min(tmp$sum_log_cpo))
  
  results[j,"model"]<-tmp$model[min_row]
  results[j,"space"]<-tmp$space[min_row]
  results[j,"time"]<-tmp$time[min_row]
  
}

##################################
# -- save the selected models -- #
write_csv(results,"output/ModelSelection/SelectedModels.csv")
