#######################################################
### Identifying the best model

### this file will read the fitted
### models and compare them in terms of goodness of fit indicators including DIC, CPO and WAIC.
### Finally, it saves the best performing model across these indicators.

rm(list=ls())

country.file <- paste0("<<<< FILEPATH REDACTED >>>>", "/SENEGAL/")
setwd(country.file)

#####################################
# -- load packages and functions -- #
source("UsefulFunctions/Packages.R")
source("UsefulFunctions/expit_logit.R")

################################################################################
################################################################################
############ Loop through the subsets of data and select a model ###############
################################################################################
################################################################################


indicators<-c("logit_sara_index","logit_fifteen.assess")

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
  
  tmp<-read_csv(paste0("output/ModelSelection/models",
                                 "_",results$outcome[j],".csv"))
  
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
