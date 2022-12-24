
# ---------------------------------------------------------------
# Stepwise VIF selection function

# Start function
stepwise_vif_selection <- function(thresh, 
                                   covariate_data,
                                   keepCovars,
                                   trace = TRUE
) {
  # ---------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------------
  # Data checks
  library(data.table)
  # make sure input data is a data table
  if(any(!'data.table' %in% class(covariate_data))) covariate_data<-data.table(covariate_data)
  cov_cols <- colnames(covariate_data)[!(colnames(covariate_data) %in% c("region","department","year"))]
  covariate_data <- covariate_data[, ..cov_cols]
  # remove any covariate values that do not vary accross the region
  var_names <- names(covariate_data)

  # ------------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------------------------------
  # VIF selection
  
  # load vif analysis package
  library(fmsb)

  # get initial vif value for all comparisons of variables
  vif_init<-NULL

  
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))

    if(val %in% keepCovars){
      vif_init<-rbind(vif_init, c(val, 0))
    } else {
      vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = covariate_data))))
    }
  }
  
  vif_max<-max(as.numeric(5), na.rm = TRUE)
  
  if(vif_max < thresh){
    
    if(trace==T){ # print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      
      message(paste('\nAll variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    
    cat(var_names)
    
  } else {
    
    # backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(covariate_data)
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        
        if(val %in% keepCovars) { 
          vif_add <- 0 
        } else {
          vif_add<-VIF(lm(form_in, data = covariate_data))
        } 
        
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ # print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        message(paste0('removed: ',vif_vals[max_row,1], ' ', vif_max,'\n\n'))
        flush.console()
      }
      
      keepCovars_cols <- var_names[-which(var_names == vif_vals[max_row,1])]
      covariate_data<-covariate_data[, ..keepCovars_cols]
      
    }
    
    
  }
  
  # vector of selected covariates
  selected_covs <- c(names(covariate_data))
  # ------------------------------------------------------------------------------------------------------------------------

  # ---------------------------------------------------------------------
  # End function
  
  # return a vector of covariates selected
  return(selected_covs)
}
# ----