####################################################################################################
## Author:      Adrien Allorant
##
## Description: Read processed survey data and merge them with spatio-temporally referenced
##              covariates (including health worker density, derived from census data), and
##              fit the "best" small area estimation model for each metric (readiness and process quality),
##              to all facility data, and stratified by managing authority (public/private) 
##              and by facility type (hospitals, health centers, clinics), identified with the 3 criteria
##              (DIC, LCPO, WAIC).
##
## Requires:    UsefulFunctions/SAE_models.R a script including all 7 models to be run in INLA
##              UsefulFunctions/Fit_SAE_models_INLA.R a script that runs the 7 models
##
## Outputs:     a .RDATA file per outcome x managing authority x facility type including
##              the model fit, and a a .rds file comprising the predictions of the best model.
####################################################################################################


rm(list=ls())
library(ggplot2)

main_dir <- paste0("<<<< FILEPATH REDACTED >>>>")
setwd(main_dir)


#####################################
# -- load packages and functions -- #
source("UsefulFunctions/Packages.R")
source("UsefulFunctions/expit_logit.R")


loc<-paste0(main_dir,"processed_data")

##############################################
# -- Read in the shapefile -- #
##############################################
shape1<- shapefile(paste0(main_dir,"Shapes/sdr_subnational_boundaries_2020-12-13/shps/sdr_subnational_boundaries"))
shape2<-  shapefile(paste0(main_dir,"Shapes/adm2_3/sen_admbnda_adm2_1m_gov_ocha_20190426"))

# adding a row number to the shape1 object, will be handy for plotting later on
shape1@data$row_num<-1:nrow(shape1@data)
shape2@data$row_num<-1:nrow(shape2@data)


key<-shape2@data[,c("row_num","ADM2_FR")]
key$department<-tolower(key$ADM2_FR)


##################################
# -- read in the combine data -- #

dat<-fread((paste0(loc,"All_SEN_",as.character(today()),".csv")))

# captures the different phases in SPA survey sampling in SEN between 2013 and 2019
dat[year %in% 2013:2014 ,sampling := 1]
dat[year %in% 2015:2016 ,sampling := 2]
dat[year %in% 2017 ,sampling := 3]
dat[year > 2018 ,sampling := 4]

# read in the covariates
covariates <- setDT(readRDS(paste0(main_dir,"covariates/covariates_by_ad2.RDS")))

# merging the data and the covariates
dat <- merge(dat,covariates, by = 'department,year', all.y = T)

#################################
# --- looking at the shapes --- #

nb.r <- poly2nb(shape2, queen=F)
mat <- nb2mat(nb.r, style="B",zero.policy=TRUE) # mat is the 0/1 adjacency matrix



####################################################################################
####################################################################################
############ Loop through the subsets of data and the selected model ###############
####################################################################################
####################################################################################

results<-read_csv("output/ModelSelection/SelectedModels.csv")


#######################
# -- cycle through -- #
out.pred <- NULL
for(j in 1:nrow(results)){
  print(paste0(j," of ",nrow(results),
               " outcomes groups. ",Sys.time()))
  All<-filter(dat,department!="ALL", mga == results$mga[j],
              factype == results$factype[j]) %>%
    mutate(mga = ifelse(is.na(mga), results$mga[j], mga),
           factype = ifelse(is.na(factype), results$factype[j], factype))
  
  All<-All%>%left_join(key)
  
  #------------------------------------------------------------------#
  #------ add in all missing timepoints from 2010 through 2020 ------#
  #------------------------------------------------------------------#
  
  department.key<-unique(All[,c("row_num","department")])
  
  
  grid<-expand.grid(row_num=c(1:45),year=c(2010:2020))
  grid<-grid%>%left_join(department.key)
  dim(grid)
  dim(All)
  
  All<-All%>%right_join(grid)
  dim(All)
  
  # creating some variables for the random effects #
  All$period.id3<-All$period.id2<-All$period.id<-as.numeric(as.factor(All$year))
  All$dist.id3<-All$dist.id2<-All$dist.id<-All$row_num
  
  All<-arrange(All,row_num,year)

  pred.all<-unique(All[,c("period.id","period.id2","period.id3","dist.id","dist.id2","dist.id3",
                          "row_num","department","year","hw_density","hdi","tt_hcf","landcover",
                          "elevation", "access","gdp")])
  pred.all<-pred.all%>%mutate(outcome=NA, prec=NA, sampling = NA,preds=1)
  
  
  All$outcome<-All[,as.character(results$outcome[j])]
  All$prec<-1/All[,as.character(results$var[j])]
  All$preds<-0
  
  # a dataset for predictions #
  row.names(All)<-NULL
  row.names(pred.all)<-NULL
  
  tmp<-All[,names(pred.all)]
  tmp$outcome<-as.numeric(unlist(tmp$outcome))
  tmp$prec<-as.numeric(unlist(tmp$prec))
  mod_dat<-rbind(tmp,pred.all)
  mod_dat<-mod_dat%>%mutate(outcome=ifelse(is.na(prec),NA,outcome),
                            prec=ifelse(is.na(outcome),NA,prec))
  
  
  ##################################
  #### -- Set some priors --- ######
  ##################################
  
  prior.iid = c(0.5,0.008)
  prior.besag = c(0.5,0.008)
  
  # creating the survey RE sum-to-one constraint
  A <- matrix(1, ncol = 4, nrow = 1)
  e <- matrix(0, ncol = 1)
  ############################
  # -- set up some models -- #
  
  
  # -- model 1: no interactions -- #
  if(results$model[j]==1){
    # -- model 1: iid RE department -- #
    model <- outcome ~ f(dist.id, model="iid", param=prior.iid) +  # department effect
      f(period.id, model=results$time[j],  param=prior.iid) +  # random walk 
      f(period.id2, model="iid",  param=prior.iid) +
      f(period.id3, model=results$time[j], replicate=dist.id, param=prior.besag) 
    # iid time
  }
  
  if(results$model[j]==2){
    # -- model 2: iid RE department + survey RE or sampling for SEN-- #
    model <- outcome ~ f(dist.id, model="iid", param=prior.iid) +  # department effect
      f(period.id, model=results$time[j],  param=prior.iid) +  # random walk 
      f(period.id2, model="iid",  param=prior.iid) +   # iid time
      f(period.id3, model=results$time[j], replicate=dist.id, param=prior.besag)+
              f(sampling, model="iid", param=prior.iid, extraconstr = list(A = A, e = e)) 
    # department X time
  }
  
  if(results$model[j]==3){
    # -- model 3: iid RE department + covariates -- #
    model <- outcome ~ f(dist.id, model="iid", param=prior.iid) +  # department effect
      f(period.id, model=results$time[j],  param=prior.iid) +  # random walk 
      f(period.id2, model="iid",  param=prior.iid) +  
      f(period.id3, model=results$time[j], replicate=dist.id, param=prior.besag) +# iid time
      scale(hw_density) + scale(gdp) + scale(hdi) + scale(elevation) +
      scale(tt_hcf) + scale(access) + scale(landcover)
  }  
  
  if(results$model[j]==4){
    # -- model 4: iid RE department + survey RE or sampling for SEN + selected covariates-- #
    model <-  outcome ~ f(dist.id, model="iid", param=prior.iid) +  # department effect
      f(period.id, model=results$time[j],  param=prior.iid) +  # random walk 
      f(period.id2, model="iid",  param=prior.iid) +   # iid time
      f(period.id3, model=results$time[j], replicate=dist.id, param=prior.besag)+
              f(sampling, model="iid", param=prior.iid, extraconstr = list(A = A, e = e))   + 
      scale(hw_density) + scale(gdp) + scale(hdi) + scale(elevation) +
      scale(tt_hcf) + scale(access) + scale(landcover)
  } 
  
  if(results$model[j]==5){
    # -- model 5: spatial RE - no interaction -- #
    model <- outcome ~ f(dist.id, model=results$space[j], param=prior.iid, graph=mat) +  # department effect
      f(period.id, model=results$time[j],  param=prior.iid) +  # random walk
      f(period.id2, model="iid",  param=prior.iid) +
      f(period.id3, model=results$time[j], replicate=dist.id, param=prior.besag) 
    
  } 
  
  
  if(results$model[j]==6){
    # -- model 6: space-time + survey RE or sampling for SEN -- #
    model <- outcome ~ f(dist.id, model=results$space[j], param=prior.iid, graph=mat) +  # department effect
      f(period.id, model=results$time[j],  param=prior.iid) +  # random walk
      f(period.id2, model="iid",  param=prior.iid) +  # iid time
      f(period.id3, model=results$time[j], replicate=dist.id, param=prior.besag) +  # department X time
              f(sampling, model="iid", param=prior.iid, extraconstr = list(A = A, e = e))  
  } 
  
  if(results$model[j]==7){
    # -- model 8: space-time + survey RE or sampling for SEN + selected covariates -- #
    model <- outcome ~ f(dist.id, model=results$space[j], param=prior.iid, graph=mat) +  # department effect
      f(period.id, model=results$time[j],  param=prior.iid) +  # random walk
      f(period.id2, model="iid",  param=prior.iid) +  # iid time
      f(period.id3, model=results$time[j], replicate=dist.id, param=prior.besag) +  # department X time
              f(sampling, model="iid", param=prior.iid, extraconstr = list(A = A, e = e))   + 
      scale(hw_density) + scale(gdp) + scale(hdi) + scale(elevation) +
      scale(tt_hcf) + scale(access) + scale(landcover)
  } 
  
  #######################################
  # -- fit the model for the outcomes-- #
  #######################################
  
  mod <- inla(model, 
              family = "gaussian", 
              data =mod_dat, 
              control.predictor=list(compute=TRUE),
              control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T),
              control.family=list(hyper=list(prec=list(initial=log(1),fixed=TRUE))),
              scale=prec)
  
  ###########################################
  # --- add predictions to the data set --- #
  ###########################################
  
  mod_dat$mean<-expit(mod$summary.fitted.values$`mean`)
  mod_dat$up<-expit(mod$summary.fitted.values$`0.975quant`)
  mod_dat$low<-expit(mod$summary.fitted.values$`0.025quant`)
  
  loc<-paste0(substr(results$outcome[j],7,20),
              "_mga",results$mga[j],"_fac",results$factype[j])
 
  ###############################
  # -- save the model output -- #
  ###############################
  pred<-filter(mod_dat,preds==1) %>%
    dplyr::select(department,year,up,low,mean) %>%
    mutate(indicator = results$outcome[j],
           mga = results$mga[j],
           factype = results$factype[j])
  
  #
  out.pred <- rbind(out.pred, pred)
  save(mod,mod_dat,All,file=paste0("output/Model_Fits/",loc,"_model.RDATA"))
}
saveRDS(out.pred,file=paste0("output/Preds/_preds.rds"))
