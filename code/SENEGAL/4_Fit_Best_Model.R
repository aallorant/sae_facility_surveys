
rm(list=ls())
library(ggplot2)

country.file <- paste0("<<<< FILEPATH REDACTED >>>>", "/SENEGAL/")
setwd(country.file)


#####################################
# -- load packages and functions -- #
source("UsefulFunctions/Packages.R")
source("UsefulFunctions/expit_logit.R")


loc<-paste0(country.file,"processed_data")

##############################################
# -- Read in the shapefile -- #
##############################################
shape1<- shapefile(paste0(country.file,"Shapes/sdr_subnational_boundaries_2020-12-13/shps/sdr_subnational_boundaries"))
shape2<-  shapefile(paste0(country.file,"Shapes/adm2_3/sen_admbnda_adm2_1m_gov_ocha_20190426"))

# adding a row number to the shape1 object, will be handy for plotting later on
shape1@data$row_num<-1:nrow(shape1@data)
shape2@data$row_num<-1:nrow(shape2@data)


key<-shape2@data[,c("row_num","ADM2_FR")]
key$department<-tolower(key$ADM2_FR)

##################################
# -- read in the combine data -- #
dat<-read_csv(paste0(loc,"All_SEN_",as.character(today()),".csv")) %>%
  mutate(year = ifelse(year == 2020, 2019, year),
         sampling = ifelse(year %in% c(2013,2014), 1,
                           ifelse(year %in% c(2015,2016), 2, 
                                  ifelse(year == 2017, 3, 4))),
         sampling = factor(sampling))

# read in the covariates
covariates <- readRDS(paste0(country.file,"covariates/covariates_by_ad2.RDS")) %>%
  rename(department = region)
optional_cov <- readRDS(paste0(country.file,"../Aim3/prepped_covariates/SEN/combined_u5m_ec_estimates.RDS")) %>%
  dplyr::select(year,department,u5m,diarrhea_prevalence, map_pf_prevalence, lri_inc)
hw_density <- readRDS(paste0(country.file,"hw_density/health_worker_density_SEN_ad2.RDS"))

# fixing department names
hw_density$department[hw_density$department=="m'backe"] <- "mbacke"
hw_density$department[hw_density$department=="m'bour"] <- "mbour"
hw_density$department[hw_density$department=="koupentoum"] <- "koumpentoum"
hw_density$department[hw_density$department=="medina yoro foulah"] <- "medina yoroufoula"
hw_density$department[hw_density$department=="malem hoddar"] <- "malem hodar"
hw_density$department[hw_density$department=="tivaouane"] <- "tivaoune"

covariates <- covariates %>%
  left_join(optional_cov) %>%
  left_join(hw_density %>%
              rename(hw_density=hw) %>%
              dplyr::select(hw_density, department))
dat <- dat %>%
  full_join(covariates)


#################################
# --- looking at the shapes --- #
nb.r <- poly2nb(shape2, queen=F)
mat <- nb2mat(nb.r, style="B",zero.policy=TRUE) # mat is the 0/1 adjacency matrix



####################################################################################
####################################################################################
############ Loop through the subsets of data and the selected model ###############
####################################################################################
####################################################################################

results<-read_csv("output/department/ModelSelection/SelectedModels.csv")


#######################
# -- cycle through -- #
out.pred <- NULL
for(j in 1:nrow(results)){
  print(paste0(j," of ",nrow(results),
               " outcomes groups. ",Sys.time()))
  All<-filter(dat,department!="ALL")# & mga == results$mga[j])
  
  All<-All%>%left_join(key)
  
  #------------------------------------------------------------------#
  #------ add in all missing timepoints from 2013 through 2020 ------#
  #------------------------------------------------------------------#
  
  department.key<-unique(All[,c("row_num","department")])
  
  
  grid<-expand.grid(row_num=c(1:45),year=c(2013:2019))
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
                          "row_num","department","year","hw_density","hdi","motorized_hcf",
                          "landcover","tt_hcf",
                          "map_pf_prevalence" ,"lri_inc","u5m","edu_mean","ntl_harm",
                          "ghslurbanicity","dmspntl","worldpop_u5", "access",
                          "elevation","gdp","diarrhea_prevalence")])
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

  # enforcing the sum to one constraint on the survey random effects
  A <- matrix(1, ncol = 4, nrow = 1)
  e <- matrix(0, ncol = 1)

  if(results$model[j]==1){
    # -- model 1: iid RE department -- #
    model <- outcome ~ f(dist.id, model="iid", param=prior.iid) +  # department random effect
      f(period.id, model=results$time[j],  param=prior.iid) + # time smoothing process (random walk)
      f(period.id2, model="iid",  param=prior.iid) + # time iid
      f(period.id3, model=results$time[j], replicate=dist.id, param=prior.besag) # interaction time-space
 
  }
  
  if(results$model[j]==2){

    model <- outcome ~ f(dist.id, model="iid", param=prior.iid) +  # department random effect
      f(period.id, model=results$time[j],  param=prior.iid) +  # time smoothing process (random walk)
      f(period.id2, model="iid",  param=prior.iid) +   # iid time
      f(period.id3, model=results$time[j], replicate=dist.id, param=prior.besag)+ # interaction time-space
        f(sampling, model="iid", param=prior.iid, extraconstr = list(A = A, e = e)) 
    # survey random effects with sum-to-one constraint
  }
  
  if(results$model[j]==3){
    if(j == 1){
      model <- outcome ~ f(dist.id, model="iid", param=prior.iid) +  # department effect
        f(period.id, model=results$time[j],  param=prior.iid) +  # random walk 
        f(period.id2, model="iid",  param=prior.iid) +  # iid time
        f(period.id3, model=results$time[j], replicate=dist.id, param=prior.besag) +# space-time interaction
        log(elevation) + log(gdp) + log(u5m) + # covariates
        log(motorized_hcf) +
        dmspntl + scale(ghslurbanicity) + log(diarrhea_prevalence)+
        log(access) + scale(landcover) +
        scale(lri_inc) + log(worldpop_u5)
    }
    if(j == 2){
      model <- outcome ~ f(dist.id, model="iid", param=prior.iid) +  # department effect
        f(period.id, model=results$time[j],  param=prior.iid) +  # random walk 
        f(period.id2, model="iid",  param=prior.iid) +  
        f(period.id3, model=results$time[j], replicate=dist.id, param=prior.besag) +# iid time
        scale(hw_density) + dmspntl + scale(ghslurbanicity) +
        log(access) + scale(landcover) + scale(map_pf_prevalence) + 
        scale(lri_inc) + log(worldpop_u5)
    }
    if(j == 3){
      model <- outcome ~ f(dist.id, model="iid", param=prior.iid) +  # department effect
        f(period.id, model=results$time[j],  param=prior.iid) +  # random walk 
        f(period.id2, model="iid",  param=prior.iid) +  
        f(period.id3, model=results$time[j], replicate=dist.id, param=prior.besag) +# iid time
        scale(edu_mean) + log(u5m) + 
        log(motorized_hcf) +
        dmspntl + scale(ghslurbanicity) + scale(hdi)+
        scale(landcover) +scale(map_pf_prevalence)+
        scale(lri_inc)
    }
    if(j == 4){
      model <- outcome ~ f(dist.id, model="iid", param=prior.iid) +  # department effect
        f(period.id, model=results$time[j],  param=prior.iid) +  # random walk 
        f(period.id2, model="iid",  param=prior.iid) +  
        f(period.id3, model=results$time[j], replicate=dist.id, param=prior.besag) +# iid time
        log(access) + 
        log(tt_hcf) +scale(gdp)+
        scale(lri_inc) + log(worldpop_u5)
    }


  }  
  
  if(results$model[j]==4){

    if(j == 1){
      model <- outcome ~ f(dist.id, model="iid", param=prior.iid) +  # department effect
        f(period.id, model=results$time[j],  param=prior.iid) +  # random walk 
        f(period.id2, model="iid",  param=prior.iid) +  
        f(period.id3, model=results$time[j], replicate=dist.id, param=prior.besag) +# iid time
          f(sampling, model="iid", param=prior.iid, extraconstr = list(A = A, e = e))  +
        log(elevation) + log(gdp) + log(u5m) + 
        log(motorized_hcf) +
        dmspntl + scale(ghslurbanicity) + log(diarrhea_prevalence)+
        log(access) + scale(landcover) +
        scale(lri_inc) + log(worldpop_u5)
    }
    if(j == 2){
      model <- outcome ~ f(dist.id, model="iid", param=prior.iid) +  # department effect
        f(period.id, model=results$time[j],  param=prior.iid) +  # random walk 
        f(period.id2, model="iid",  param=prior.iid) +  
        f(period.id3, model=results$time[j], replicate=dist.id, param=prior.besag) +# iid time
          f(sampling, model="iid", param=prior.iid, extraconstr = list(A = A, e = e))  +
        scale(hw_density) + dmspntl + scale(ghslurbanicity) +
        log(access) + scale(landcover) + scale(map_pf_prevalence) + 
        scale(lri_inc) + log(worldpop_u5)
    }
    if(j == 3){
      model <- outcome ~ f(dist.id, model="iid", param=prior.iid) +  # department effect
        f(period.id, model=results$time[j],  param=prior.iid) +  # random walk 
        f(period.id2, model="iid",  param=prior.iid) +  
        f(period.id3, model=results$time[j], replicate=dist.id, param=prior.besag) +# iid time
          f(sampling, model="iid", param=prior.iid, extraconstr = list(A = A, e = e))  +
        scale(edu_mean) + log(u5m) + 
        log(motorized_hcf) +
        dmspntl + scale(ghslurbanicity) + scale(hdi)+
        scale(landcover) +scale(map_pf_prevalence)+
        scale(lri_inc)
    }
    if(j == 4){
      model <- outcome ~ f(dist.id, model="iid", param=prior.iid) +  # department effect
        f(period.id, model=results$time[j],  param=prior.iid) +  # random walk 
        f(period.id2, model="iid",  param=prior.iid) +  
        f(period.id3, model=results$time[j], replicate=dist.id, param=prior.besag) +# iid time
          f(sampling, model="iid", param=prior.iid, extraconstr = list(A = A, e = e)) + scale(ntl_harm) + 
        log(access) + 
        log(tt_hcf) +scale(gdp)+
        scale(lri_inc) + log(worldpop_u5)
    }
    

  } 
  
  if(results$model[j]==5){

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

    if(j == 1){
      model <- outcome ~ f(dist.id, model=results$space[j], param=prior.iid, graph=mat) +  # department effect
        f(period.id, model=results$time[j],  param=prior.iid) +  # random walk
        f(period.id2, model="iid",  param=prior.iid) +  # iid time
        f(period.id3, model=results$time[j], replicate=dist.id, param=prior.besag) +  # department X time
          f(sampling, model="iid", param=prior.iid, extraconstr = list(A = A, e = e))  +
        log(elevation) + log(gdp) + log(u5m) + 
        log(motorized_hcf) +
        dmspntl + scale(ghslurbanicity) + log(diarrhea_prevalence)+
        log(access) + scale(landcover) +
        scale(lri_inc) + log(worldpop_u5)
    }
    if(j == 2){
      model <- outcome ~ f(dist.id, model=results$space[j], param=prior.iid, graph=mat) +  # department effect
        f(period.id, model=results$time[j],  param=prior.iid) +  # random walk
        f(period.id2, model="iid",  param=prior.iid) +  # iid time
        f(period.id3, model=results$time[j], replicate=dist.id, param=prior.besag) +  # department X time
          f(sampling, model="iid", param=prior.iid, extraconstr = list(A = A, e = e))  +
        scale(hw_density) + dmspntl + scale(ghslurbanicity) +
        log(access) + scale(landcover) + scale(map_pf_prevalence) + 
        scale(lri_inc) + log(worldpop_u5)
    }
    if(j == 3){
      model <- outcome ~ f(dist.id, model=results$space[j], param=prior.iid, graph=mat) +  # department effect
        f(period.id, model=results$time[j],  param=prior.iid) +  # random walk
        f(period.id2, model="iid",  param=prior.iid) +  # iid time
        f(period.id3, model=results$time[j], replicate=dist.id, param=prior.besag) +  # department X time
          f(sampling, model="iid", param=prior.iid, extraconstr = list(A = A, e = e))  +
        scale(edu_mean) + log(u5m) + 
        log(motorized_hcf) +
        dmspntl + scale(ghslurbanicity) + scale(hdi)+
        scale(landcover) +scale(map_pf_prevalence)+
        scale(lri_inc)
    }
    if(j == 4){
      model <- outcome ~ f(dist.id, model=results$space[j], param=prior.iid, graph=mat) +  # department effect
        f(period.id, model=results$time[j],  param=prior.iid) +  # random walk
        f(period.id2, model="iid",  param=prior.iid) +  # iid time
        f(period.id3, model=results$time[j], replicate=dist.id, param=prior.besag) +  # department X time
          f(sampling, model="iid", param=prior.iid, extraconstr = list(A = A, e = e)) + 
        scale(ntl_harm) + 
        log(access) + 
        log(tt_hcf) +scale(gdp)+
        scale(lri_inc) + log(worldpop_u5)
    }
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
  
  loc<-paste0(substr(results$outcome[j],7,30))
 
  ###############################
  # -- save the model output -- #
  ###############################
  pred<-filter(mod_dat,preds==1) %>%
    dplyr::select(department,year,up,low,mean) %>%
    mutate(indicator = results$outcome[j])
  
  #
  out.pred <- rbind(out.pred, pred)
  save(mod,mod_dat,All,file=paste0("output/Model_Fits/",loc,"_model.RDATA"))
}
saveRDS(out.pred,file=paste0("output/Preds/_preds.rds"))
