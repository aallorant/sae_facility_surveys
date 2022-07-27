#######################################################
### Identifying the best model

### this file will read the processed data and fit different 
### models defined by different combinations of included covariates
### and will calculate several goodness of fit indicators including DIC, CPO and WAIC.

rm(list=ls())


country.file <- paste0("<<<< FILEPATH REDACTED >>>>", "/SENEGAL/")
setwd(country.file)

#####################################
# -- load packages and functions -- #

source("UsefulFunctions/Packages.R")
source("UsefulFunctions/expit_logit.R")
source("UsefulFunctions/stepwise_vif.R")


loc<-paste0(country.file,"processed_data")

##############################################
# -- Read in the shapefile -- #
##############################################
shape1<- shapefile(paste0(country.file,"Shapes/sdr_subnational_boundaries_2020-12-13/shps/sdr_subnational_boundaries"))
shape2<-  shapefile(paste0(country.file,"Shapes/adm2_3/sen_admbnda_adm2_1m_gov_ocha_20190426"))

# adding a row number to the shape1 object, will be handy for plotting later on
shape1@data$row_num<-1:nrow(shape1@data)
shape2@data$row_num<-1:nrow(shape2@data)


key<-shape2@data[,c("row_num","ADM1_FR","ADM2_FR")]
key$department<-tolower(key$ADM2_FR)

##################################
# -- read in the combine data -- #
dat<-read_csv(paste0(loc,"All_SEN_",as.character(today()-2),".csv")) %>%
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
              dplyr::select(hw_density, department)) #%>%
# merging the data and the covariates
dat <- dat %>%
  full_join(covariates)

# identifying the optimal set of covariates best on VIF
stepwise_vif_selection(thresh = 5, covariate_data = covariates, keepCovars = c(hw_density))
library(MASS)
# Fit the full model for the different indicators
full.model <- lm(logit_sara_index ~., data = dat[dat$department!="ALL",c("logit_sara_index",names(covariates))] %>%
                   dplyr::select(-department))
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
full.model <- lm(logit_fifteen.assess ~., data = dat[dat$department!="ALL",c("logit_fifteen.assess",names(covariates))] %>%
                   dplyr::select(-department))
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)


#################################
# --- looking at the shapes --- #
nb.r <- poly2nb(shape2, queen=F)
mat <- nb2mat(nb.r, style="B",zero.policy=TRUE) # mat is the 0/1 adjacency matrix


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

for(j in 1:nrow(results)){
  print(paste0(j," of ",nrow(results),
               " outcomes groups. ",Sys.time()))
  source("/UsefulFunctions/SAE_Models.R")
  
  All<-filter(dat,department!="ALL")
  
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
                          "row_num","department","year","ADM1_FR",
                          "hw_density","hdi","motorized_hcf",
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
  ############################
  # -- set up some models -- #
  
  
  
  time<-c("rw1")
  space<-c("bym2")
  
  models<-expand.grid(time=time,space=space)
  models$time<-as.character(models$time)
  models$space<-as.character(models$space)
  
  model_results<-NULL
  
  for(i in 1:nrow(models)){
    print(paste0(i," of ",nrow(models)," of spatial options.",Sys.time()))
    
    
    #######################################
    # -- fit the model for the outcomes-- #
    #######################################

    source("/UsefulFunctions/Fit_SAE_Models_INLA.R")
    #####################################
    # --- look at model performance --- #
    #####################################
    # looking for low DIC and WAIC and high sum(log(cpo))
    res<-rbind(cbind(mod1$dic$dic,mod1$dic$p.eff,-sum(log(mod1$cpo$cpo),na.rm=T),mod1$waic$waic),# 
               cbind(mod2$dic$dic,mod2$dic$p.eff,-sum(log(mod2$cpo$cpo),na.rm=T),mod2$waic$waic),#,
               cbind(mod3$dic$dic,mod3$dic$p.eff,-sum(log(mod3$cpo$cpo),na.rm=T),mod3$waic$waic),
               cbind(mod4$dic$dic,mod4$dic$p.eff,-sum(log(mod4$cpo$cpo),na.rm=T),mod4$waic$waic),
               cbind(mod5$dic$dic,mod5$dic$p.eff,-sum(log(mod5$cpo$cpo),na.rm=T),mod5$waic$waic), 
               cbind(mod6$dic$dic,mod6$dic$p.eff,-sum(log(mod6$cpo$cpo),na.rm=T),mod6$waic$waic),
               cbind(mod7$dic$dic,mod7$dic$p.eff,-sum(log(mod7$cpo$cpo),na.rm=T),mod7$waic$waic))
    
    res<-as.data.frame(res)
    names(res)<-c("DIC","p_eff","sum_log_cpo","WAIC")
    res$time<-models$time[i]
    res$space<-models$space[i]
    res$model<-1:7
    
    
    model_results<-rbind(model_results,res)
    
  }
  write_csv(model_results,paste0("output/ModelSelection/models",
                                 "_",results$outcome[j],".csv"))
  
  
}
