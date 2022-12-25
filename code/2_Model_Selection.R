####################################################################################################
## Author:      Adrien Allorant
##
## Description: Read processed survey data and merge them with spatio-temporally referenced
##              covariates (including health worker density, derived from census data), and
##              fit 7 different small area estimation models for each metric (readiness and process quality),
##              to all facility data, and stratified by managing authority (public/private) 
##              and by facility type (hospitals, health centers, clinics).
##              The 7 models include different combinations
##              of random effects for time, space, survey instrument or sampling, time-space,
##              and VIF-selected covariates. Several goodness of fit criteria including DIC, LCPO
##              and WAIC are calculated.
##
## Requires:    UsefulFunctions/SAE_models.R a script including all 7 models to be run in INLA
##              UsefulFunctions/Fit_SAE_models_INLA.R a script that runs the 7 models
##
## Outputs:     a CSV file per outcome x managing authority x facility type indicating the value
##              of each of the 3 criteria for the 7 models.
##              For example, the 3 criteria for the 7 models run on the readiness metric in ALL public facilities
##              are saved in the file: "output/ModelSelection/models/readiness_public_ALL.csv"
####################################################################################################


rm(list=ls())


main_dir <- paste0("<<<< FILEPATH REDACTED >>>>")
setwd(main_dir)

#####################################
# -- load packages and functions -- #

source("UsefulFunctions/Packages.R")
source("UsefulFunctions/expit_logit.R")
source("UsefulFunctions/stepwise_vif.R")


loc<-paste0(main_dir,"processed_data")

##############################################
# -- Read in the shapefile -- #
##############################################
shape1<- shapefile(paste0(main_dir,"Shapes/sdr_subnational_boundaries_2020-12-13/shps/sdr_subnational_boundaries"))
shape2<-  shapefile(paste0(main_dir,"Shapes/adm2_3/sen_admbnda_adm2_1m_gov_ocha_20190426"))

# adding a row number to the shape1 object, will be handy for plotting later on
shape1@data$row_num<-1:nrow(shape1@data)
shape2@data$row_num<-1:nrow(shape2@data)


key<-shape2@data[,c("row_num","ADM1_FR","ADM2_FR")]
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


################################################################################
################################################################################
############ Loop through the subsets of data and select a model ###############
################################################################################
################################################################################

indicators<-c("logit_readiness","logit_process_quality")

mga<-c("public","private")
factype<-c("clinic","health center", "hospital")

groups_mga<-expand.grid(outcome=indicators,mga=mga, factype = "ALL")
groups_factype<-expand.grid(outcome=indicators,mga="ALL", factype = factype)
groups_all<-data.frame(outcome=indicators,mga="ALL", factype = "ALL")

results<-rbind(groups_all,groups_mga, groups_factype)
results$var<-paste0("var_",results$outcome)

#######################
# -- cycle through -- #

for(j in 1:nrow(results)){
  print(paste0(j," of ",nrow(results),
               " outcomes groups. ",Sys.time()))
  source("/UsefulFunctions/SAE_Models.R")
  
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
                                 "_",results$outcome[j],"_",results$mga[j],
                                 "_",results$factype[j],".csv"))
  
  
}
