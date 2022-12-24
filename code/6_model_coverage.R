#####################################################
##
##
## Fitting the selected models for all indicators and groups
## Last Modified: August 31st 2021

rm(list=ls())


country.file <- paste0("<<<< FILEPATH REDACTED >>>>", "/SENEGAL/")
setwd(country.file)

#####################################
# -- load packages and functions -- #

source("UsefulFunctions/Packages.R")
source("UsefulFunctions/expit_logit.R")

##############################################
# -- Read in the shapefile -- #
##############################################

indicators<-paste0(substr(c("logit_sara_index","logit_fifteen.assess"),7,30))

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

coverage <- NULL
mse <- NULL
nmse <- NULL
for (loc in indicators) {
  # loc<- indicators[1]
  load(paste0(country.file,"output/PredictiveValidity/",loc,"_model.RDATA"))
  
  survey_est <-pred[,as.character(loc)]
  pred$covered <- as.numeric(survey_est > pred$low & survey_est < pred$up)
  coverage <- cbind(coverage, sum(pred$covered/nrow(pred)))
  pred$id <- 1:nrow(pred)
  survey_est <- data.frame(id = 1:length(survey_est), est = survey_est)
  pred <- merge(pred, survey_est, by = "id")
  
  mse <- cbind(mse, (1/nrow(pred))*sum((100*pred$est - 100*pred$mean)^2))
  nmse <- cbind(nmse, (1/nrow(pred))*sum((100*pred$est - 100*pred$mean)^2)/(100*mean(pred$est)))
  
  pred <- pred %>%
    mutate(
      Coverage = case_when(
        covered == 1 ~ "Covered",
        covered == 0 ~ "Not covered"
      )
    ) %>%
    mutate_at(c("low","up","est","mean"), function(x) 100*x)
  

  p1 <- ggplot() +
    geom_errorbar(data = pred, aes(x = id, ymin = low, ymax = up, 
                                   color = Coverage)) +
    geom_point(data = pred, aes(x = id, y = est, color = Coverage)) +
    ggtitle(paste0(c("A","B","C","D")[which(loc == indicators)],". ",
                   c("Readiness",
                     "Process quality", "Main symptoms")[which(loc == indicators)])) +
    scale_y_continuous(limits= c(0,100), breaks=seq(0,100,20)) +
    theme_classic() +
    labs(x ='', y = 'Estimate', color = "Coverage") +
    theme(
          axis.text.x = element_blank()) +
    scale_color_manual(values=c("#800080","#FFA500"))

  assign(paste0("p_",loc), p1 + theme(legend.position = 'none'))
  if(loc == "fifteen.assess") {legend <- get_legend(p1)}
  
}

ggsave(
  grid.arrange(p_sara_index,p_fifteen.assess,legend,
               ncol=3, nrow = 1, widths = c(4/9,4/9,1/9)), filename = paste0(country.file, "output/coverage_SEN.png"),
       width = 9, height = 6)

