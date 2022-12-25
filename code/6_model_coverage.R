####################################################################################################
## Author:      Adrien Allorant
##
## Description: Read the .RDATA file including the predictions for the areas that were held-out
##              and the direct survey estimates, and compare these two estimates systematically
##              for each area included in the validation_dataset, to calculate:
##              1) the absolute mean error (as a measure of precision)
##              2) the mean error (as a measure of bias)
##              3) the 50%, 80% and 95% coverages (as a measure of calibration)
##
##
## Outputs:     manuscript's figure 2
####################################################################################################


rm(list=ls())


main_dir <- paste0("<<<< FILEPATH REDACTED >>>>")
setwd(main_dir)

#####################################
# -- load packages and functions -- #
source("UsefulFunctions/Packages.R")
source("UsefulFunctions/expit_logit.R")

library(ggplot2)
##############################################
# -- Read in the shapefile -- #
##############################################

indicators<-paste0(substr(c("logit_readiness","logit_process_quality"),7,30))

results<-read_csv("output/ModelSelection/SelectedModels.csv")


table1 <- NULL
prob <- .5
  
  coverage50 <- NULL
  coverage80 <- NULL
  coverage95 <- NULL
  mean_sq_err <- NULL
  n_mean_sq_err <- NULL
  mean_error <- NULL
  mean_abs_error <- NULL
  for (j in 1:nrow(results)) {
    loc<-paste0(substr(results$outcome[j],7,20),
                "_mga",results$mga[j],"_fac",results$factype[j])
    load(paste0("output/PredictiveValidity/",prob,"_",loc,"_model.RDATA"))
    
    survey_est <-pred[,as.character(substr(results$outcome[j],7,20))]
    pred$covered50 <- as.numeric(survey_est > pred$low4 & survey_est < pred$up4)
    pred$covered80 <- as.numeric(survey_est > pred$low3 & survey_est < pred$up3)
    pred$covered95 <- as.numeric(survey_est > pred$low1 & survey_est < pred$up1)
    pred$id <- 1:nrow(pred)
    survey_est <- data.frame(id = 1:length(survey_est), est = survey_est)
    pred <- merge(pred, survey_est, by = "id")
    
    mean_sq_err <- c(mean_sq_err, (1/nrow(pred))*sum((100*pred$est - 100*pred$mean)^2))
    n_mean_sq_err <- c(n_mean_sq_err, (1/nrow(pred))*sum((100*pred$est - 100*pred$mean)^2)/(100*mean(pred$est)))
    mean_error <- c(mean_error, (1/nrow(pred))*sum(100*pred$est - 100*pred$mean))
    mean_abs_error <- c(mean_abs_error, (1/nrow(pred))*sum(abs(100*pred$est - 100*pred$mean)))
    
    coverage50 <- c(coverage50, sum(pred$covered50/nrow(pred)))
    coverage80 <- c(coverage80, sum(pred$covered80/nrow(pred)))
    coverage95 <- c(coverage95, sum(pred$covered95/nrow(pred)))
  }
  table1 <- rbind(table1,
                  data.frame(outcome = results$outcome,
                             mga = results$mga,
                             factype = results$factype,
                             prob = prob,
                             cov50 = coverage50,
                             cov80 = coverage80,
                             cov95 = coverage95,
                             mean_abs_error = mean_abs_error,
                             mean_error = mean_error,
                             mean_sq_err = mean_sq_err,
                             n_mean_sq_err = n_mean_sq_err))
fig2a <- table1 %>%
  filter(prob == .5) %>%
  dplyr::select(outcome, mga, factype, prob,cov50, cov80,cov95, mean_abs_error,mean_error) %>%
  mutate_at(c('cov50','cov80', 'cov95'), function(x) 100*x) %>%
  data.table() %>%
  melt.data.table(id.vars = c('outcome', 'mga', 'factype','prob')) %>%
  filter(variable %in% c('cov50','cov80','cov95')) %>%
  mutate(factype = paste0(mga,".",factype),
         outcome = case_when(
           outcome == 'logit_process_quality' ~ 'Process quality metric',
           outcome == 'logit_readiness' ~ 'Readiness metric'
         ),
         variable = case_when(
           variable == 'cov50' ~ '50% coverage',
           variable == 'cov80' ~ '80% coverage',
           variable == 'cov95' ~ '95% coverage',
           variable == 'mean_abs_error' ~ 'Mean absolute error',
           variable == 'mean_error' ~ 'Mean error'
         ),
         factype = case_when(
           factype == "ALL.ALL" ~ "All facilities",
           factype == "private.ALL" ~ "Private facilities",
           factype == "public.ALL" ~ "Public facilities",
           factype == "ALL.clinic" ~ "Clinics",
           factype == "ALL.health center" ~ "Health centers",
           factype == "ALL.hospital" ~ "Hospitals")
  ) %>%
  filter(factype != "All facilities") %>%
  mutate(outcome = factor(outcome, levels = c('Readiness metric', 'Process quality metric'))) %>%
  ggplot(aes(factype, value, fill = variable, color = variable)) +
  geom_col(position = "dodge", alpha = 0.7, color = NA) +
  geom_hline(yintercept = 80, color = 'dodgerblue3', linetype = 'dotted') +
  geom_hline(yintercept = 95, color = 'green3', linetype = 'dotted') +
  geom_hline(yintercept = 50, color = 'brown3', linetype = 'dotted') +
  # geom_linerange(position = position_dodge(width = 0.9), linewidth = 1.0) +
  scale_color_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1") +
  labs(tag = "B", x = 'Facility type', y = element_blank(),
       color = element_blank(), fill = 'Metric') +
  facet_wrap(~outcome) +
  theme_bw(10) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = rel(1.3)),
    axis.text= element_text(size = rel(1.1)),
    legend.text = element_text(size = rel(1.1)),
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.tag = element_text(face = "bold"),
    legend.background = element_blank(),
    legend.just = c(1, 1)
  )

fig2b <- table1 %>%
  filter(prob == .5) %>%
  dplyr::select(outcome, mga, factype, prob,cov50, cov80,cov95, mean_abs_error,mean_error) %>%
  mutate_at(c('cov50','cov80', 'cov95'), function(x) 100*x) %>%
  data.table() %>%
  melt.data.table(id.vars = c('outcome', 'mga', 'factype','prob')) %>%
  filter(!(variable %in% c('cov50','cov80','cov95'))) %>%
  mutate(factype = paste0(mga,".",factype),
         outcome = case_when(
           outcome == 'logit_process_quality' ~ 'Process quality metric',
           outcome == 'logit_readiness' ~ 'Readiness metric'
         ),
         variable = case_when(
           variable == 'cov50' ~ '50% coverage',
           variable == 'cov80' ~ '80% coverage',
           variable == 'cov95' ~ '95% coverage',
           variable == 'mean_abs_error' ~ 'Mean abs. err.',
           variable == 'mean_error' ~ 'Mean err'
         ),
         factype = case_when(
           factype == "ALL.ALL" ~ "All facilities",
           factype == "private.ALL" ~ "Private facilities",
           factype == "public.ALL" ~ "Public facilities",
           factype == "ALL.clinic" ~ "Clinics",
           factype == "ALL.health center" ~ "Health centers",
           factype == "ALL.hospital" ~ "Hospitals")
  ) %>%
  filter(factype != "All facilities") %>%
  mutate(outcome = factor(outcome, levels = c('Readiness metric', 'Process quality metric'))) %>%
  ggplot(aes(factype, value, fill = variable, color = variable)) +
  geom_col(position = "dodge", alpha = 0.7, color = NA) +
  geom_hline(yintercept = 0, color = 'black', linetype = 'dotted') +
  ylim(-5,15) +
  scale_color_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1") +
  labs(tag = "A", x = element_blank(), y = element_blank(),
       color = element_blank(), fill = 'Metric') +
  facet_wrap(~outcome) +
  theme_bw(10) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = rel(1.3)),
    axis.text= element_text(size = rel(1.1)),
    legend.text = element_text(size = rel(1.1)),
    legend.title =element_text(face = "bold", size = rel(1.3)),
    plot.tag = element_text(face = "bold"),
    axis.text.x = element_blank(),
    legend.background = element_blank(),
    legend.just = c(1, 1)
  )

ggsave(filename = 'manuscript_figure_2.pdf',
       grid.arrange(fig2b, fig2a, heights = c(1,1.05)),
       width = 300, height = 300, units = 'mm')