###############################################################################################################
## figures_plots.r
## Adrien Allorant
## May 2023
## 
## Code to reproduce figures presented in the manuscript
##############################################################################################################

rm(list=ls())
library(scales)
library(biscale)
library(cowplot)
library(sf)
library(PNWColors)
library(data.table)

source('Code/UsefulFunctions/expit_logit.R')

# constants
sae_repo <- "/Users/adrienallorant/Documents/Dissertation/SAE_facility_survey/sae_facility_surveys"

setwd(sae_repo)

# define colors for the maps
pal <- rev(pnw_palette("Sailboat", 30))

# loading shapefiles
shape1 <- read_sf("Shapes/sdr_subnational_boundaries_2020-12-13/shps/sdr_subnational_boundaries.shp")
shape2 <- read_sf("Shapes/adm2_3/sen_admbnda_adm2_1m_gov_ocha_20190426.shp")

shape1$row_num<-1:nrow(shape1)
shape2$row_num<-1:nrow(shape2)

key1<-shape1[,c("row_num","DHSREGEN")]
key2<-shape2[,c("row_num","ADM2_FR","ADM1_FR")]
key <- data.frame(row_num = key2$row_num,
                  department = tolower(key2$ADM2_FR))
#################
## Figure 1 ##
#################
load("Source data/source_data_figure_1.rdata")

colors <- c("#0D4E93", "#ADD9EB",
                     # "#FEEB98",  
                     "#B31F39")
p1 <- st_as_sf(shape1) %>%
  ggplot() +
  geom_sf(fill = "white") +
  geom_point(data = st_as_sf(dat %>% filter(factype != "Case de sante")), aes(x = LONGNUM, y = LATNUM, shape = mga, col = factype)) + 
  geom_sf(data = st_as_sf(shape2), fill = NA, linetype = "dotted") +
  scale_colour_manual("Facility type",values = colors, drop =FALSE) +
  labs(
  ) +
  labs(tag = 'A. ',
       shape = "Managing authority", title = 'Sampled Health Facilities SPA 2017') +
  theme_minimal(10) +
  theme(
    # title = element_text(face = 'bold', size = rel(1.3), hjust = .7),
    legend.title =element_text(face = "bold", size = rel(.7)), 
    plot.tag = element_text(face = "bold"),
    axis.title  = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = rel(0.85), hjust = .5),
    panel.spacing = unit(0, "lines"),    
    axis.text = element_blank(),
    legend.text = element_text(size = rel(.5)),
    legend.background = element_blank()
  )

p2 <- st_as_sf(shape2) %>%
  left_join(direct_est %>% mutate(ADM2_FR = str_to_title(department)) %>%
              dplyr::select(department, ADM2_FR, readiness = sara_index, 
                            process = fifteen.assess) %>%
              data.table() %>%
              melt.data.table(id.vars = c('department','ADM2_FR'))) %>%
  ggplot(aes(fill = value)) +
  geom_sf(size = 0.1, linetype = 'dotted') +
  facet_wrap(~ str_to_title(variable))+
  geom_sf(data = st_as_sf(shape1), fill = NA, lwd = 1.05) +
  scale_fill_viridis_c(option = "D", direction = -1,
                       begin = 0.05, end = 0.9,
                       labels = label_percent(1),
                       limits = c(0.15, 0.9)) +
  labs(tag = "",
       fill = "Percentage") +
  coord_sf(expand = FALSE) +  
  theme_minimal(10) +
  theme(
    panel.grid = element_blank(),
    panel.spacing = unit(0, "lines"),    
    axis.text = element_blank(),
    strip.text = element_text(face = "bold", size = rel(0.95)),
    legend.title = element_text(size = rel(0.7)),
    legend.text = element_text(size = rel(0.7)),
    plot.tag = element_text(face = "bold")
  )

p3 <- st_as_sf(shape2) %>%
  left_join(panelB %>% mutate(ADM2_FR = str_to_title(department)) %>%
              dplyr::select(department, ADM2_FR, readiness, process) %>%
              data.table() %>%
              melt.data.table(id.vars = c('department','ADM2_FR'))) %>%
  ggplot(aes(fill = value)) +
  geom_sf(size = 0.1, linetype = 'dotted') +
  facet_wrap(~ str_to_title(variable))+
  geom_sf(data = st_as_sf(shape1), fill = NA, lwd = 1.05) +
  scale_fill_viridis_c(option = "D", direction = -1,
                       begin = 0.05, end = 0.9,
                       labels = label_percent(1),
                       limits = c(0.15, 0.9)) +
  labs(tag = "B.",
       fill = "Percentage") +
  coord_sf(expand = FALSE) +  
  theme_minimal(10) +
  theme(
    panel.grid = element_blank(),
    panel.spacing = unit(0, "lines"),    
    axis.text = element_blank(),
    strip.text = element_text(face = "bold", size = rel(0.95)),
    legend.title = element_text(size = rel(0.7)),
    legend.text = element_text(size = rel(0.7)),
    plot.tag = element_text(face = "bold")
  )

p4a <- st_as_sf(shape2) %>%
  full_join(panelB %>%
              dplyr::select(department, tt_hcf, hw_density) %>%
              data.table() %>%
              melt.data.table(id.vars = 'department') %>%
              mutate(ADM2_FR = str_to_title(department),
                     variable = case_when(
                       variable == 'tt_hcf' ~ 'Travel time \nnearest facility'
                     ))) %>%
  filter(variable == 'Travel time \nnearest facility') %>%
  ggplot(aes(fill = value)) +
  geom_sf(size = 0.1, linetype = 'dotted') +
  geom_sf(data = st_as_sf(shape1), fill = NA, lwd = 1.05) +
  scale_fill_viridis_c(option = "C", direction = 1,
                       begin = 0.05, end = 0.9) +
  labs(tag = "", title = 'Travel time \nto nearest facility',
       fill = "Minutes") +
  coord_sf(expand = FALSE) +  
  theme_minimal(10) +
  theme(
    panel.grid = element_blank(),
    panel.spacing = unit(0, "lines"),    
    axis.text = element_blank(),
    plot.title = element_text(face = "bold", size = rel(0.95), hjust = .5),
    legend.title = element_text(size = rel(0.7)),
    legend.text = element_text(size = rel(0.7)),
    plot.tag = element_text(face = "bold")
  )

p4b <- st_as_sf(shape2) %>%
  full_join(panelB %>%
              dplyr::select(department, hw_density) %>%
              data.table() %>%
              melt.data.table(id.vars = 'department') %>%
              mutate(ADM2_FR = str_to_title(department),
                     variable = case_when(
                       variable == 'hw_density' ~ 'Health worker density'
                     ))) %>%
  filter(variable ==  'Health worker density') %>%
  ggplot(aes(fill = value)) +
  geom_sf(size = 0.1, linetype = 'dotted') +
  geom_sf(data = st_as_sf(shape1), fill = NA, lwd = 1.05) +
  scale_fill_viridis_c(option = "C", direction = 1,
                       begin = 0.05, end = 0.9,
                       labels = label_number(scale = 1000)) +
  labs(tag = "", title = 'Health worker density',
       fill = "per 1000 \ninhabitants") +
  coord_sf(expand = FALSE) +  
  theme_minimal(10) +
  theme(
    panel.grid = element_blank(),
    panel.spacing = unit(0, "lines"),    
    axis.text = element_blank(),
    plot.title = element_text(face = "bold", size = rel(0.95), hjust = .5),
    legend.title = element_text(size = rel(0.7)),
    legend.text = element_text(size = rel(0.7)),
    plot.tag = element_text(face = "bold")
  )

p4<-grid.arrange(p4a,p4b,nrow = 1, widths = c(1,1))
fig1a <- grid.arrange(p1,p2,nrow = 2)
fig1b <- grid.arrange(p3,p4, nrow = 2, heights = c(1,1))

figure_1 <- grid.arrange(fig1a,fig1b, heights = c(1, 1.05))

###############
## Figure 2 ##
##############

estDF <- readRDS("Source data/source_data_figure_2.RDS")

pA <- estDF %>% filter(
  country == 'Kenya' & indicator == 'readiness'
) %>%
  ggplot() +
  geom_point(aes(x = expit(outcome), y = mean, col = factor(year), shape = factor(year), size = prec)) +
  scale_size(range = c(1,10))+
  scale_y_continuous(labels = label_percent(1)) +
  scale_x_continuous(labels = label_percent(1)) +
  geom_abline(slope = 1, linetype = 'dotted') +            
  scale_color_manual(
    values = c("#999999", "#E69F00", "#56B4E9", "#009E73",
                        "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
                        labels = c("1999" = "SPA 1999",
                                   "2004" = "SPA 2004",
                                   "2010" = "SPA 2010",
                                   "2018" = "SDI 2018")
  ) +
  scale_shape_manual(
    values = c(3:4,15:18),
    # element_blank(),
    labels = c("1999" = "SPA 1999",
               "2004" = "SPA 2004",
               "2010" = "SPA 2010",
               "2018" = "SDI 2018")
  ) +
  labs(tag = 'A. Kenya',
       y = "Predicted", x = "Observed", col = 'Survey', shape = 'Survey', size = 'Precision',
       title = 'Readiness metric') +
  theme_minimal() +
  theme(
    title = element_text(face = 'bold', size = rel(1)),
    legend.title =element_text(face = "bold", size = rel(.8)), 
    plot.tag = element_text(face = "bold", size = rel(.8)),
    plot.title = element_text(face = "bold", size = rel(.8), hjust = .5),
    axis.title  = element_text(face = 'bold',size = rel(.7)),
    axis.text  = element_text(size = rel(.6)),
    legend.text = element_text(size = rel(.7)),
    legend.background = element_blank()
  )

pB <-estDF %>% filter(
  country == 'Kenya' & indicator == 'process_quality'
) %>%
  ggplot() +
  geom_point(aes(x = expit(outcome), y = mean, col = factor(year), shape = factor(year), size = prec)) +
  scale_size(range = c(1,10))+
  scale_y_continuous(labels = label_percent(1)) +
  scale_x_continuous(labels = label_percent(1)) +
  geom_abline(slope = 1, linetype = 'dotted') +            
  scale_color_manual(
    values = c("#999999", "#E69F00", "#56B4E9", "#009E73",
                        "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
                        labels = c("1999" = "SPA 1999",
                                   "2004" = "SPA 2004",
                                   "2010" = "SPA 2010",
                                   "2018" = "SDI 2018")
  ) +
  scale_shape_manual(
    values = c(3:4,15:18),
    # element_blank(),
    labels = c("1999" = "SPA 1999",
               "2004" = "SPA 2004",
               "2010" = "SPA 2010",
               "2018" = "SDI 2018")
  ) +
  labs(tag = 'B. Kenya',
       y = "Predicted", x = "Observed", col = 'Survey', shape = 'Survey', size = 'Precision',
       title = 'Process quality metric') +
  theme_minimal() +
  theme(
    title = element_text(face = 'bold', size = rel(1)),
    legend.title =element_text(face = "bold", size = rel(.8)), 
    plot.tag = element_text(face = "bold", size = rel(.8)),
    plot.title = element_text(face = "bold", size = rel(.8), hjust = .5),
    axis.title  = element_text(face = 'bold',size = rel(.7)),
    axis.text  = element_text(size = rel(.6)),
    legend.text = element_text(size = rel(.7)),
    legend.background = element_blank()
  )

pC <-estDF %>% filter(
  country == 'Senegal' & indicator == 'readiness'
) %>%
  ggplot() +
  geom_point(aes(x = expit(outcome), y = mean, col = factor(year), shape = factor(year), size = prec)) +
  scale_size(range = c(1,10))+
  scale_y_continuous(labels = label_percent(1)) +
  scale_x_continuous(labels = label_percent(1)) +
  geom_abline(slope = 1, linetype = 'dotted') +            
  scale_color_manual(
    values = c("#999999", "#E69F00", "#56B4E9", "#009E73",
                        "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
                        labels = c("2013" = "SPA 2013/14",
                                   "2015" = "SPA 2015/16",
                                   "2017" = "SPA 2017",
                                   "2018" = "SPA 2018",
                                   "2019" = "SPA 2019-20")
  ) +
  scale_shape_manual(
    values = c(3:4,15:18),
    # element_blank(),
    labels = c("2013" = "SPA 2013/14",
               "2015" = "SPA 2015/16",
               "2017" = "SPA 2017",
               "2018" = "SPA 2018",
               "2019" = "SPA 2019-20")
  ) +
  labs(tag = 'C. Senegal',
       y = "Predicted", x = "Observed", col = 'Survey', shape = 'Survey', size = 'Precision',
       title = 'Readiness metric') +
  theme_minimal() +
  theme(
    title = element_text(face = 'bold', size = rel(1)),
    legend.title =element_text(face = "bold", size = rel(.8)), 
    plot.tag = element_text(face = "bold", size = rel(.8)),
    plot.title = element_text(face = "bold", size = rel(.8), hjust = .5),
    axis.title  = element_text(face = 'bold',size = rel(.7)),
    axis.text  = element_text(size = rel(.6)),
    legend.text = element_text(size = rel(.7)),
    legend.background = element_blank()
  )

pD<-estDF %>% filter(
  country == 'Senegal' & indicator == 'process_quality'
) %>%
  ggplot() +
  geom_point(aes(x = expit(outcome), y = mean, col = factor(year), shape = factor(year), size = prec)) +
  scale_size(range = c(1,10))+
  scale_y_continuous(labels = label_percent(1)) +
  scale_x_continuous(labels = label_percent(1)) +
  geom_abline(slope = 1, linetype = 'dotted') +            
  scale_color_manual(
    values = c("#999999", "#E69F00", "#56B4E9", "#009E73",
                        "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
                        labels = c("2013" = "SPA 2013/14",
                                   "2015" = "SPA 2015/16",
                                   "2017" = "SPA 2017",
                                   "2018" = "SPA 2018",
                                   "2019" = "SPA 2019-20")
  ) +
  scale_shape_manual(
    values = c(3:4,15:18),
    # element_blank(),
    labels = c("2013" = "SPA 2013/14",
               "2015" = "SPA 2015/16",
               "2017" = "SPA 2017",
               "2018" = "SPA 2018",
               "2019" = "SPA 2019-20")
  ) +
  labs(tag = 'D. Senegal',
       y = "Predicted", x = "Observed", col = 'Survey', shape = 'Survey', size = 'Precision',
       title = 'Process quality metric') +
  theme_minimal() +
  theme(
    title = element_text(face = 'bold', size = rel(1)),
    legend.title =element_text(face = "bold", size = rel(.8)), 
    plot.tag = element_text(face = "bold", size = rel(.8)),
    plot.title = element_text(face = "bold", size = rel(.8), hjust = .5),
    axis.title  = element_text(face = 'bold',size = rel(.7)),
    axis.text  = element_text(size = rel(.6)),
    legend.text = element_text(size = rel(.7)),
    legend.background = element_blank()
  )

pE<-estDF %>% filter(
  country == 'Tanzania' & indicator == 'readiness'
) %>%
  ggplot() +
  geom_point(aes(x = expit(outcome), y = mean, col = factor(year), shape = factor(year), size = prec)) +
  scale_size(range = c(1,10))+
  scale_y_continuous(labels = label_percent(1)) +
  scale_x_continuous(labels = label_percent(1)) +
  geom_abline(slope = 1, linetype = 'dotted') +            
  scale_color_manual(
    values = c("#999999", "#E69F00", "#56B4E9", "#009E73",
                        "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
                        labels = c("2006" = "SPA 2006",
                                   "2014" = "SDI 2014",
                                   "2015" = "SPA 2014-15",
                                   "2016" = "SDI 2016")
  ) +
  scale_shape_manual(
    values = c(3:4,15:18),
    # element_blank(),
    labels = c("2006" = "SPA 2006",
               "2014" = "SDI 2014",
               "2015" = "SPA 2014-15",
               "2016" = "SDI 2016")
  ) +
  labs(tag = 'E. Tanzania',
       y = "Predicted", x = "Observed", col = 'Survey', shape = 'Survey', size = 'Precision',
       title = 'Readiness metric') +
  theme_minimal() +
  theme(
    title = element_text(face = 'bold', size = rel(1)),
    legend.title =element_text(face = "bold", size = rel(.8)), 
    plot.tag = element_text(face = "bold", size = rel(.8)),
    plot.title = element_text(face = "bold", size = rel(.8), hjust = .5),
    axis.title  = element_text(face = 'bold',size = rel(.7)),
    axis.text  = element_text(size = rel(.6)),
    legend.text = element_text(size = rel(.7)),
    legend.background = element_blank()
  )

pF<-estDF %>% filter(
  country == 'Tanzania' & indicator == 'process_quality'
) %>%
  ggplot() +
  geom_point(aes(x = expit(outcome), y = mean, col = factor(year), shape = factor(year), size = prec)) +
  scale_size(range = c(1,10))+
  scale_y_continuous(labels = label_percent(1)) +
  scale_x_continuous(labels = label_percent(1)) +
  geom_abline(slope = 1, linetype = 'dotted') +            
  scale_color_manual(
    values = c("#999999", "#E69F00", "#56B4E9", "#009E73",
                        "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
                        labels = c("2006" = "SPA 2006",
                                   "2014" = "SDI 2014",
                                   "2015" = "SPA 2014-15",
                                   "2016" = "SDI 2016")
  ) +
  scale_shape_manual(
    values = c(3:4,15:18),
    # element_blank(),
    labels = c("2006" = "SPA 2006",
               "2014" = "SDI 2014",
               "2015" = "SPA 2014-15",
               "2016" = "SDI 2016")
  ) +
  labs(tag = 'F. Tanzania',
       y = "Predicted", x = "Observed", col = 'Survey', shape = 'Survey', size = 'Precision',
       title = 'Process quality metric') +
  theme_minimal() +
  theme(
    title = element_text(face = 'bold', size = rel(1)),
    legend.title =element_text(face = "bold", size = rel(.8)), 
    plot.tag = element_text(face = "bold", size = rel(.8)),
    plot.title = element_text(face = "bold", size = rel(.8), hjust = .5),
    axis.title  = element_text(face = 'bold',size = rel(.7)),
    axis.text  = element_text(size = rel(.6)),
    legend.text = element_text(size = rel(.7)),
    legend.background = element_blank()
  )

grid.arrange(pA + guides(shape = 'none', color = 'none') ,
             NULL,
             pB,
             pC + guides(shape = 'none', color = 'none'),
             NULL,
             pD,
             pE + guides(shape = 'none', color = 'none'),
             NULL,
             pF, nrow = 3,
             heights = c(1,1,1.01),
             widths = c(1,-.1,1.01))

###############
## Figure 3 ##
##############

mse_cov <- readRDS("Source data/source_data_figure_3.RDS")

fig2a <- mse_cov %>%
  dplyr::select(outcome, country, cov50, cov80,cov95, mean_abs_error,mean_error) %>%
  mutate_at(c('cov50','cov80', 'cov95'), function(x) 100*x) %>%
  data.table() %>%
  melt.data.table(id.vars = c('outcome', 'country')) %>%
  filter(variable %in% c('cov50','cov80','cov95')) %>%
  mutate(
    outcome = case_when(
      outcome == 'logit_fifteen.assess' ~ 'Process quality metric',
      outcome == 'logit_sara_index' ~ 'Readiness metric'
    ),
    variable = case_when(
      variable == 'cov50' ~ '50% coverage',
      variable == 'cov80' ~ '80% coverage',
      variable == 'cov95' ~ '95% coverage',
      variable == 'mean_abs_error' ~ 'Mean absolute error',
      variable == 'mean_error' ~ 'Mean error'
    )
  ) %>%
  mutate(outcome = factor(outcome, levels = c('Readiness metric', 'Process quality metric'))) %>%
  ggplot(aes(country, value, fill = variable, color = variable)) +
  geom_col(position = "dodge", alpha = 0.7, color = NA) +
  geom_hline(yintercept = 80, color = 'dodgerblue3', linetype = 'dotted') +
  geom_hline(yintercept = 95, color = 'green3', linetype = 'dotted') +
  geom_hline(yintercept = 50, color = 'brown3', linetype = 'dotted') +
  # geom_linerange(position = position_dodge(width = 0.9), linewidth = 1.0) +
  scale_color_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1") +
  labs(tag = "B", x = '', y = element_blank(),
       color = element_blank(), fill = element_blank()) +
  facet_wrap(~outcome) +
  theme_bw(10) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = rel(.95)),
    axis.text= element_text(size = rel(.8)),
    legend.text = element_text(size = rel(.85)),
    legend.title =element_text(face = "bold", size = rel(.9)), 
    plot.tag = element_text(face = "bold", size = rel(.9)),
    # axis.text.x = element_blank(),
    # legend.position = c(1,.9),
    legend.background = element_blank(),
    legend.just = c(1, 1)
  )

fig2b <- mse_cov %>%
  dplyr::select(outcome, country, cov50, cov80,cov95, mean_abs_error,mean_error) %>%
  mutate_at(c('cov50','cov80', 'cov95'), function(x) 100*x) %>%
  data.table() %>%
  melt.data.table(id.vars = c('outcome', 'country')) %>%
  filter(!(variable %in% c('cov50','cov80','cov95'))) %>%
  mutate(
    outcome = case_when(
      outcome == 'logit_fifteen.assess' ~ 'Process quality metric',
      outcome == 'logit_sara_index' ~ 'Readiness metric'
    ),
    variable = case_when(
      variable == 'cov50' ~ '50% coverage',
      variable == 'cov80' ~ '80% coverage',
      variable == 'cov95' ~ '95% coverage',
      variable == 'mean_abs_error' ~ 'Mean abs. err.',
      variable == 'mean_error' ~ 'Mean err'
    )
  ) %>%
  mutate(outcome = factor(outcome, levels = c('Readiness metric', 'Process quality metric'))) %>%
  ggplot(aes(country, value, fill = variable, color = variable)) +
  geom_col(position = "dodge", alpha = 0.7, color = NA) +
  geom_hline(yintercept = 0, color = 'black', linetype = 'dotted') +
  ylim(-2.5,7.5) +
  # geom_linerange(position = position_dodge(width = 0.9), linewidth = 1.0) +
  scale_color_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1") +
  labs(tag = "A", x = element_blank(), y = element_blank(),
       color = element_blank(), fill = element_blank()) +
  facet_wrap(~outcome) +
  theme_bw(10) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = rel(.95)),
    axis.text= element_text(size = rel(.8)),
    legend.text = element_text(size = rel(.85)),
    legend.title =element_text(face = "bold", size = rel(.9)), 
    plot.tag = element_text(face = "bold", size = rel(.9)),
    axis.text.x = element_blank(),
    # legend.position = c(1,.9),
    legend.background = element_blank(),
    legend.just = c(1, 1)
  )

grid.arrange(fig2b, fig2a, heights = c(1,1.05))

##############
## Figure 4 ##
##############

fig4DF <- readRDS("Source data/source_data_figure_4.RDS")


nice_col <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                       "#44AA99", "#999933", "#882255", "#661100",  "#888888")

p_1 <- fig4DF %>%
  filter(indicator == 'sara_index') %>%
  ggplot(aes(x = fct_reorder(str_to_title(department), mean, .desc = TRUE), y = mean)) +
  geom_linerange(aes(ymin = low, ymax = up, color = "Model 95% UI"), linewidth = 1.5) +
  geom_crossbar(ymin = NA, ymax = NA, width = 0.6, size = 0.4, color = nice_col[1]) +
  geom_linerange(aes(ymin = pp_lower95, ymax = pp_upper95, color = "Survey 95%CI"), position = position_dodge(width = 0.35)) +
  geom_point(aes(y = estimate, color = "Survey Mean"), position = position_dodge(width = 0.35)) +
  theme_bw(8) +
  labs(tag = "A.", y = element_blank(), x = element_blank(), shape = element_blank()) +
  scale_color_manual(name = "Estimates",
                     breaks = c("Model 95% UI", "Survey Mean", "Survey 95%CI"),
                     values = c("Survey 95%CI" = nice_col[2], "Survey Mean" = nice_col[3], "Model 95% UI" = nice_col[1]) ) +
  scale_y_continuous('Readiness metric',
                     labels = label_percent(1)) +
  expand_limits(y = 0) +
  theme(
    legend.position = 'none',
    legend.just = c(1, 0.85),
    legend.background = element_rect(fill = NA),
    legend.key.size = unit(1, "lines"),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 5, angle = 30, hjust = 1),
    plot.tag = element_text(face = "bold"),
    plot.tag.position = c(0, 1.0),
    plot.background = element_rect(fill = NA),
    plot.margin = margin(12, 2, 0, 10, "pt")
  )

p_2 <- fig4DF %>%
  filter(indicator == 'fifteen.assess') %>%
  ggplot(aes(x = fct_reorder(str_to_title(department), mean, .desc = TRUE), y = mean)) +
  geom_linerange(aes(ymin = low, ymax = up, color = "Model 95% UI"), linewidth = 1.5) +
  geom_crossbar(ymin = NA, ymax = NA, width = 0.6, size = 0.4, color = nice_col[1]) +
  geom_linerange(aes(ymin = pp_lower95, ymax = pp_upper95, color = "Survey 95%CI"), position = position_dodge(width = 0.35)) +
  geom_point(aes(y = estimate, color = "Survey Mean"), position = position_dodge(width = 0.35)) +
  theme_bw(8) +
  labs(tag = "B.", y = element_blank(), x = element_blank(), shape = element_blank()) +
  scale_color_manual(name = "Estimates",
                     breaks = c("Model 95% UI", "Survey Mean", "Survey 95%CI"),
                     values = c("Survey 95%CI" = nice_col[2], "Survey Mean" = nice_col[3], "Model 95% UI" = nice_col[1]) ) +
  scale_y_continuous('Process quality metric',
                     labels = label_percent(1)) +
  expand_limits(y = 0) +
  theme(
    legend.position = 'none',
    legend.just = c(1, 0.85),
    legend.background = element_rect(fill = NA),
    legend.key.size = unit(1, "lines"),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 5, angle = 30, hjust = 1),
    plot.tag = element_text(face = "bold"),
    plot.tag.position = c(0, 1.0),
    plot.background = element_rect(fill = NA),
    plot.margin = margin(12, 2, 0, 10, "pt")
  )

grid.arrange(p_1, p_2, heights = c(1, 1))

#################
## Figures 5-7 ##
#################

estDF <- readRDS("Source data/source_data_figure_5.RDS")

fig5a <- st_as_sf(shape2) %>%
  full_join(estDF %>%
              filter(indicator == "Average readiness (%)") %>%
              left_join(key)) %>%
  ggplot(aes(fill = mean)) +
  geom_sf(size = 0.1) +
  scale_fill_gradientn(colors= pal,
                       labels = label_percent(1)) +
  expand_limits(fill = c(0,1)) +
  labs(x = NULL, y = NULL, title ="Average Readiness (%)") +
  theme_classic(base_size = 12) +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = rel(1.15)),
        plot.margin = unit(c(0, 0, 0, 0), "in"), legend.title=element_blank())

estDF$ci <- estDF$up - estDF$low # absolute uncertainty
data <- bi_class(estDF, y = ci, x = mean, style = "quantile", dim = 4)

custom_pal4 <- c("1-1" ="#EA818F", "2-1" = "#E7A184","3-1" = "#88C7E2", "4-1"= "#8AB5DF",
                 "1-2" = "#EDAAB3", "2-2" = "#EEBEAA", "3-2" = "#ADD8EB", "4-2" = "#B1CBE6",
                 "1-3" = "#EFD4DB", "2-3" = "#EEDBD2", "3-3" ="#CBE2EB", "4-3" ="#CDD8EC",
                 "1-4" = "#F9F4F8", "2-4" ="#FAF7F5", "3-4" = "#EEF4F7", "4-4" ="#EDF1F7")

map_uiA <- st_as_sf(shape2) %>%
  full_join(data %>%
              left_join(key)) %>%
  ggplot() +
  geom_sf(aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = custom_pal4, dim = 4) +
  bi_theme()+
  labs(title = c("Readiness Uncertainty")) +
  coord_sf(expand = FALSE)+
  theme_minimal(10) +
  theme(
    plot.title = element_text(hjust = 0.5, size = rel(1.15)),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    panel.spacing = unit(0, "lines")
  )

fig5b <- st_as_sf(shape2) %>%
  full_join(estDF %>%
              filter(indicator == "Average process quality (%)") %>%
              left_join(key)) %>%
  ggplot(aes(fill = mean)) +
  geom_sf(size = 0.1) +
  scale_fill_gradientn(colors= pal,
                       labels = label_percent(1)) +
  expand_limits(fill = c(0,1)) +
  labs(x = NULL, y = NULL, title ="Average Process Quality (%)") +
  theme_classic(base_size = 12) +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = rel(1.15)),
        plot.margin = unit(c(0, 0, 0, 0), "in"), legend.title=element_blank())

estDF$ci <- estDF$up - estDF$low # absolute uncertainty
data <- bi_class(estDF, y = ci, x = mean, style = "quantile", dim = 4)

custom_pal4 <- c("1-1" ="#EA818F", "2-1" = "#E7A184","3-1" = "#88C7E2", "4-1"= "#8AB5DF",
                 "1-2" = "#EDAAB3", "2-2" = "#EEBEAA", "3-2" = "#ADD8EB", "4-2" = "#B1CBE6",
                 "1-3" = "#EFD4DB", "2-3" = "#EEDBD2", "3-3" ="#CBE2EB", "4-3" ="#CDD8EC",
                 "1-4" = "#F9F4F8", "2-4" ="#FAF7F5", "3-4" = "#EEF4F7", "4-4" ="#EDF1F7")

map_uiB <- st_as_sf(shape2) %>%
  full_join(data %>%
              left_join(key)) %>%
  ggplot() +
  geom_sf(aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = custom_pal4, dim = 4) +
  bi_theme()+
  labs(title = c("Process Quality Uncertainty")) +
  coord_sf(expand = FALSE)+
  theme_minimal(10) +
  theme(
    plot.title = element_text(hjust = 0.5, size = rel(1.15)),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    panel.spacing = unit(0, "lines")
  )


# legend
legend <- bi_legend(pal = custom_pal4, dim=4, 
                    xlab= paste0("Higher average Readiness/Process quality"),
                    ylab="Higher Uncertainty",
                    size=10)

fig5A <- grid.arrange(fig5a, map_uiA, widths = c(1.25, 1), nrow = 1)
fig5B <- grid.arrange(fig5b, map_uiB, widths = c(1.25, 1), nrow = 1)

grid.arrange(fig5A,fig5B,legend, nrow=3)

##############
## Figure 6 ##
##############

estDF <- readRDS("Source data/source_data_figure_6.RDS")

fig6a <- st_as_sf(shape2) %>%
  full_join(estDF %>%
              filter(indicator == "Average readiness (%)" & managing_authority == 'public') %>%
              left_join(key)) %>%
  ggplot(aes(fill = mean)) +
  geom_sf(size = 0.1) +
  scale_fill_gradientn(colors= pal,
                       labels = label_percent(1)) +
  expand_limits(fill = c(0,1)) +
  labs(tag = "A.",
       fill = "",
       title = c("Readiness Public Facilities (%)")) +
  coord_sf(expand = FALSE) +  
  theme_minimal(10) +
  theme(
    plot.title = element_text(hjust = 0.5, size = rel(1.15)),
    panel.grid = element_blank(),
    panel.spacing = unit(0, "lines"),
    axis.text = element_blank(),
    strip.text = element_text(face = "bold", size = rel(0.85)),
    legend.title = element_text(size = rel(0.7)),
    legend.text = element_text(size = rel(0.7)),
    legend.position = c('none'),
    plot.tag = element_text(face = "bold")
  )
fig6b <- st_as_sf(shape2) %>%
  full_join(estDF %>%
              filter(indicator == "Average readiness (%)" & managing_authority == 'private') %>%
              left_join(key)) %>%
  ggplot(aes(fill = mean)) +
  geom_sf(size = 0.1) +
  scale_fill_gradientn(colors= pal,
                       labels = label_percent(1)) +
  expand_limits(fill = c(0,1)) +
  labs(tag = "B.",
       fill = "",
       title = c("Readiness Private facilities (%)")) +
  coord_sf(expand = FALSE) +  
  theme_minimal(10) +
  theme(
    plot.title = element_text(hjust = 0.5, size = rel(1.15)),
    panel.grid = element_blank(),
    panel.spacing = unit(0, "lines"),
    axis.text = element_blank(),
    strip.text = element_text(face = "bold", size = rel(0.85)),
    legend.title = element_text(size = rel(0.7)),
    legend.text = element_text(size = rel(0.7)),
    legend.position = c('right'),
    plot.tag = element_text(face = "bold")
  )

fig6c <- st_as_sf(shape2) %>%
  full_join(estDF %>%
              filter(indicator == "Average process quality (%)" & managing_authority == 'public') %>%
              left_join(key)) %>%
  ggplot(aes(fill = mean)) +
  geom_sf(size = 0.1) +
  scale_fill_gradientn(colors= pal,
                       labels = label_percent(1)) +
  expand_limits(fill = c(0,1)) +
  labs(tag = "C.",
       fill = "",
       title = c("Process quality Public Facilities (%)")) +
  coord_sf(expand = FALSE) +  
  theme_minimal(10) +
  theme(
    plot.title = element_text(hjust = 0.5, size = rel(1.15)),
    panel.grid = element_blank(),
    panel.spacing = unit(0, "lines"),
    axis.text = element_blank(),
    strip.text = element_text(face = "bold", size = rel(0.85)),
    legend.title = element_text(size = rel(0.7)),
    legend.text = element_text(size = rel(0.7)),
    legend.position = c('none'),
    plot.tag = element_text(face = "bold")
  )
fig6d <- st_as_sf(shape2) %>%
  full_join(estDF %>%
              filter(indicator == "Average process quality (%)" & managing_authority == 'private') %>%
              left_join(key)) %>%
  ggplot(aes(fill = mean)) +
  geom_sf(size = 0.1) +
  scale_fill_gradientn(colors= pal,
                       labels = label_percent(1)) +
  expand_limits(fill = c(0,1)) +
  labs(tag = "D.",
       fill = "",
       title = c("Process quality Private facilities (%)")) +
  coord_sf(expand = FALSE) +  
  theme_minimal(10) +
  theme(
    plot.title = element_text(hjust = 0.5, size = rel(1.15)),
    panel.grid = element_blank(),
    panel.spacing = unit(0, "lines"),
    axis.text = element_blank(),
    strip.text = element_text(face = "bold", size = rel(0.85)),
    legend.title = element_text(size = rel(0.7)),
    legend.text = element_text(size = rel(0.7)),
    legend.position = c('right'),
    plot.tag = element_text(face = "bold")
  )

grid.arrange(fig6a,fig6b,fig6c,fig6d,
             widths = c(1,1.25), nrow = 2)

##############
## Figure 7 ##
##############

estDF <- readRDS("Source data/source_data_figure_7.RDS")

fig7a <- st_as_sf(shape2) %>%
  full_join(estDF %>%
              filter(indicator == "Average readiness (%)" & fac_type == "hospital") %>%
              left_join(key)) %>%
  ggplot(aes(fill = mean)) +
  geom_sf(size = 0.1) +
  scale_fill_gradientn(colors= pal,
                       labels = label_percent(1)) +
  expand_limits(fill = c(0,1)) +
  labs(tag = "A.",
       fill = "",
       title = c("Readiness Hospitals (%)")) +
  coord_sf(expand = FALSE) +  
  theme_minimal(10) +
  theme(
    plot.title = element_text(hjust = 0.5, size = rel(1.15)),
    panel.grid = element_blank(),
    panel.spacing = unit(0, "lines"),
    axis.text = element_blank(),
    strip.text = element_text(face = "bold", size = rel(0.85)),
    legend.title = element_text(size = rel(0.7)),
    legend.text = element_text(size = rel(0.7)),
    legend.position = c('none'),
    plot.tag = element_text(face = "bold")
  )

fig7b <- st_as_sf(shape2) %>%
  full_join(estDF %>%
              filter(indicator == "Average readiness (%)" & fac_type == "health center") %>%
              left_join(key)) %>%
  ggplot(aes(fill = mean)) +
  geom_sf(size = 0.1) +
  scale_fill_gradientn(colors= pal,
                       labels = label_percent(1)) +
  expand_limits(fill = c(0,1)) +
  labs(tag = "B.",
       fill = "",
       title = c("Readiness Health Centers (%)")) +
  coord_sf(expand = FALSE) +  
  theme_minimal(10) +
  theme(
    plot.title = element_text(hjust = 0.5, size = rel(1.15)),
    panel.grid = element_blank(),
    panel.spacing = unit(0, "lines"),
    axis.text = element_blank(),
    strip.text = element_text(face = "bold", size = rel(0.85)),
    legend.title = element_text(size = rel(0.7)),
    legend.text = element_text(size = rel(0.7)),
    legend.position = c('none'),
    plot.tag = element_text(face = "bold")
  )

fig7c <- st_as_sf(shape2) %>%
  full_join(estDF %>%
              filter(indicator == "Average readiness (%)" & fac_type == "clinic") %>%
              left_join(key)) %>%
  ggplot(aes(fill = mean)) +
  geom_sf(size = 0.1) +
  scale_fill_gradientn(colors= pal,
                       labels = label_percent(1)) +
  expand_limits(fill = c(0,1)) +
  labs(tag = "C.",
       fill = "",
       title = c("Readiness Clinics (%)")) +
  coord_sf(expand = FALSE) +  
  theme_minimal(10) +
  theme(
    plot.title = element_text(hjust = 0.5, size = rel(1.15)),
    panel.grid = element_blank(),
    panel.spacing = unit(0, "lines"),
    axis.text = element_blank(),
    strip.text = element_text(face = "bold", size = rel(0.85)),
    legend.title = element_text(size = rel(0.7)),
    legend.text = element_text(size = rel(0.7)),
    legend.position = c('none'),
    plot.tag = element_text(face = "bold")
  )

fig7d <- st_as_sf(shape2) %>%
  full_join(estDF %>%
              filter(indicator == "Average process quality (%)" & fac_type == "hospital") %>%
              left_join(key)) %>%
  ggplot(aes(fill = mean)) +
  geom_sf(size = 0.1) +
  scale_fill_gradientn(colors= pal,
                       labels = label_percent(1)) +
  expand_limits(fill = c(0,1)) +
  labs(tag = "D.",
       fill = "",
       title = c("Process Quality Hospitals (%)")) +
  coord_sf(expand = FALSE) +  
  theme_minimal(10) +
  theme(
    plot.title = element_text(hjust = 0.5, size = rel(1.15)),
    panel.grid = element_blank(),
    panel.spacing = unit(0, "lines"),
    axis.text = element_blank(),
    strip.text = element_text(face = "bold", size = rel(0.85)),
    legend.title = element_text(size = rel(0.7)),
    legend.text = element_text(size = rel(0.7)),
    legend.position = c('right'),
    plot.tag = element_text(face = "bold")
  )

fig7e <- st_as_sf(shape2) %>%
  full_join(estDF %>%
              filter(indicator == "Average process quality (%)" & fac_type == "health center") %>%
              left_join(key)) %>%
  ggplot(aes(fill = mean)) +
  geom_sf(size = 0.1) +
  scale_fill_gradientn(colors= pal,
                       labels = label_percent(1)) +
  expand_limits(fill = c(0,1)) +
  labs(tag = "E.",
       fill = "",
       title = c("Process Quality Health Centers (%)")) +
  coord_sf(expand = FALSE) +  
  theme_minimal(10) +
  theme(
    plot.title = element_text(hjust = 0.5, size = rel(1.15)),
    panel.grid = element_blank(),
    panel.spacing = unit(0, "lines"),
    axis.text = element_blank(),
    strip.text = element_text(face = "bold", size = rel(0.85)),
    legend.title = element_text(size = rel(0.7)),
    legend.text = element_text(size = rel(0.7)),
    legend.position = c('right'),
    plot.tag = element_text(face = "bold")
  )

fig7f <- st_as_sf(shape2) %>%
  full_join(estDF %>%
              filter(indicator == "Average process quality (%)" & fac_type == "clinic") %>%
              left_join(key)) %>%
  ggplot(aes(fill = mean)) +
  geom_sf(size = 0.1) +
  scale_fill_gradientn(colors= pal,
                       labels = label_percent(1)) +
  expand_limits(fill = c(0,1)) +
  labs(tag = "F.",
       fill = "",
       title = c("Process Quality Clinics (%)")) +
  coord_sf(expand = FALSE) +  
  theme_minimal(10) +
  theme(
    plot.title = element_text(hjust = 0.5, size = rel(1.15)),
    panel.grid = element_blank(),
    panel.spacing = unit(0, "lines"),
    axis.text = element_blank(),
    strip.text = element_text(face = "bold", size = rel(0.85)),
    legend.title = element_text(size = rel(0.7)),
    legend.text = element_text(size = rel(0.7)),
    legend.position = c('right'),
    plot.tag = element_text(face = "bold")
  )
grid.arrange(fig7a,NULL, fig7d,
             fig7b,NULL,fig7e,
             fig7c,NULL,fig7f,
             widths = c(1,-.1,1), ncol = 3, nrow = 3)
