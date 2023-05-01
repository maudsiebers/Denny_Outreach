##### Girl Guides Outreach event #####
### Maud Siebers
### Created: 26-04-2023
### last changed: xxx
# author: Maud Siebers


#### Worksheet setup ####
cat("\f")
rm(list = ls())
library(RColorBrewer)
library(rLakeAnalyzer)
library(tidyverse)
library(lubridate)
library(GLM3r)
library(glmtools)
library(ggplot2)
library(ggpubr)

Sys.setenv(TZ="Etc/GMT+0")
Sys.getenv("tz")

#Setting the working directory 
setwd('C:/Users/ms128/Denny_Outreach/Model')
sim_folder <- getwd()

#### changing parameters 

name <- 'Test_Maud'
phos <-
Nitrate <-
Silica <-
Temp <- 2  # times .... 






#### model things
setwd(sim_folder)


glm_template = 'glm3_prediction.nml' 

eg_nml <- read_nml(nml_file = glm_template)

### name output 
eg_nml <- set_nml(eg_nml, 'out_fn', paste0('Prediction_',name))


##### temp 
eg_nml <- set_nml(eg_nml, 'at_factor', Temp)


##### write to file 
write_nml(eg_nml, file = glm_template)
###############################################################################
####run the model
file.copy(glm_template, 'glm3.nml', overwrite = TRUE)
nml_file <- file.path(sim_folder, 'glm3.nml')
GLM3r::run_glm(sim_folder, nml_file = 'glm3.nml', verbose = T)

##### visualise the results 
Original <- get_var(file = paste0(sim_folder,'/Output/Prediction_Original.nc'), 
                       var_name = 'PHY_TCHLA',
                       reference = 'surface',
                       z_out = 0:5)  
Original$org <- rowMeans(Original[,2:5], na.rm = TRUE)
Original$DateTime <- as.Date(Original$DateTime)



 
avg_chl_new <- get_var(file = paste0(sim_folder,'/Output/', get_nml_value(eg_nml, 'out_fn'),'.nc'), 
                       var_name = 'PHY_TCHLA',
                       reference = 'surface',
                       z_out = 0:5)  
avg_chl_new$avg <- rowMeans(avg_chl_new[,2:5], na.rm = TRUE)
avg_chl_new$DateTime <- as.Date(avg_chl_new$DateTime)

chlplot <- data.frame(date = Original$DateTime, original = Original$org, prediction = avg_chl_new$avg)
chlplot <- reshape2::melt(chlplot, id = c('date'))
ggplot(chlplot, aes(date, value, col = variable))+
  geom_line()+
theme(plot.title = element_text(size = 30), legend.title = element_blank(),
        axis.title.y = element_text(size = 20), legend.text = element_text(size = 20))+
  xlab(label = '') + ylab(label = 'Total Chla (ug/L)')+
  #xlim(as.Date(c('2016-01-01', '2020-12-31'), tz = 'UTC', 
   #            format = '%Y-%m-%d'))+
  geom_hline(yintercept = 35, col='red')


surface_ox_org <- get_var(file = paste0(sim_folder,'/Output/Prediction_Original.nc'), 
                      var_name = 'OXY_oxy',
                      reference = 'surface',
                      z_out = 0)
bottom_ox_org <- get_var(file = paste0(sim_folder,'/Output/Prediction_Original.nc'), 
                     var_name = 'OXY_oxy',
                     reference = 'surface',
                     z_out = 12)


surface_ox <- get_var(file = paste0(sim_folder,'/Output/', get_nml_value(eg_nml, 'out_fn'),'.nc'), 
                      var_name = 'OXY_oxy',
                      reference = 'surface',
                      z_out = 0)
bottom_ox <- get_var(file = paste0(sim_folder,'/Output/', get_nml_value(eg_nml, 'out_fn'),'.nc'), 
                     var_name = 'OXY_oxy',
                     reference = 'surface',
                     z_out = 12)


Oxy <-ggplot()+
  geom_line(data = surface_ox, aes(as.Date(DateTime), OXY_oxy_0, col = 'Surf. Mod.'))+
  geom_line(data = bottom_ox, aes(as.Date(DateTime), OXY_oxy_12, col = 'Bot. Mod.'))+
  geom_point(data = surface_ox_org, aes(as.Date(DateTime), OXY_oxy_0, col = 'Surf. Org.'))+
  geom_point(data = bottom_ox_org, aes(as.Date(DateTime), OXY_oxy_12, col = 'Bot. Org.'))+
  ggtitle('Oxygen concentration') +
  xlab(label = '') + ylab(label = 'Oxygen (mmol/m3)') +
  theme_minimal()+
  scale_x_date(minor_breaks = "1 month")+
  scale_color_manual(name = "Depth", values = c("Surf. Mod." = "red", "Surf. Org." = "red", "Bot. Mod." = "black", "Bot. Org." = "black"), guide = guide_legend(override.aes = list(
    linetype = c("solid", "blank", "solid", "blank"),
    shape = c(NA,16, NA, 16))))
Oxy

