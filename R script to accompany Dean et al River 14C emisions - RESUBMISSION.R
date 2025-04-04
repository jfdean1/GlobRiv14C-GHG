#############################################################################
############ Data analysis and presentation for the manuscript   ############
############ JF Dean et al., Old carbon routed from land to the  ############
############ atmosphere by global river systems                  ############
#############################################################################


##### All data files used below are included as individual .csv files in the associated Github repository #####

# packages #
library(ggplot2)
library(sf)
library(rnaturalearth)
library(ggpubr)
library(dplyr)
library(conover.test)
library(cowplot)
library(matlib)
library(simmr)
library(rjags)
library(R.matlab)
library(randomForest)


##### load global river 14C-GHG database #####

Glob_F14C_all = read.csv("Table S1.csv")

# subset by C form

Glob_F14C_DIC = (Glob_F14C_all[which(Glob_F14C_all$compound=="DIC"),])
Glob_F14C_CO2 = (Glob_F14C_all[which(Glob_F14C_all$compound=="CO2"),])
Glob_F14C_CH4 = (Glob_F14C_all[which(Glob_F14C_all$compound=="CH4"),])

F14C_DICCO2 = c(Glob_F14C_DIC$F14C_value, Glob_F14C_CO2$F14C_value)
mean(F14C_DICCO2) # 0.9225878


#### Initial database summary ####

## Fig. 1A global map of sample locations ##
# https://r-spatial.org/r/2018/10/25/ggplot2-sf.html 

world <- ne_countries(scale = "medium", returnclass = "sf")

Fig_Glob_F14C_map2 = ggplot() +
  geom_sf(data = world, color = "gray90", fill = "grey95") +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
  geom_point(data=Glob_F14C_all, aes(x = longitude, y = latitude,
                                     shape = factor(compound, level = c('DIC','CO2','CH4')),
                                     fill = factor(compound, level = c('DIC','CO2','CH4')),
                                     color = factor(compound, level = c('DIC','CO2','CH4')),
                                     size = factor(compound, level = c('DIC','CO2','CH4')))) +
  scale_fill_manual(labels = c('DIC', expression('CO'[2]), expression('CH'[4])), values = c("gray50", "sienna1", "darkslategray")) +
  scale_shape_manual(labels = c('DIC', expression('CO'[2]), expression('CH'[4])), values = c(1,0,4)) +
  scale_color_manual(labels = c('DIC', expression('CO'[2]), expression('CH'[4])), values = c("gray50", "sienna1", "darkslategray")) +
  scale_size_manual(labels = c('DIC', expression('CO'[2]), expression('CH'[4])), values = c(2,2.5,3.)) +
  theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.title=element_blank(), legend.position=c(0.07, 0.25), legend.background = element_rect(fill='transparent'))
Fig_Glob_F14C_map2


## basic summary statistics ##

# All
mean(Glob_F14C_all$F14C_value)    # 0.921
median(Glob_F14C_all$F14C_value)  # 0.952
sd(Glob_F14C_all$F14C_value)      # 0.168
n_distinct(Glob_F14C_all$DOI)     # 67 distinct studies
n_distinct(Glob_F14C_all$country) # 26 distinct countries
n_distinct(Glob_F14C_all$site_id) # 660 distinct sites
Glob_F14C_H = (Glob_F14C_all[which(Glob_F14C_all$hemisphere=="N"),]) # 1087

# DIC
mean(Glob_F14C_DIC$F14C_value)   # 0.913
median(Glob_F14C_DIC$F14C_value) # 0.947
sd(Glob_F14C_DIC$F14C_value)     # 0.179
n_distinct(Glob_F14C_DIC$DOI)    # 49 distinct studies

# CO2
mean(Glob_F14C_CO2$F14C_value)   # 0.969
median(Glob_F14C_CO2$F14C_value) # 0.982
sd(Glob_F14C_CO2$F14C_value)     # 0.079
n_distinct(Glob_F14C_CO2$DOI)    # 19 distinct studies
min(Glob_F14C_CO2$F14C_value)    # 0.79

# CH4
mean(Glob_F14C_CH4$F14C_value)   # 0.836
median(Glob_F14C_CH4$F14C_value) # 0.793
sd(Glob_F14C_CH4$F14C_value)     # 0.165
n_distinct(Glob_F14C_CH4$DOI)    # 8 distinct studies


## All data boxplot ##

## What is old?

Glob_F14C_old = (Glob_F14C_all[which(Glob_F14C_all$F14C_value<1),])      # old = 765, young = 430 (36%)
Glob_F14C_DIC_old = (Glob_F14C_DIC[which(Glob_F14C_DIC$F14C_value<1),])  # old = 631, young = 342 (35%)
Glob_F14C_CO2_old = (Glob_F14C_CO2[which(Glob_F14C_CO2$F14C_value<1),])  # old = 114, young =  83 (42%)
Glob_F14C_CH4_old = (Glob_F14C_CH4[which(Glob_F14C_CH4$F14C_value<1),])  # old =  20, young =   5 (20%)


Fig_GlobF14CAll = ggplot(Glob_F14C_all, aes(x = factor(compound, level = c('DIC','CO2','CH4')), y = F14C_value, fill = compound)) +
  geom_boxplot(outlier.shape=NA) +
  geom_jitter(aes(color = compound), width = 0.25, height = 0, shape=16, size=0.7, alpha=0.7) +
  scale_color_manual(values = c(DIC = "gray70", CO2 = "sienna1", CH4 = "darkslategray")) +
  scale_fill_manual(values=c("#FFFFFF", "#FFFFFF","#FFFFFF")) +
  scale_y_continuous(limits = c(0.0,1.4), breaks = c(0,0.2,0.4,0.6,0.8,1,1.2,1.4),
                     sec.axis = sec_axis(~ -8033*log((. * 100)/100), 
                                         name = expression(''^14*'C years'),
                                         breaks = c(0,1000,4000,10000,40000))) +
  scale_x_discrete(labels=c(expression('DIC'),
                            expression('CO'[2]),
                            expression('CH'[4]))) +
  xlab('Compound') + ylab(expression(italic('F')^14*'C')) +
  annotate("text", x=1, y=1.4, size=3, label='(35%)') +
  annotate("text", x=2, y=1.4, size=3, label='(42%)') +
  annotate("text", x=3, y=1.4, size=3, label='(20%)') +
  annotate("text", x=3.8, y = 1.22, size=3, angle = 270, label='modern') +
  coord_cartesian(xlim = c(1, 3), clip = "off") +
  geom_hline(yintercept=1, linetype="dashed", linewidth = 0.5, color = "black") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.position = "none", axis.title.x = element_blank())
Fig_GlobF14CAll



#### removing duplicates ####

##### search for duplicates #####

n_distinct(Glob_F14C_all$latitude) # 575
n_distinct(Glob_F14C_all$longitude) # 582
n_distinct(Glob_F14C_all$site_id) # 660
n_distinct(Glob_F14C_all$site_description) # 32
n_distinct(Glob_F14C_all$catchment_name) # 189

# combine latitude and longitude to look for distinct sites

latlong = as.numeric(Glob_F14C_all$latitude + Glob_F14C_all$longitude)
Glob_F14C_all$latlong = c(latlong)
n_distinct(Glob_F14C_all$latlong) # 586


# subset by C form again

Glob_F14C_DIC = (Glob_F14C_all[which(Glob_F14C_all$compound=="DIC"),])
Glob_F14C_CO2 = (Glob_F14C_all[which(Glob_F14C_all$compound=="CO2"),])
Glob_F14C_CH4 = (Glob_F14C_all[which(Glob_F14C_all$compound=="CH4"),])


# duplicates by C form

n_distinct(Glob_F14C_DIC$latlong) # 554
n_distinct(Glob_F14C_CO2$latlong) # 33
n_distinct(Glob_F14C_CH4$latlong) # 10


# create a "count" column for latlong

Glob_F14C_DIC_dup = Glob_F14C_DIC %>% group_by(latlong) %>% mutate(latlong_n = n())
Glob_F14C_CO2_dup = Glob_F14C_CO2 %>% group_by(latlong) %>% mutate(latlong_n = n())
Glob_F14C_CH4_dup = Glob_F14C_CH4 %>% group_by(latlong) %>% mutate(latlong_n = n())

write.csv(Glob_F14C_DIC_dup, file = "Glob_F14C_DIC.csv")
write.csv(Glob_F14C_CO2_dup, file = "Glob_F14C_CO2.csv")
write.csv(Glob_F14C_CH4_dup, file = "Glob_F14C_CH4.csv")

# manual duplicate removal from here



#### USING DATASET WITH DUPLICATES REMOVED ####

Glob_F14C_ND = read.csv("GlobalRiver14C_NoDup.csv")  # n = 1020 observations

# subset by C form

Glob_F14C_ND_DIC = (Glob_F14C_ND[which(Glob_F14C_ND$compound=="DIC"),])  # n = 884 observations
Glob_F14C_ND_CO2 = (Glob_F14C_ND[which(Glob_F14C_ND$compound=="CO2"),])  # n = 117 observations
Glob_F14C_ND_CH4 = (Glob_F14C_ND[which(Glob_F14C_ND$compound=="CH4"),])  # n =  19 observations


## basic summary statistics - Fraction modern ##

# All
mean(Glob_F14C_ND$F14C_value)   # 0.919
median(Glob_F14C_ND$F14C_value) # 0.953
sd(Glob_F14C_ND$F14C_value)     # 0.175
n_distinct(Glob_F14C_ND$DOI)    # 67 distinct studies


# DIC and CO2
F14C_DICCO2_ND = c(Glob_F14C_ND_DIC$F14C_value, Glob_F14C_ND_CO2$F14C_value)
mean(F14C_DICCO2_ND) # 0.919
sd(F14C_DICCO2_ND)   # 0.175
min(F14C_DICCO2_ND)  # 0.005
max(F14C_DICCO2_ND)  # 1.262

# DIC
mean(Glob_F14C_ND_DIC$F14C_value)   # 0.914
median(Glob_F14C_ND_DIC$F14C_value) # 0.950
sd(Glob_F14C_ND_DIC$F14C_value)     # 0.184
n_distinct(Glob_F14C_ND_DIC$DOI)    # 49 distinct studies

# CO2
mean(Glob_F14C_ND_CO2$F14C_value)   # 0.961
median(Glob_F14C_ND_CO2$F14C_value) # 0.965
sd(Glob_F14C_ND_CO2$F14C_value)     # 0.074
n_distinct(Glob_F14C_ND_CO2$DOI)    # 19 distinct studies

# CH4
mean(Glob_F14C_ND_CH4$F14C_value)   # 0.879
median(Glob_F14C_ND_CH4$F14C_value) # 0.900
sd(Glob_F14C_ND_CH4$F14C_value)     # 0.167
n_distinct(Glob_F14C_ND_CH4$DOI)    # 8 distinct studies


## basic summary statistics - normalised F14C ##

# All
mean(Glob_F14C_ND$norm_F14C)   # 0.873
median(Glob_F14C_ND$norm_F14C) # 0.908
sd(Glob_F14C_ND$norm_F14C)     # 0.160

# DIC
mean(Glob_F14C_ND_DIC$norm_F14C)   # 0.867
median(Glob_F14C_ND_DIC$norm_F14C) # 0.907
sd(Glob_F14C_ND_DIC$norm_F14C)     # 0.167

# CO2
mean(Glob_F14C_ND_CO2$norm_F14C)   # 0.924
median(Glob_F14C_ND_CO2$norm_F14C) # 0.926
sd(Glob_F14C_ND_CO2$norm_F14C)     # 0.072

# CH4
mean(Glob_F14C_ND_CH4$norm_F14C)   # 0.857
median(Glob_F14C_ND_CH4$norm_F14C) # 0.898
sd(Glob_F14C_ND_CH4$norm_F14C)     # 0.170


## Supplementary Fig. S5 - F14Catm histogram ##

## what is "old"? (no duplicates)

Glob_F14C_ND_old = (Glob_F14C_ND[which(Glob_F14C_ND$F14C_value<1),])              # old = 649, young = 371 (36%)
Glob_F14C_ND_DIC_old = (Glob_F14C_ND_DIC[which(Glob_F14C_ND_DIC$F14C_value<1),])  # old = 562, young = 322 (36%)
Glob_F14C_ND_CO2_old = (Glob_F14C_ND_CO2[which(Glob_F14C_ND_CO2$F14C_value<1),])  # old =  73, young =  44 (38%)
Glob_F14C_ND_CH4_old = (Glob_F14C_ND_CH4[which(Glob_F14C_ND_CH4$F14C_value<1),])  # old =  14, young =   5  (26%)


Fig_F14C_histo = ggplot(Glob_F14C_ND, aes(x = F14C_value, fill = factor(compound, level = c('DIC','CO2','CH4')),
                                          color = factor(compound, level = c('DIC','CO2','CH4')))) +
  geom_histogram(alpha = 0.7, binwidth = 0.025, position="identity") +
  scale_fill_manual(labels = c('DIC', expression('CO'[2]), expression('CH'[4])), values = c("gray70", "sienna1", "darkslategray")) +
  scale_color_manual(labels = c('DIC', expression('CO'[2]), expression('CH'[4])),values = c("gray70", "sienna1", "darkslategray")) +
  scale_x_continuous(limits = c(0,1.4), breaks = c(0,0.2,0.4,0.6,0.8,1,1.2,1.4)) +
  scale_y_continuous(limits = c(0,120), breaks = c(0,40,80,120)) + 
  xlab(expression(italic('F')^14*'C')) + ylab('Count') +
  geom_vline(xintercept = 1, linewidth = 0.5, linetype="dashed", color = "black") +
  annotate("text", x=1.3, y=90, size=3, color = "gray70", label=expression(italic('n')*' = 322 (36%)')) +
  annotate("text", x=1.2, y=90, size=3, color = "sienna1", label=expression(italic('n')*' = 44 (38%)')) +
  annotate("text", x=1.1, y=90, size=3, color = "darkslategray", label=expression(italic('n')*' = 5 (26%)')) +
  annotate("text", x=0.6, y=90, size=3, color = "gray70", label=expression('DIC '*italic('n')*' = 562')) +
  annotate("text", x=0.5, y=90, size=3, color = "sienna1", label=expression('CO'[2]*' '*italic('n')*' = 73')) +
  annotate("text", x=0.4, y=90, size=3, color = "darkslategray", label=expression('CH'[4]*' '*italic('n')*' = 14')) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  coord_flip()
Fig_F14C_histo

ggsave("Fig_S5.png", width = 8, height = 8, units = c("cm"), dpi = 600)



## Fig. 1C

# load atmospheric 14Co2 data
Atm_14CO2 = read.csv("Atm_14CO2.csv")
Atm_14CO2_sampleyears = read.csv("Atm_14CO2_sampleyears.csv")

Fig_atm_14CO2_obs = ggplot() +
  geom_point(data = Glob_F14C_all, aes(x = year, y = F14C_value,
                                       shape = factor(compound, level = c('DIC','CO2','CH4')),
                                       fill = factor(compound, level = c('DIC','CO2','CH4')),
                                       color = factor(compound, level = c('DIC','CO2','CH4')),
                                       size = factor(compound, level = c('DIC','CO2','CH4'))), alpha = 0.7) +
  scale_fill_manual(labels = c('DIC', expression('CO'[2]), expression('CH'[4])), values = c("gray70", "sienna1", "darkslategray")) +
  scale_shape_manual(labels = c('DIC', expression('CO'[2]), expression('CH'[4])), values = c(16,15,4)) +
  scale_color_manual(labels = c('DIC', expression('CO'[2]), expression('CH'[4])), values = c("gray70", "sienna1", "darkslategray")) +
  scale_size_manual(labels = c('DIC', expression('CO'[2]), expression('CH'[4])), values = c(0.8,0.8,0.8)) +
  geom_line(data = Atm_14CO2, aes(x = year, y = F14C), colour = 'black', linewidth = 0.2) + 
  geom_hline(yintercept=1, linetype = "dashed", linewidth = 0.25, color = "black") +
  scale_x_continuous(limits = c(1950,2023), breaks = c(1950,1960,1970,1980,1990,2000,2010,2020)) +
  scale_y_continuous(limits = c(0,2), breaks = c(0,0.5,1,1.5,2)) +
  xlab('Year') + ylab(expression(italic('F')^14*'C')) +
  annotate("text", x=1985, y=1.6, size=3, color = "black", label=expression('Atmospheric '^14*'CO'[2])) +
  geom_rect(aes(xmin = 1950, xmax = 1964, ymin = 0.03, ymax = 0.6), linewidth = 0.25, fill = "transparent", alpha = 0, color = "black") + 
  border() +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title=element_blank(), 
                     legend.position = c(0.12, 0.2), legend.background = element_rect(fill='transparent'))
Fig_atm_14CO2_obs


## Figure 1 combined ##

ggdraw() +
  draw_plot(Fig_Glob_F14C_map2, x = 0,     y = 0.5, width = 1,     height = 0.5) +
  draw_plot(Fig_GlobF14CAll,    x = 0,     y = 0,   width = 0.375, height = 0.5) +
  draw_plot(Fig_atm_14CO2_obs,  x = 0.375, y = 0,   width = 0.625, height = 0.5) +
      draw_plot_label(label = c("A","B","C"), size = 10,
                  x = c(0.005, 0.005, 0.38),
                  y = c(1, 0.5, 0.5))

ggsave("Fig_1.png", width = 18.5, height = 19, units = c("cm"), dpi = 600)






## Figure 2 ##

# Figure 2A

# normalised F14C by catchment size class (small v large)

l_Glob_F14C_ND = Glob_F14C_ND[which(Glob_F14C_ND$size_class=="large"),]
s_Glob_F14C_ND = Glob_F14C_ND[which(Glob_F14C_ND$size_class=="small"),]

# Small
mean(s_Glob_F14C_ND$norm_F14C)   # 0.897
median(s_Glob_F14C_ND$norm_F14C) # 0.955
sd(s_Glob_F14C_ND$norm_F14C)     # 0.179

# Large
mean(l_Glob_F14C_ND$norm_F14C)   # 0.863
median(l_Glob_F14C_ND$norm_F14C) # 0.891
sd(l_Glob_F14C_ND$norm_F14C)     # 0.150


Fig_CatchmentSize_F14Cnorm = ggplot(Glob_F14C_ND, aes(x = reorder(size_class, -norm_F14C), y = norm_F14C, fill = size_class)) +
  geom_boxplot(aes(group = size_class), outlier.shape=NA) +
  geom_jitter(aes(color = factor(compound, level = c('DIC','CO2','CH4')),
                  shape = factor(compound, level = c('DIC','CO2','CH4'))), width = 0.3, height = 0, size=0.9, alpha=0.7) +
  stat_compare_means(label.y = 1.4, size = 2.5) +     # Add global p-value
  scale_color_manual(values = c(DIC = "gray70", CO2 = "sienna1", CH4 = "darkslategray"),
                     labels = c('DIC', expression('CO'[2]), expression('CH'[4]))) +
  scale_shape_manual(values = c(16,15,4),
                     labels = c('DIC', expression('CO'[2]), expression('CH'[4]))) +
  scale_fill_manual(values=c("#FFFFFF","#FFFFFF")) +
  scale_y_continuous(limits = c(0.0,1.4), breaks = c(0,0.2,0.4,0.6,0.8,1,1.2,1.4)) +
  scale_x_discrete(labels=c(expression('<10 km'^2),expression('>10 km'^2))) +
  xlab('Catchment size') + ylab(expression(italic('F')^14*'C'['atm'])) +
  geom_hline(yintercept=0.873, linewidth = 0.25, color = "black") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")
Fig_CatchmentSize_F14Cnorm


# Figure 2B

## Biome binning

Glob_F14C_ND_TempGrassShrub = subset(Glob_F14C_ND, tbi_cl_cmj %in% c("8"))
Glob_F14C_ND_TempGrassShrub$tbi_cl_cmj
n_distinct(Glob_F14C_ND_TempGrassShrub$DOI) # 5
TempGrassShrub = rep("Temp. grass & shrub", 24)
nF14C_TempGrassShrub = Glob_F14C_ND_TempGrassShrub$norm_F14C
F14C_TempGrassShrub = Glob_F14C_ND_TempGrassShrub$F14C_value
C_TempGrassShrub = Glob_F14C_ND_TempGrassShrub$compound
id_TempGrassShrub = Glob_F14C_ND_TempGrassShrub$Iso_ID
mean(Glob_F14C_ND_TempGrassShrub$norm_F14C)    # 0.949
median(Glob_F14C_ND_TempGrassShrub$norm_F14C)  # 1.005
sd(Glob_F14C_ND_TempGrassShrub$norm_F14C)      # 0.127


Glob_F14C_ND_TropGrassShrub = subset(Glob_F14C_ND, tbi_cl_cmj %in% c("7","9")) # includes "flooded grassland" as mostly in tropical regions: https://ecoregions.appspot.com/
Glob_F14C_ND_TropGrassShrub$tbi_cl_cmj
n_distinct(Glob_F14C_ND_TropGrassShrub$DOI) # 6
TropGrassShrub = rep("Trop. grass & shrub", 45)
nF14C_TropGrassShrub = Glob_F14C_ND_TropGrassShrub$norm_F14C
F14C_TropGrassShrub = Glob_F14C_ND_TropGrassShrub$F14C_value
C_TropGrassShrub = Glob_F14C_ND_TropGrassShrub$compound
id_TropGrassShrub = Glob_F14C_ND_TropGrassShrub$Iso_ID
mean(Glob_F14C_ND_TropGrassShrub$norm_F14C)    # 0.745
median(Glob_F14C_ND_TropGrassShrub$norm_F14C)  # 0.836
sd(Glob_F14C_ND_TropGrassShrub$norm_F14C)      # 0.287


Glob_F14C_ND_TempConiferBorealFor = subset(Glob_F14C_ND, tbi_cl_cmj %in% c("5","6"))
Glob_F14C_ND_TempConiferBorealFor$tbi_cl_cmj
n_distinct(Glob_F14C_ND_TempConiferBorealFor$DOI) # 11
TempConiferBorealFor = rep("Temp. conifer & boreal for.", 259)
nF14C_TempConiferBorealFor = Glob_F14C_ND_TempConiferBorealFor$norm_F14C
F14C_TempConiferBorealFor = Glob_F14C_ND_TempConiferBorealFor$F14C_value
C_TempConiferBorealFor = Glob_F14C_ND_TempConiferBorealFor$compound
id_TempConiferBorealFor = Glob_F14C_ND_TempConiferBorealFor$Iso_ID
mean(Glob_F14C_ND_TempConiferBorealFor$norm_F14C)    # 0.908
median(Glob_F14C_ND_TempConiferBorealFor$norm_F14C)  # 0.963
sd(Glob_F14C_ND_TempConiferBorealFor$norm_F14C)      # 0.142


Glob_F14C_ND_TropBroadleafConiferFor = subset(Glob_F14C_ND, tbi_cl_cmj %in% c("1","2","3"))
Glob_F14C_ND_TropBroadleafConiferFor$tbi_cl_cmj
n_distinct(Glob_F14C_ND_TropBroadleafConiferFor$DOI) # 13
TropBroadleafConiferFor = rep("Trop. broadleaf & conifer for.", 218)
nF14C_TropBroadleafConiferFor = Glob_F14C_ND_TropBroadleafConiferFor$norm_F14C
F14C_TropBroadleafConiferFor = Glob_F14C_ND_TropBroadleafConiferFor$F14C_value
C_TropBroadleafConiferFor = Glob_F14C_ND_TropBroadleafConiferFor$compound
id_TropBroadleafConiferFor = Glob_F14C_ND_TropBroadleafConiferFor$Iso_ID
mean(Glob_F14C_ND_TropBroadleafConiferFor$norm_F14C)    # 0.863
median(Glob_F14C_ND_TropBroadleafConiferFor$norm_F14C)  # 0.917
sd(Glob_F14C_ND_TropBroadleafConiferFor$norm_F14C)      # 0.174


Glob_F14C_ND_TempBroadMixFor = subset(Glob_F14C_ND, tbi_cl_cmj %in% c("4","12"))
Glob_F14C_ND_TempBroadMixFor$tbi_cl_cmj
n_distinct(Glob_F14C_ND_TempBroadMixFor$DOI) # 26
TempBroadMixFor = rep("Temp. broadleaf & mixed for.", 312)
nF14C_TempBroadMixFor = Glob_F14C_ND_TempBroadMixFor$norm_F14C
F14C_TempBroadMixFor = Glob_F14C_ND_TempBroadMixFor$F14C_value
C_TempBroadMixFor = Glob_F14C_ND_TempBroadMixFor$compound
id_TempBroadMixFor = Glob_F14C_ND_TempBroadMixFor$Iso_ID
mean(Glob_F14C_ND_TempBroadMixFor$norm_F14C)    # 0.878
median(Glob_F14C_ND_TempBroadMixFor$norm_F14C)  # 0.886
sd(Glob_F14C_ND_TempBroadMixFor$norm_F14C)      # 0.113


Glob_F14C_ND_Tundra = subset(Glob_F14C_ND, tbi_cl_cmj %in% c("11"))
Glob_F14C_ND_Tundra$tbi_cl_cmj
n_distinct(Glob_F14C_ND_Tundra$DOI) # 5
Tundra = rep("Tundra", 69)
nF14C_Tundra = Glob_F14C_ND_Tundra$norm_F14C
F14C_Tundra = Glob_F14C_ND_Tundra$F14C_value
C_Tundra = Glob_F14C_ND_Tundra$compound
id_Tundra = Glob_F14C_ND_Tundra$Iso_ID
mean(Glob_F14C_ND_Tundra$norm_F14C)    # 0.929
median(Glob_F14C_ND_Tundra$norm_F14C)  # 0.971
sd(Glob_F14C_ND_Tundra$norm_F14C)      # 0.121


Glob_F14C_ND_Montane = subset(Glob_F14C_ND, tbi_cl_cmj %in% c("10"))
Glob_F14C_ND_Montane$tbi_cl_cmj
n_distinct(Glob_F14C_ND_Montane$DOI) # 6
Montane = rep("Montane grass & shrub", 56)
nF14C_Montane = Glob_F14C_ND_Montane$norm_F14C
F14C_Montane = Glob_F14C_ND_Montane$F14C_value
C_Montane = Glob_F14C_ND_Montane$compound
id_Montane = Glob_F14C_ND_Montane$Iso_ID
mean(Glob_F14C_ND_Montane$norm_F14C)    # 0.703
median(Glob_F14C_ND_Montane$norm_F14C)  # 0.735
sd(Glob_F14C_ND_Montane$norm_F14C)      # 0.144


Glob_F14C_ND_Desert = subset(Glob_F14C_ND, tbi_cl_cmj %in% c("13","15"))
Glob_F14C_ND_Desert$tbi_cl_cmj
n_distinct(Glob_F14C_ND_Desert$DOI) # 4
Desert = rep("Deserts", 37)
nF14C_Desert = Glob_F14C_ND_Desert$norm_F14C
F14C_Desert = Glob_F14C_ND_Desert$F14C_value
C_Desert = Glob_F14C_ND_Desert$compound
id_Desert = Glob_F14C_ND_Desert$Iso_ID
mean(Glob_F14C_ND_Desert$norm_F14C)    # 0.911
median(Glob_F14C_ND_Desert$norm_F14C)  # 0.953
sd(Glob_F14C_ND_Desert$norm_F14C)      # 0.169


biome_bin = as.vector(c(TempGrassShrub, TropGrassShrub, TempConiferBorealFor, TropBroadleafConiferFor,
                        TempBroadMixFor, Tundra, Montane, Desert))

nF14C_biome_bin = as.vector(c(nF14C_TempGrassShrub, nF14C_TropGrassShrub, nF14C_TempConiferBorealFor, nF14C_TropBroadleafConiferFor,
                              nF14C_TempBroadMixFor, nF14C_Tundra, nF14C_Montane, nF14C_Desert))

F14C_biome_bin = as.vector(c(F14C_TempGrassShrub, F14C_TropGrassShrub, F14C_TempConiferBorealFor, F14C_TropBroadleafConiferFor,
                             F14C_TempBroadMixFor, F14C_Tundra, F14C_Montane, F14C_Desert))

C_biome_bin = as.vector(c(C_TempGrassShrub, C_TropGrassShrub, C_TempConiferBorealFor, C_TropBroadleafConiferFor,
                          C_TempBroadMixFor, C_Tundra, C_Montane, C_Desert))

id_biome_bin = as.vector(c(id_TempGrassShrub, id_TropGrassShrub, id_TempConiferBorealFor, id_TropBroadleafConiferFor,
                           id_TempBroadMixFor, id_Tundra, id_Montane, id_Desert))


Glob_F14C_biome_bin = data.frame(id_biome_bin, F14C_biome_bin, nF14C_biome_bin, C_biome_bin, biome_bin)
Glob_F14C_biome_bin_names <- c("Iso_ID", "F14C_value","norm_F14C", "compound", "biome_bin")
names(Glob_F14C_biome_bin) <- Glob_F14C_biome_bin_names


#### 

# test for differences between groups

kruskal.test(norm_F14C ~ biome_bin, data = Glob_F14C_biome_bin) # p < 2.2e-16
conover.test(Glob_F14C_biome_bin$norm_F14C, Glob_F14C_biome_bin$biome_bin, altp=T)

# build data frame of values and sig-letts for plot
F14CbyBiome.SigLett = c('a','a','a','ab','b','c','d','e')
F14CbyBiome.MaxValue.df = data.frame(F14CbyBiome.SigLett,
                                     c(1.29,1.29,1.29,1.27,1.25,1.21,1.18,1.1),
                                     c("Temp. grass & shrub",
                                       "Tundra",
                                       "Temp. conifer & boreal for.",
                                       "Deserts",
                                       "Trop. broadleaf & conifer for.",
                                       "Temp. broadleaf & mixed for.",
                                       "Trop. grass & shrub",
                                       "Montane grass & shrub"))
F14CbyBiome.MaxValue.df_names <- c("SigLett", "MaxValue", "biome_bin")
names(F14CbyBiome.MaxValue.df) <- F14CbyBiome.MaxValue.df_names


Fig_GlobF14C_biome_bin = ggplot(Glob_F14C_biome_bin, aes(x = reorder(biome_bin, -norm_F14C, FUN = median), y = norm_F14C), fill = biome_bin) +
  geom_boxplot(aes(group = biome_bin), outlier.shape=NA) +
  geom_jitter(aes(color = factor(compound, level = c('DIC','CO2','CH4')),
                  shape = factor(compound, level = c('DIC','CO2','CH4'))), width = 0.3, height = 0, size=0.9, alpha=0.7) +
  stat_compare_means(label.x = 1.5, label.y = 1.4, size = 2.5) +   # Add global p-value
  scale_color_manual(values = c(DIC = "gray70", CO2 = "sienna1", CH4 = "darkslategray"),
                     labels = c('DIC', expression('CO'[2]), expression('CH'[4]))) +
  scale_shape_manual(values = c(16,15,4),
                     labels = c('DIC', expression('CO'[2]), expression('CH'[4]))) +
  scale_fill_manual(values=c("#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF")) +
  scale_y_continuous(limits = c(0.0,1.4), breaks = c(0,0.2,0.4,0.6,0.8,1,1.2,1.4),
                     sec.axis = sec_axis(~ -8033*log((. * 100)/100), 
                                         name = expression(''^14*'C years'),
                                         breaks = c(0,1000,4000,10000,40000))) +
  annotate("text", x=8.8, y = 1.22, size=3, angle = 270, label='modern') +
  coord_cartesian(xlim = c(1, 8), clip = "off") +
  xlab('Biome') + ylab(expression(italic('F')^14*'C'['atm'])) +
  geom_hline(yintercept=0.873, linewidth = 0.25, color = "black") +
  geom_text(data = F14CbyBiome.MaxValue.df,
            aes(x = biome_bin, y = MaxValue,  label = SigLett), vjust=0 , size = 3) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8),
                     legend.title=element_blank(), legend.position=c(0.1, 0.2),
                     legend.background = element_rect(fill='transparent')) +
  geom_rect(aes(xmin = 0.5, xmax = 2.1, ymin = 0, ymax = 0.42), linewidth = 0.25, fill = "transparent", alpha = 0, color = "black") + 
  guides(fill = "none", colour = guide_legend(override.aes = list(alpha = 1)))
Fig_GlobF14C_biome_bin


# Figure 2C

## Lithology binning

# removing Antarctic values because no lithology data available
Glob_F14C_ND_noANT = Glob_F14C_ND[-c(207:220,445),]

Glob_F14C_ND_Sed = subset(Glob_F14C_ND_noANT, lit_cl_cmj %in% c("1","3","5","6"))
Glob_F14C_ND_Sed$lit_cl_cmj
n_distinct(Glob_F14C_ND_Sed$DOI) # 51
sed = rep("Sed.", 612)
nF14C_sed = Glob_F14C_ND_Sed$norm_F14C
F14C_sed = Glob_F14C_ND_Sed$F14C_value
C_sed = Glob_F14C_ND_Sed$compound
id_sed = Glob_F14C_ND_Sed$Iso_ID
mean(Glob_F14C_ND_Sed$norm_F14C)    # 0.848
median(Glob_F14C_ND_Sed$norm_F14C)  # 0.873
sd(Glob_F14C_ND_Sed$norm_F14C)      # 0.159

Glob_F14C_ND_Meta = subset(Glob_F14C_ND_noANT, lit_cl_cmj %in% "8")
Glob_F14C_ND_Meta$lit_cl_cmj
n_distinct(Glob_F14C_ND_Meta$DOI) # 17
met = rep("Met.", 73)
nF14C_met = Glob_F14C_ND_Meta$norm_F14C
F14C_met = Glob_F14C_ND_Meta$F14C_value
C_met = Glob_F14C_ND_Meta$compound
id_met = Glob_F14C_ND_Meta$Iso_ID
mean(Glob_F14C_ND_Meta$norm_F14C)    # 0.957
median(Glob_F14C_ND_Meta$norm_F14C)  # 0.983
sd(Glob_F14C_ND_Meta$norm_F14C)      # 0.092

Glob_F14C_ND_Ig = subset(Glob_F14C_ND_noANT, lit_cl_cmj %in% c("2","4","7","9","10","12","13"))
Glob_F14C_ND_Ig$lit_cl_cmj
n_distinct(Glob_F14C_ND_Ig$DOI) # 17
ign = rep("Ign.", 319)
nF14C_ign = Glob_F14C_ND_Ig$norm_F14C
F14C_ign = Glob_F14C_ND_Ig$F14C_value
C_ign = Glob_F14C_ND_Ig$compound
id_ign = Glob_F14C_ND_Ig$Iso_ID
mean(Glob_F14C_ND_Ig$norm_F14C)    # 0.903
median(Glob_F14C_ND_Ig$norm_F14C)  # 0.959
sd(Glob_F14C_ND_Ig$norm_F14C)      # 0.156

lith_bin = as.vector(c(sed, met, ign))
nF14C_lith_bin = as.vector(c(nF14C_sed, nF14C_met, nF14C_ign))
F14C_lith_bin = as.vector(c(F14C_sed, F14C_met, F14C_ign))
C_lith_bin = as.vector(c(C_sed, C_met, C_ign))

Glob_F14C_lith_bin = data.frame(F14C_lith_bin,nF14C_lith_bin, C_lith_bin, lith_bin)
Glob_F14C_lith_bin_names <- c("F14C_value","norm_F14C", "compound", "lith_bin")
names(Glob_F14C_lith_bin) <- Glob_F14C_lith_bin_names

lith_bin_comparisons <- list( c("Met.", "Ign."), c("Ign.", "Sed."), c("Met.", "Sed.") )


Fig_GlobF14C_lith_bin = ggplot(Glob_F14C_lith_bin, aes(x = reorder(lith_bin, -norm_F14C), y = norm_F14C, fill = lith_bin)) +
  geom_boxplot(aes(group = lith_bin), outlier.shape=NA) +
  geom_jitter(aes(color = factor(compound, level = c('DIC','CO2','CH4')),
                  shape = factor(compound, level = c('DIC','CO2','CH4'))), width = 0.3, height = 0, size=0.9, alpha=0.7) +
  stat_compare_means(comparisons = lith_bin_comparisons, label.y = c(1.2, 1.125, 1.275), size = 2.5) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 1.41, size = 2.5) +     # Add global p-value
  scale_color_manual(values = c(DIC = "gray70", CO2 = "sienna1", CH4 = "darkslategray"),
                     labels = c('DIC', expression('CO'[2]), expression('CH'[4]))) +
  scale_shape_manual(values = c(16,15,4),
                     labels = c('DIC', expression('CO'[2]), expression('CH'[4]))) +
  scale_fill_manual(values=c("#FFFFFF","#FFFFFF","#FFFFFF")) +
  scale_y_continuous(limits = c(0.0,1.41), breaks = c(0,0.2,0.4,0.6,0.8,1,1.2,1.4)) +
  scale_x_discrete(labels=c('Metamorphic','Igneous','Sedimentary')) +
  xlab('Lithology') + ylab(expression(italic('F')^14*'C'['atm'])) +
  geom_hline(yintercept=0.864, linewidth = 0.25, color = "black") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  guides(fill = "none", colour = guide_legend(override.aes = list(alpha = 1)))
Fig_GlobF14C_lith_bin


# combine panels

ggdraw() +
  draw_plot(Fig_CatchmentSize_F14Cnorm, x = 0,     y = 0.55, width = 0.375, height = 0.45) +
  draw_plot(Fig_GlobF14C_lith_bin,      x = 0.375, y = 0.55, width = 0.5,   height = 0.45) +
  draw_plot(Fig_GlobF14C_biome_bin,     x = 0,     y = 0,    width = 1,     height = 0.55) +
  draw_plot_label(label = c("A","B","C"), size = 10,
                  x = c(0.005, 0.38, 0.005),
                  y = c(1, 1, 0.55))

ggsave("Fig_2.png", width = 14, height = 20, units = c("cm"), dpi = 600)






########################################
#### Extended Data Figures #############
########################################


#### Extended Data Figure 1 ####
## F14Catm trend through time

F14Cnorm_ND_time.lm = lm(Glob_F14C_ND$norm_F14C ~ Glob_F14C_ND$year)
summary(F14Cnorm_ND_time.lm)$r.squared # R^2 = 0.04
summary(F14Cnorm_ND_time.lm)$coefficients[,4] # p-value = 2.756958e-10 ***
summary(F14Cnorm_ND_time.lm)$coefficients[] # y = -0.00436196x + 9.63680120

F14Cnorm_ND_time_DIC.lm = lm(Glob_F14C_ND_DIC$norm_F14C ~ Glob_F14C_ND_DIC$year)
summary(F14Cnorm_ND_time_DIC.lm)$r.squared # R^2 = 0.06
summary(F14Cnorm_ND_time_DIC.lm)$coefficients[,4] # p-value = 2.061658e-12***
summary(F14Cnorm_ND_time_DIC.lm)$coefficients[] # y = -0.005518821x + 11.421122166  

F14Cnorm_ND_time_CO2.lm = lm(Glob_F14C_ND_CO2$norm_F14C ~ Glob_F14C_ND_CO2$year)
summary(F14Cnorm_ND_time_CO2.lm)$r.squared # R^2 = 0.03017762
summary(F14Cnorm_ND_time_CO2.lm)$coefficients[,4] # p-value = 0.06105029
summary(F14Cnorm_ND_time_CO2.lm)$coefficients[] # y = 0.002620287x - 4.346210013  


Fig_F14Cnorm_time = ggplot(Glob_F14C_ND, aes(x = year, y = norm_F14C,
                                             shape = factor(compound, level = c('DIC','CO2','CH4')),
                                             color = factor(compound, level = c('DIC','CO2','CH4')))) +
  geom_point(size = 0.75, alpha=0.7) +
  scale_color_manual(values = c(DIC = "gray70", CO2 = "sienna1", CH4 = "darkslategray"),
                     labels = c('DIC', expression('CO'[2]), expression('CH'[4]))) +
  scale_shape_manual(values = c(16,15,4),
                     labels = c('DIC', expression('CO'[2]), expression('CH'[4]))) +
  scale_y_continuous(limits = c(0.0,1.4), breaks = c(0,0.2,0.4,0.6,0.8,1,1.2,1.4)) +
  xlim(c(1990,2023)) +
  xlab(expression('Year sampled')) + ylab(expression(italic('F')^14*'C'['atm'])) +
  geom_smooth(method=lm, level = 0.95, linewidth = 0.2, linetype="dashed", aes(group=FALSE), col = "black", fill = "gray70") +
  annotate("text", x=1997, y=1.4, size=3, color = "black", label=expression('R'^2*' = 0.04, p << 0.001')) +
  geom_hline(yintercept=1, linetype="dashed", linewidth = 0.25, color = "black") +
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0, 1), fill=NA))) +
  
  #geom_rect(aes(xmin = 1990, xmax = 1995, ymin = 0.05, ymax = 0.41), linewidth = 0.25, fill = "transparent", alpha = 0, color = "black") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title=element_blank(), 
                     legend.position = "right", legend.background = element_rect(fill='transparent'))
Fig_F14Cnorm_time

# x-axis density plot

xplot2 = ggdensity(Glob_F14C_ND, "year", color = 'steelblue1', fill = 'steelblue1', size = 0.25) +
  scale_x_continuous(limits = c(1990,2023))
xplot2 = xplot2 + clean_theme()
xplot2


## Combine plots ##

ggdraw() +
  draw_plot(xplot2,            x = 0.11, y = 0.87, width = 0.71, height = 0.13) +
  draw_plot(Fig_F14Cnorm_time, x = 0,    y = 0,   width = 1,   height = 0.9)

ggsave("Fig_E1.png", width = 12, height = 10, units = c("cm"), dpi = 600)






#### Extended Data Figure 2 ####
# catchment size class multi

kruskal.test(norm_F14C ~ size_class_multi, data = Glob_F14C_ND) # p = 5.923e-16***
conover.test(Glob_F14C_ND$norm_F14C, Glob_F14C_ND$size_class_multi, altp=T)

Fig_CatchmentSize_F14Cnorm_multi.SigLett = c('a','ab','c','b','d','d')
Fig_CatchmentSize_F14Cnorm_multi.df = data.frame(Fig_CatchmentSize_F14Cnorm_multi.SigLett,
                                                 c(1.25,1.24,1.22,1.23,1.17,1.17),
                                                 c("10", "100", "1000", "10000", "100000", "1000000"))
Fig_CatchmentSize_F14Cnorm_multi.df_names <- c("SigLett", "MaxValue", "size_bin")
names(Fig_CatchmentSize_F14Cnorm_multi.df) <- Fig_CatchmentSize_F14Cnorm_multi.df_names

Fig_CatchmentSize_F14Cnorm_multi = ggplot(Glob_F14C_ND, aes(x = factor(size_class_multi), y = norm_F14C), fill = size_class_multi) +
  geom_boxplot(aes(group = size_class_multi), outlier.shape=NA) +
  geom_jitter(aes(color = factor(compound, level = c('DIC','CO2','CH4')),
                  shape = factor(compound, level = c('DIC','CO2','CH4'))), width = 0.3, height = 0, size=0.9, alpha=0.7) +
  stat_compare_means(label.y = 1.4, size = 2.5) +     # Add global p-value
  scale_color_manual(values = c(DIC = "gray70", CO2 = "sienna1", CH4 = "darkslategray"),
                     labels = c('DIC', expression('CO'[2]), expression('CH'[4]))) +
  scale_shape_manual(values = c(16,15,4),
                     labels = c('DIC', expression('CO'[2]), expression('CH'[4]))) +
  scale_fill_manual(values=c("#FFFFFF","#FFFFFF")) +
  scale_y_continuous(limits = c(0.0,1.4), breaks = c(0,0.2,0.4,0.6,0.8,1,1.2,1.4)) +
  scale_x_discrete(labels = c(expression('10'),'100','1000','10000','100000','1000000')) +
  xlab(expression('Indicative catchment size class (km'^2*')')) + ylab(expression(italic('F')^14*'C'['atm'])) +
  geom_hline(yintercept=0.873, linewidth = 0.25, color = "black") +
  geom_text(data = Fig_CatchmentSize_F14Cnorm_multi.df,
            aes(x = size_bin, y = MaxValue,  label = SigLett), vjust=0 , size = 3) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title=element_blank()) +
  guides(fill = "none", colour = guide_legend(override.aes = list(alpha = 1)))
Fig_CatchmentSize_F14Cnorm_multi

ggsave("Fig_E2.png", width = 14, height = 10, units = c("cm"), dpi = 600)






#### Extended Data Figure 3-4 ####
## Random Forest relationships

# Random forest model code
# 10-fold cross validation 
# multi model example

# var<-"norm_F14C"
Agn<-c(1:1)
final_table = matrix(0, nrow =3, ncol=3)

for (gn in Agn) {
  Araw<-readMat(paste("RFdata",gn,".mat",sep=""))
  data<-as.data.frame(Araw$large)
  
  
  Im_table = matrix(0, nrow = ncol(data)-1, ncol=10)
  
  Vname<-read.table(paste("XvariablesFileName.txt",sep=""),head=FALSE,sep = ",")
  Vname[]<-lapply(Vname,as.character)
  colnames(data)<-c(Vname)
  
  
  names(data) = tolower(names(data))
  colnames(data) <- make.names(colnames(data))
  
  
  pred <- data.frame() #
  library(plyr)  
  library(randomForest) 
  m <- 500
  
  
  CVgroup <- function(k, datasize, seed) {  
    cvlist <- list()  
    set.seed(seed)  
    n <- rep(1:k, ceiling(datasize/k))[1:datasize] #
    temp <- sample(n, datasize)  # 
    x <- 1:k  
    dataseq <- 1:datasize   
    cvlist <- lapply(x, function(x) dataseq[temp==x])  #
    return(cvlist)  
  }  
  
  
  ##
  k<-10
  datasize <- nrow(data)  
  cvlist <- CVgroup(k = k, datasize = datasize, seed = 1206)
  
  if (sum(is.na(data))>=1)
  {
    data.imputed<-rfImpute(erc ~ ., data,iter=5,ntree=500)
    Fdata<-data.imputed
  }
  else
  {Fdata<-data}
  
  
  for (j in m) {                                    #
    progress.bar <- create_progress_bar("text")     #
    progress.bar$init(k)                            #
    
    for (i in 1:k) {  
      train <- Fdata[-cvlist[[i]],]                     #  
      test <- Fdata[cvlist[[i]],] 
      
      
      model <- randomForest(erc ~ ., data = train, ntree = j,importance=TRUE)   #  
      importance<-importance(model,type=1) 
      print(importance)
      Im_table[,i] = importance[,1];
      prediction <- predict(model, subset(test, select = - erc))#  
      
      randomtree <- rep(j, length(prediction))          # 
      
      kcross <- rep(i, length(prediction))              #  
      
      temp <- data.frame(cbind(subset(test, select = erc), prediction, randomtree, kcross))  
      #
      pred <- rbind(pred, temp) #
      
    }
  }
  
  maefun <- function(pred, obs) mean(abs(pred - obs))  
  msefun <- function(pred, obs) mean((pred - obs)^2)  
  nmsefun <- function(pred, obs) 1-mean((pred - obs)^2)/mean((mean(obs) - obs)^2)  
  
  library(dplyr)  
  eval <- pred %>% group_by(randomtree, kcross) %>%    
    summarise(mae = maefun(prediction, erc),  
              mse = msefun(prediction, erc),  
              nmse = nmsefun(prediction, erc))
  
  R2<-mean(eval$nmse)
  print(R2)
  final_table[gn,1] = gn
  final_table[gn,2] = R2
  write.csv(Im_table,paste("OutputPath/OIncMSE",gn,".csv", sep = ""),row.names=T,quote=F)
  
  rm(Araw)
  rm(data)
  rm(model)
  
}


write.csv(final_table,paste("OutputPath/OutputR2",gn,".csv", sep = ""),row.names=T,quote=F)

# Fig. E3 bar plot

clc; clear; close all;

% Add color scheme function
addpath('.\BrewerMap-master');

% Load data
CDname = importdata('.\VarCDname_SmallCatchment.txt');
Vname = {'norm F14C'};
IncMSE_data = importdata('IncMSE_SmallCatchment.csv');
Alldata = importdata('RFinputdata_SmallCatchment.mat');

% Calculate mean IncMSE
AIncMSE = mean(IncMSE_data.data(:, 2:end), 2)';

% Spearman correlation
Sigvar = Alldata(:, 1);
Catvar = Alldata(:, 2:end);
[AARp, AARppv] = arrayfun(@(col) ...
    corrcoef_with_nans(Sigvar, Catvar(:, col), 'Spearman'), ...
    1:size(Catvar, 2));

% Classification for correlation
CAARp = arrayfun(@(val) classify_correlation(val), AARp);


% Generate color map
cmap = brewermap(6, 'RdBu');
colors = cmap(7 - CAARp, :);

% Combine and sort data
AAdataSum = [AIncMSE', colors, AARp', AARppv'];
[AAdataSort, ff] = sortrows(AAdataSum, 1, 'descend');
ACDname = CDname(ff);

% Plot horizontal bar chart
figure('Renderer', 'painters', 'Position', [10 10 420 1000]);
b = barh(flip(AAdataSort(:, 1)));
b.FaceColor = 'flat';
b.CData = flip(AAdataSort(:, 2:4));

% Configure axes and add significant markers
set(gca, 'YTick', 1:length(AIncMSE), 'YTickLabel', flip(ACDname), 'FontSize', 13);
title({'F^{14}C_{atm}', 'Small catchments (≤10 km²)'});
xlabel('IncMSE');
hold on;

% Add Spearman coefficient annotations
spearman_values = flip(AAdataSort(:, 5)); % Spearman coefficients
x_offset = 4.2; % Offset for text placement (adjust as needed)
for i = 1:length(spearman_values)
text(x_offset, i, sprintf('%.2f', spearman_values(i)), ...
     'HorizontalAlignment', 'right', ... % Align to the right of x_offset
     'FontSize', 10, ...
     'Color', 'k');
end

% Add markers for significant p-values
significant_idx = find(AAdataSort(:, 6) < 0.05);
scatter(4.8, size(CDname,1)+1 - significant_idx, '*k');

print('-r1200','-djpeg',['SmallCatchmentBarplot.jpg'])

% Helper functions
function [r, p] = corrcoef_with_nans(x, y, method)
valid_idx = ~isnan(x) & ~isnan(y);
[r, p] = corr(x(valid_idx), y(valid_idx), 'type', method);
end

function class = classify_correlation(value)
if value >= 0.5
class = 1;
elseif value >= 0.25
class = 2;
elseif value >= 0
class = 3;
elseif value >= -0.25
class = 4;
elseif value >= -0.5
class = 5;
else
  class = 6;
end
end

# Fig. E4 barplot

clc; clear; close all;

% Add color scheme function
addpath('.\BrewerMap-master');

% Load data
CDname = importdata('.\VarCDname_LargeCatchment.txt');
Vname = {'norm F14C'};
IncMSE_data = importdata('IncMSE_LargeCatchment.csv');
Alldata = importdata('RFinputdata_LargeCatchment.mat');

% Calculate mean IncMSE
AIncMSE = mean(IncMSE_data.data(:, 2:end), 2)';

% Spearman correlation
Sigvar = Alldata(:, 1);
Catvar = Alldata(:, 2:end);
[AARp, AARppv] = arrayfun(@(col) ...
    corrcoef_with_nans(Sigvar, Catvar(:, col), 'Spearman'), ...
    1:size(Catvar, 2));

% Classification for correlation
CAARp = arrayfun(@(val) classify_correlation(val), AARp);


% Generate color map
cmap = brewermap(6, 'RdBu');
colors = cmap(7 - CAARp, :);

% Combine and sort data
AAdataSum = [AIncMSE', colors, AARp', AARppv'];
[AAdataSort, ff] = sortrows(AAdataSum, 1, 'descend');
ACDname = CDname(ff);

% Plot horizontal bar chart
figure('Renderer', 'painters', 'Position', [10 10 420 1000]);
b = barh(flip(AAdataSort(:, 1)));
b.FaceColor = 'flat';
b.CData = flip(AAdataSort(:, 2:4));

% Configure axes and add significant markers
set(gca, 'YTick', 1:length(AIncMSE), 'YTickLabel', flip(ACDname), 'FontSize', 13);
title({'F^{14}C_{atm}', 'Large catchments (>10 km²)'});
xlabel('IncMSE');
xlim([0,32])
hold on;

% Add Spearman coefficient annotations
spearman_values = flip(AAdataSort(:, 5)); % Spearman coefficients
x_offset = 7; % Offset for text placement (adjust as needed)
for i = 1:length(spearman_values)
text(x_offset, i, sprintf('%.2f', spearman_values(i)), ...
     'HorizontalAlignment', 'right', ... % Align to the right of x_offset
     'FontSize', 10, ...
     'Color', 'k');
end

% Add markers for significant p-values
significant_idx = find(AAdataSort(:, 6) < 0.05);
scatter(8, size(CDname,1)+1 - significant_idx, '*k');


print('-r1200','-djpeg',['LargeCatchmentBarplot.jpg'])

% Helper functions
function [r, p] = corrcoef_with_nans(x, y, method)
valid_idx = ~isnan(x) & ~isnan(y);
[r, p] = corr(x(valid_idx), y(valid_idx), 'type', method);
end

function class = classify_correlation(value)
if value >= 0.5
class = 1;
elseif value >= 0.25
class = 2;
elseif value >= 0
class = 3;
elseif value >= -0.25
class = 4;
elseif value >= -0.5
class = 5;
else
  class = 6;
end
end


## pdp analyses created using separate matlab code included in github repository ##






#### Extended Data Figure 5 ####
##
## Monte Carlo simulation of residual F14C after accounting for fossil flux

# river CO2 emissions = 2.0 Pg C y-1 (asssumed from Liu et al 2022 PNAS)
# DIC export to the ocean = 0.5 Pg C y-1 (assumed from Liu et al 2024 Nat Geo)
# minimum fossil inputs from carbonate weathering = 0.15 Pg C y-1 (assumed from Gaillardet et al.2019 Chem Geo)
# maximum additional fossil inputs from rock organic C oxidation = 0.068 Pg C y-1 (assumed from Zondervan et al 2023 Nature)
# Therefore:
# total DIC export through rivers = 2.5 Pg C y-1
# total contribution from weathering = 0.15 - 0.218 Pg C y-1



# generate range of fossil flux contributions
Fossil = runif(10000, min = 0.15, max = 0.218)

# then: res_flux = 2.5 Pg C y-1 - "Fossil"
res_flux = 2.5 - Fossil

# remaining flux to account for
min(res_flux) # 2.28
max(res_flux) # 2.35

# then: res_proportion = "res_flux" / 2.5 Pg C y-1
res_proportion = res_flux/2.5

# then: Theta = mean F14C_value (ND) / "emission_proportion"
theta = 0.919/res_proportion

# F14C signature of remaining flux using F14C = 0.919 (mean of all data, ND)
min(theta) # 0.978
max(theta) # 1.007

## note source F14C values come from:

# bomb peak = mean 14CO2 1955-2019 from Hua et al. 2022, Radiocarbon https://doi.org/10.1017/RDC.2021.95
# 1.011 - 1.442 (1.2263 +/- 0.2155 Fm, 1950-2019)

# 0-30cm mean soil age and sd = carbon-weighted mean age of mineral soil carbon from Shi et al. 2020, Nature Geoscience https://doi.org/10.1038/s41561-020-0596-z 
# 0.808 - 0.874 (1390 +/- 310 yBP = 0.841 +/- 0.033 Fm)

Bomb = runif(10000, min = 1.011, max = 1.442)
Soil = runif(10000, min = 0.808, max = 0.874)

hist(Bomb)
hist(Soil)
hist(theta)

MC_inputs = data.frame(Bomb, Soil, theta)


# Solve equation for each row in MC_inputs table

# create empty list for solutions
solutions <- data.frame()

# Loop through each row of the data frame

for (i in 1:nrow(MC_inputs)) {
  # Extract values from the current row
  Bo <- MC_inputs$Bomb[i]
  So <- MC_inputs$Soil[i]
  th <- MC_inputs$theta[i]
  
  # Construct the coefficient matrix and constant vector
  e1 <- matrix(c(Bo, So, 1, 1), nrow = 2, byrow = TRUE)
  e2 <- c(th, 1)
  
  # Solve the simultaneous equations
  solution <- solve(e1, e2)
  
  # Store the solution in the list
  solutions <- rbind(solutions, t(solution))
}

# Name the columns of the solutions data frame
colnames(solutions) <- c("P_bomb", "P_soil")

# plot solutions against each other
Fig_BombVSoil = ggplot(data = solutions, aes(x = P_bomb, y = P_soil)) + 
  geom_point(color="black", shape = 16, size = 1, alpha = 0.5)
Fig_BombVSoil

output = data.frame(MC_inputs$Bomb, MC_inputs$Soil, MC_inputs$theta, solutions$P_bomb, solutions$P_soil)
colnames(output) = c("Bomb_input","Soil_input","theta_input","P_bomb","P_soil")

# remove implausible values (negative or >1)
output_filtered <- output[output$P_bomb < 1, ] # removed NO values


## plots 

# Decadal proportion

mean(output_filtered$P_bomb) # 0.44
median(output_filtered$P_bomb) # 0.39
sd(output_filtered$P_bomb) # 0.17


Fig_Bomb_input.P_bomb = ggplot(data = output_filtered, aes(x = Bomb_input, y = P_bomb, color = Soil_input)) + 
  geom_point(shape = 16, size = 1, alpha = 0.5) +
  scale_color_binned() + guides(colour = guide_coloursteps(show.limits = TRUE)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  geom_hline(yintercept=0.44, linetype="dashed", linewidth = 0.5, color = "black") +
  geom_hline(yintercept=0.38, linetype="dotted", linewidth = 0.5, color = "black") +
  xlab(expression('Decadal '*italic('F')^14*'C')) + ylab('Decadal proportion') +
  annotate("text", x=1.03, y=0.475, size=3, label='0.44 ? 0.17') +
  labs(color = expression('Millennial '*italic('F')^14*'C')) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.position = c(0.85, 0.8), legend.background = element_rect(fill='transparent'))
Fig_Bomb_input.P_bomb

yplot2 = ggdensity(output_filtered, "P_bomb", color = 'sienna1', fill = 'sienna1', size = 0.25) +
  rotate() +  scale_x_continuous(limits = c(0,1))
yplot2 = yplot2 + clean_theme()
yplot2


# Millennial proportion

mean(output_filtered$P_soil) # 0.56
median(output_filtered$P_soil) # 0.61
sd(output_filtered$P_soil) # 0.17


Fig_Bomb_input.P_soil = ggplot(data = output_filtered, aes(x = Bomb_input, y = P_soil, color = Soil_input)) + 
  geom_point(shape = 16, size = 1, alpha = 0.5) +
  scale_color_binned() + guides(colour = guide_coloursteps(show.limits = TRUE)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  geom_hline(yintercept=0.56, linetype="dashed", linewidth = 0.5, color = "black") +
  geom_hline(yintercept=0.61, linetype="dotted", linewidth = 0.5, color = "black") +
  xlab(expression('Decadal '*italic('F')^14*'C')) + ylab('Millennial proportion') +
  annotate("text", x=1.03, y=0.525, size=3, label='0.56 ? 0.17') +
  labs(color = expression('Millennial '*italic('F')^14*'C')) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.position = 'none')
Fig_Bomb_input.P_soil

yplot3 = ggdensity(output_filtered, "P_soil", color = 'sienna1', fill = 'sienna1', size = 0.25) +
  rotate() +  scale_x_continuous(limits = c(0,1))
yplot3 = yplot3 + clean_theme()
yplot3


# combine plots

ggarrange(Fig_Bomb_input.P_bomb, yplot2, Fig_Bomb_input.P_soil, yplot3,  
          ncol = 2, nrow = 2,  align = "h", 
          widths = c(8, 1), heights = c(1, 1),
          common.legend=F)

ggsave("Fig_E5.png", width = 14, height = 22, units = c("cm"), dpi = 600)


## Calculating vertical fluxes based on proportions using total DIC-CO2 flux of 2.5 Pg C y-1

# Petrogenic
# 0.18 +/- 0.034 Pg C y-1 of 2.5 Pg C y-1

# proportion
0.18/2.5 # 0.072
0.034/2.5 # 0.014

# Vertical CO2 flux
0.072 * 2 # 0.144

# uncertainty propagation
0.014 * 2 # 0.028

# Petrogenic vertical CO2 flux reporting = 0.1 +/- 0.0 Pg C y-1 (0.07 +/- 0.03)


# Decadal

# proportion including petrogenic contribution
1-0.072 # 0.928
0.44 * 0.928 # 0.41
0.17 * 0.928 # 0.16

# Vertical CO2 flux (proportion of 2.0 +/- 0.2 Pg C y-1)
mean(output_filtered$P_bomb) * 2 # 0.88
sd(output_filtered$P_bomb) * 2 # 0.34

# Decadal reporting = 0.88 +/- 0.34 (0.41 +/- 0.16)


# Millennial

# proportion including petrogenic contribution
0.56 * 0.928 # 0.52
0.17 * 0.928 # 0.16

# Vertical CO2 flux (proportion of 2.0 +/- 0.2 Pg C y-1)
mean(output_filtered$P_soil) * 2 # 1.12
sd(output_filtered$P_soil) * 2 # 0.34

# Millennial reporting = 1.12 +/- 0.34 (0.52 +/- 0.16)






#### Extended Data Figure 6 ####
## Bayesian isotope mixing model

# biome binned 14C source attribution #####

# 1. input the (non-normalised) F14C data from the Biome binning 
C14_biome <- matrix(c(F14C_biome_bin), ncol = 1, nrow = 1020)

## naming the column in the matrix
colnames(C14_biome) <- c('F14C_value')


# 2. input the source information we have. In this case 3 sources:
source_names <- c("Decadal", "Millennial", "Petrogenic")

# input mean C-14 values for these sources:
source_means <- matrix(c(1.2263, 0.8411, 0.0001), ncol = 1, nrow = 3)

# input the standard deviations of these values:
source_stddevs <- matrix(c(0.2115, 0.0325, 0.0001), ncol = 1, nrow = 3)

## note source F14C values come from:

# Decadal (bomb peak) = mean 14CO2 1955-2019 from Hua et al. 2022, Radiocarbon https://doi.org/10.1017/RDC.2021.95
# 1.2263 +/- 0.2155 Fm (1955-2019)

# Millenial (0-30cm mean soil age and sd) = carbon-weighted mean age of mineral soil carbon from Shi et al. 2020, Nature Geoscience https://doi.org/10.1038/s41561-020-0596-z 
# 1390 +/- 310 yBP = 0.8411 +/- 0.0325 Fm

# Petrogenic age = as close to 0 Fm as you can get "without getting your eye wet"
# 0.0001 +/- 0.0001


# 3. input all this information into the mixing model:
simmr_input_biome = simmr_load(mixtures = C14_biome,
                               source_names = source_names,
                               source_means = source_means,
                               source_sds = source_stddevs,
                               group = biome_bin)


# 4. see the samples and sources relative to one another:
Fig_simmr_inputs_biome = plot(simmr_input_biome, group = 1:8, mix_name = '') +
  xlab(expression(italic('F')^14*'C')) + ylab('Sample grouping & potential carbon sources') +
  geom_hline(yintercept=8.5, linewidth=0.2, color = "black") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.position = "none", plot.title = element_blank(),
                     axis.text.y = element_text(angle = 45, vjust = -0.5, hjust = 1, size = 8))
Fig_simmr_inputs_biome


# 5. run the model:
simmr_output_biome <- simmr_mcmc(simmr_input_biome)


# 6. Check the results are close to 1.
## If values are above 1.1, we recommend a longer run. See help(simmr_mcmc) for how to do this.
## Note this is not very relevant here but is definitely relevant if there are more than two potential sources.
summary(simmr_output_biome, type = 'diagnostics')

# 7. Mean and standard deviation of contributions of 3 sources:
summary(simmr_output_biome, type = 'statistics', group = c(1))
summary(simmr_output_biome, type = 'statistics', group = c(2))
summary(simmr_output_biome, type = 'statistics', group = c(3))
summary(simmr_output_biome, type = 'statistics', group = c(4))
summary(simmr_output_biome, type = 'statistics', group = c(5))
summary(simmr_output_biome, type = 'statistics', group = c(6))
summary(simmr_output_biome, type = 'statistics', group = c(7))
summary(simmr_output_biome, type = 'statistics', group = c(8))

summary(simmr_output_biome, type = 'quantiles', group = c(1))
summary(simmr_output_biome, type = 'quantiles', group = c(2))
summary(simmr_output_biome, type = 'quantiles', group = c(3))
summary(simmr_output_biome, type = 'quantiles', group = c(4))
summary(simmr_output_biome, type = 'quantiles', group = c(5))
summary(simmr_output_biome, type = 'quantiles', group = c(6))
summary(simmr_output_biome, type = 'quantiles', group = c(7))
summary(simmr_output_biome, type = 'quantiles', group = c(8))

# 8. Output plots
plot(simmr_output_biome,type='histogram',group=1, title='Deserts')
plot(simmr_output_biome,type='histogram',group=2, title='Montane grass & shrub')
plot(simmr_output_biome,type='histogram',group=3, title='Temp. broadleaf & mixed for.')
plot(simmr_output_biome,type='histogram',group=4, title='Temp. conifer & boreal for.')
plot(simmr_output_biome,type='histogram',group=5, title='Temp. grass & shrub')
plot(simmr_output_biome,type='histogram',group=6, title='Trop. broadleaf & conifer for.')
plot(simmr_output_biome,type='histogram',group=7, title='Trop. grass & shrub')
plot(simmr_output_biome,type='histogram',group=8, title='Tundra')

plot(simmr_output_biome,type='boxplot',group=1, title='Deserts') 
plot(simmr_output_biome,type='boxplot',group=2, title='Montane grass & shrub')
plot(simmr_output_biome,type='boxplot',group=3, title='Temp. broadleaf & mixed for.')
plot(simmr_output_biome,type='boxplot',group=4, title='Temp. conifer & boreal for.')
plot(simmr_output_biome,type='boxplot',group=5, title='Temp. grass & shrub')
plot(simmr_output_biome,type='boxplot',group=6, title='Trop. broadleaf & conifer for.')
plot(simmr_output_biome,type='boxplot',group=7, title='Trop. grass & shrub')
plot(simmr_output_biome,type='boxplot',group=8, title='Tundra')


# Extract the source proportions for the final plot

## Deserts ##

simmr_out_Deserts <- simmr_output_biome$output[[1]]$BUGSoutput$sims.list$p
colnames(simmr_out_Deserts) <- simmr_output_biome$input$source_names

# Now turn into a proper data frame
simmr_out_Deserts.df <- reshape2::melt(simmr_out_Deserts)
colnames(simmr_out_Deserts.df) <- c("Num", "Source", "Proportion")

# add biome bin category
Deserts_simmr_group = rep("Deserts", 10800)
simmr_out_Deserts.df$biome_bin = c(Deserts_simmr_group)


## Montane grass & shrub ##

simmr_out_MGS <- simmr_output_biome$output[[2]]$BUGSoutput$sims.list$p
colnames(simmr_out_MGS) <- simmr_output_biome$input$source_names

# Now turn into a proper data frame
simmr_out_MGS.df <- reshape2::melt(simmr_out_MGS)
colnames(simmr_out_MGS.df) <- c("Num", "Source", "Proportion")

# add biome bin category
MGS_simmr_group = rep("Montane grass & shrub", 10800)
simmr_out_MGS.df$biome_bin = c(MGS_simmr_group)


## Temp. broadleaf & mixed for. ##

simmr_out_TempBM <- simmr_output_biome$output[[3]]$BUGSoutput$sims.list$p
colnames(simmr_out_TempBM) <- simmr_output_biome$input$source_names

# Now turn into a proper data frame
simmr_out_TempBM.df <- reshape2::melt(simmr_out_TempBM)
colnames(simmr_out_TempBM.df) <- c("Num", "Source", "Proportion")

# add biome bin category
TempBM_simmr_group = rep("Temp. broadleaf & mixed for.", 10800)
simmr_out_TempBM.df$biome_bin = c(TempBM_simmr_group)


## Temp. conifer & boreal for. ##

simmr_out_TempCB <- simmr_output_biome$output[[4]]$BUGSoutput$sims.list$p
colnames(simmr_out_TempCB) <- simmr_output_biome$input$source_names

# Now turn into a proper data frame
simmr_out_TempCB.df <- reshape2::melt(simmr_out_TempCB)
colnames(simmr_out_TempCB.df) <- c("Num", "Source", "Proportion")

# add biome bin category
TempCB_simmr_group = rep("Temp. conifer & boreal for.", 10800)
simmr_out_TempCB.df$biome_bin = c(TempCB_simmr_group)


## Temp. grass & shrub ##

simmr_out_TempGS <- simmr_output_biome$output[[5]]$BUGSoutput$sims.list$p
colnames(simmr_out_TempGS) <- simmr_output_biome$input$source_names

# Now turn into a proper data frame
simmr_out_TempGS.df <- reshape2::melt(simmr_out_TempGS)
colnames(simmr_out_TempGS.df) <- c("Num", "Source", "Proportion")

# add biome bin category
TempGS_simmr_group = rep("Temp. grass & shrub", 10800)
simmr_out_TempGS.df$biome_bin = c(TempGS_simmr_group)


## Trop. broadleaf & conifer for. ##

simmr_out_TropBC <- simmr_output_biome$output[[6]]$BUGSoutput$sims.list$p
colnames(simmr_out_TropBC) <- simmr_output_biome$input$source_names

# Now turn into a proper data frame
simmr_out_TropBC.df <- reshape2::melt(simmr_out_TropBC)
colnames(simmr_out_TropBC.df) <- c("Num", "Source", "Proportion")

# add biome bin category
TropBC_simmr_group = rep("Trop. broadleaf & conifer for.", 10800)
simmr_out_TropBC.df$biome_bin = c(TropBC_simmr_group)


## Trop. grass & shrub ##

simmr_out_TropGS <- simmr_output_biome$output[[7]]$BUGSoutput$sims.list$p
colnames(simmr_out_TropGS) <- simmr_output_biome$input$source_names

# Now turn into a proper data frame
simmr_out_TropGS.df <- reshape2::melt(simmr_out_TropGS)
colnames(simmr_out_TropGS.df) <- c("Num", "Source", "Proportion")

# add biome bin category
TropGS_simmr_group = rep("Trop. grass & shrub", 10800)
simmr_out_TropGS.df$biome_bin = c(TropGS_simmr_group)


## Tundra ##

simmr_out_Tundra <- simmr_output_biome$output[[8]]$BUGSoutput$sims.list$p
colnames(simmr_out_Tundra) <- simmr_output_biome$input$source_names

# Now turn into a proper data frame
simmr_out_Tundra.df <- reshape2::melt(simmr_out_Tundra)
colnames(simmr_out_Tundra.df) <- c("Num", "Source", "Proportion")

# add biome bin category
Tundra_simmr_group = rep("Tundra", 10800)
simmr_out_Tundra.df$biome_bin = c(Tundra_simmr_group)


## combine into single dataframe & plot ##

simmr_out_allgroups_biome = rbind(simmr_out_Deserts.df, simmr_out_MGS.df, simmr_out_TempBM.df, simmr_out_TempCB.df,
                                  simmr_out_TempGS.df, simmr_out_TropBC.df, simmr_out_TropGS.df, simmr_out_Tundra.df)

Fig_simmr_out_biome = ggplot(simmr_out_allgroups_biome,
                             aes(x = Source, y = Proportion,
                                 fill = factor(biome_bin, level = c('Temp. grass & shrub',
                                                                    'Tundra',
                                                                    'Temp. conifer & boreal for.',
                                                                    'Deserts',
                                                                    'Trop. broadleaf & conifer for.',
                                                                    'Temp. broadleaf & mixed for.',
                                                                    'Trop. grass & shrub',
                                                                    'Montane grass & shrub')))) +
  geom_boxplot(outlier.shape=NA) +
  scale_fill_manual(values = c("yellow","lightblue1","aquamarine4","antiquewhite","palegreen3","darkgreen","goldenrod1", "bisque3")) +
  scale_x_discrete(labels = c('Decadal', 'Millennial', 'Petrogenic')) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1)) +
  xlab('Potential carbon source') + ylab('Proportional contribution to mixture') +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(), 
                     legend.title=element_blank(), legend.position = c(0.81, 0.78),
                     legend.background = element_rect(fill='transparent'), legend.text = element_text(size=8))
Fig_simmr_out_biome



##### lithology binned 14C source attribution #####

## Original information on using this tool is from:
## https://cran.r-project.org/web/packages/simmr/vignettes/simmr.html

# 1. input the (non-normalised) F14C data from the lithology binning 
C14_lith <- matrix(c(F14C_lith_bin), ncol = 1, nrow = 1004)

## naming the column in the matrix
colnames(C14_lith) <- c('F14C_value')


# 2. Sources as above for Biomes...


# 3. input all this information into the mixing model:
simmr_input_lith = simmr_load(mixtures = C14_lith,
                              source_names = source_names,
                              source_means = source_means,
                              source_sds = source_stddevs,
                              group = lith_bin)


# 4. see the samples and sources relative to one another:
Fig_simmr_inputs_lith = plot(simmr_input_lith, group = 1:3, mix_name = '') +
  xlab(expression(italic('F')^14*'C')) + ylab('Sample grouping & potential carbon sources') +
  geom_hline(yintercept=3.5, linewidth=0.2, color = "black") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.position = "none", plot.title = element_blank(),
                     axis.text.y = element_text(angle = 45, vjust = -0.5, hjust = 1, size = 8))
Fig_simmr_inputs_lith


# 5. run the model:
simmr_output_lith <- simmr_mcmc(simmr_input_lith)


# 6. Check the results are close to 1.
## If values are above 1.1, we recommend a longer run. See help(simmr_mcmc) for how to do this.
## Note this is not very relevant here but is definitely relevant if there are more than two potential sources.
summary(simmr_output_lith, type = 'diagnostics')

# 7. Mean and standard deviation of contributions of 3 sources:
summary(simmr_output_lith, type = 'statistics', group = c(1))
summary(simmr_output_lith, type = 'statistics', group = c(2))
summary(simmr_output_lith, type = 'statistics', group = c(3))

summary(simmr_output_lith, type = 'quantiles', group = c(1))
summary(simmr_output_lith, type = 'quantiles', group = c(2))
summary(simmr_output_lith, type = 'quantiles', group = c(3))


# 8. Output plots
plot(simmr_output_lith,type='histogram',group=1, title='Igneous')
plot(simmr_output_lith,type='histogram',group=2, title='Metamorphic')
plot(simmr_output_lith,type='histogram',group=3, title='Sedimentary')

plot(simmr_output_lith,type='boxplot',group=1, title='Igneous') 
plot(simmr_output_lith,type='boxplot',group=2, title='Metamorphic')
plot(simmr_output_lith,type='boxplot',group=3, title='Sedimentary')


# Extract the source proportions for the final plot

## Igneous ##

simmr_out_ign <- simmr_output_lith$output[[1]]$BUGSoutput$sims.list$p
colnames(simmr_out_ign) <- simmr_output_lith$input$source_names

# Now turn into a proper data frame
simmr_out_ign.df <- reshape2::melt(simmr_out_ign)
colnames(simmr_out_ign.df) <- c("Num", "Source", "Proportion")

# add lithology bin category
ign_simmr_group = rep("Ign.", 10800)
simmr_out_ign.df$lith_bin = c(ign_simmr_group)


## Metamorphic ##

simmr_out_met <- simmr_output_lith$output[[2]]$BUGSoutput$sims.list$p
colnames(simmr_out_met) <- simmr_output_lith$input$source_names

# Now turn into a proper data frame
simmr_out_met.df <- reshape2::melt(simmr_out_met)
colnames(simmr_out_met.df) <- c("Num", "Source", "Proportion")

# add lithology bin category
met_simmr_group = rep("Met.", 10800)
simmr_out_met.df$lith_bin = c(met_simmr_group)


## Sedimentary ##

simmr_out_sed <- simmr_output_lith$output[[3]]$BUGSoutput$sims.list$p
colnames(simmr_out_sed) <- simmr_output_lith$input$source_names

# Now turn into a proper data frame
simmr_out_sed.df <- reshape2::melt(simmr_out_sed)
colnames(simmr_out_sed.df) <- c("Num", "Source", "Proportion")

# add lithology bin category
sed_simmr_group = rep("Sed.", 10800)
simmr_out_sed.df$lith_bin = c(sed_simmr_group)


## combine into single dataframe & plot ##

simmr_out_allgroups_lith = rbind(simmr_out_ign.df, simmr_out_met.df, simmr_out_sed.df)

Fig_simmr_out_lith = ggplot(simmr_out_allgroups_lith, aes(x = Source, y = Proportion, fill = factor(lith_bin, level = c('Met.','Ign.','Sed.')))) +
  geom_boxplot(outlier.shape=NA) +
  scale_fill_manual(values = c("seashell4","seashell3","seashell1"), labels = c('Metamorphic','Igneous','Sedimentary')) +
  scale_x_discrete(labels = c('Decadal', 'Millennial', 'Petrogenic')) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1)) +
  xlab('Potential carbon source') + ylab('Proportional contribution to mixture') +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.y = element_blank(),
                     axis.title.x = element_blank(),legend.title=element_blank(), legend.position = c(0.25, 0.9),
                     legend.background = element_rect(fill='transparent'), legend.text = element_text(size=8))
Fig_simmr_out_lith



## Figure E6 combined ##

ggdraw() +
  draw_plot(Fig_simmr_out_biome,  x = 0,     y = 0, width = 0.7,  height = 1) +
  draw_plot(Fig_simmr_out_lith,   x = 0.71,  y = 0, width = 0.29,  height = 1) +
  draw_plot_label(label = c("A","B"), size = 10,
                  x = c(0.005, 0.7),
                  y = c(1, 1))

ggsave("Fig_E6.png", width = 22, height = 13, units = c("cm"), dpi = 600)







#################################
##### Supplementary figures #####
#################################


## Supplementary Fig. S1 ##
# paired DIC-CO2 F14C - DIC isotopic equilibrium

CO2DIC_eq = read.csv("CO2_DIC_eq.csv")

# calculate difference between CO2 and DIC F14C
CO2DIC_eq$CO2_DIC = c(CO2DIC_eq$CO2-CO2DIC_eq$DIC)
mean(CO2DIC_eq$CO2_DIC) # 0.02 
median(CO2DIC_eq$CO2_DIC) # 0.01
min(CO2DIC_eq$CO2_DIC)  # -0.03
max(CO2DIC_eq$CO2_DIC)  # 0.13

CO2DIC_eq.lm = lm(CO2DIC_eq$CO2 ~ CO2DIC_eq$DIC)
summary(CO2DIC_eq.lm)$r.squared # R^2 = 0.7743999
summary(CO2DIC_eq.lm)$coefficients[,4] # p-value = 1.455165e-08***

Fig_CO2DIC_eq = ggplot(data = CO2DIC_eq, aes(x = DIC, y = CO2, shape = factor(Data_source,
                                                                              level = c('This study','Garcia-Tigreros, F., Elder, C.D., Kurek, M.R. et al. (2023) Arctic-boreal lakes of interior Alaska dominated by contemporary carbon. Environ. Res. Lett. 18, 124024. https://doi.org/10.1088/1748-9326/ad0993')))) + 
  geom_point(color="black") +
  geom_smooth(method=lm, level = 0.95, linewidth = 0.3, linetype="dashed", aes(group=FALSE), col = "black", fill = "seagreen1") +
  scale_shape_manual(values = c(19,17),
                     labels = c('This study', 'Garcia-Tigreros et al.')) +
  scale_y_continuous(limits = c(0.78,1.05), breaks = c(0.8,0.9,1)) +
  scale_x_continuous(limits = c(0.78,1.05), breaks = c(0.8,0.9,1)) +
  xlab(expression(italic('F')^14*'C-DIC')) + ylab(expression(italic('F')^14*'C-CO'[2])) +
  geom_abline(intercept = 0, slope = 1, linewidth = 0.25, color = "grey") +
  annotate("text", x=0.85, y=0.98, size=3, label=expression('R'^2*' = 0.77, p << 0.001')) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.title=element_blank(), legend.position=c(0.7, 0.15), legend.background = element_rect(fill='transparent'))
Fig_CO2DIC_eq

Fig_CO2DIC_eq_pH = ggplot(data = CO2DIC_eq, aes(x = pH, y = CO2_DIC, shape = factor(Data_source))) + 
  geom_point(color="black") +
  scale_shape_manual(values = c(17,19),
                     labels = c('Garcia-Tigreros et al.', 'This study')) +
  scale_y_continuous(limits = c(-0.2,0.2), breaks = c(-0.2,-0.1,0,0.1,0.2)) +
  scale_x_continuous(limits = c(3,10), breaks = c(3,4,5,6,7,8,9,10)) +
  xlab('pH') + ylab(expression(italic('F')^14*'C difference (CO'[2]*' - DIC)')) +
  geom_hline(yintercept=0, linewidth = 0.25, color = "grey75") +
  geom_segment(aes(x = 5.5, y = -0.2, xend = 8.5, yend = -0.2), linewidth = 1, color = "steelblue1", alpha = 0.05) +
  annotate("text", x=5.5, y=-0.175, size=3, label='pH of most natural waters', color = "steelblue1") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.position = "none")
Fig_CO2DIC_eq_pH


## Fig S1 combined ##

ggdraw() +
  draw_plot(Fig_CO2DIC_eq,    x = 0,   y = 0, width = 0.49, height = 1) +
  draw_plot(Fig_CO2DIC_eq_pH, x = 0.5, y = 0, width = 0.49, height = 1) +
  draw_plot_label(label = c("A","B"), size = 10,
                  x = c(0.005, 0.49),
                  y = c(1, 1))

ggsave("Fig_S1.png", width = 18, height = 9, units = c("cm"), dpi = 600)





## Supplementary Fig. S2-3 ##
# global lithology and biome distributions
# made independently to this code using ArcGIS





## Supplementary Fig. S4 ##
# Lithology and Biome representativeness

Represent_lith = read.csv("Represent-Lith.csv")
Represent_biome = read.csv("Represent-Biome.csv")

# Lithology

Fig_Represent_lith = ggplot(data = Represent_lith, aes(x = Lithology, y = Proportion,
                                                       fill = factor(Group, level = c("Global", "Database")))) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values = c("grey70", "black")) +
  scale_y_continuous(limits = c(0,0.63), breaks = c(0,0.2,0.4,0.6)) +
  theme_bw() + theme(legend.title=element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
                     axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8), axis.title.x = element_blank(),
                     legend.position="none")
Fig_Represent_lith

# Biome

Fig_Represent_biome = ggplot(data = Represent_biome, aes(x = Biome, y = Proportion,
                                                         fill = factor(Group, level = c("Global", "Database")))) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values = c("grey70", "black")) +
  scale_y_continuous(limits = c(0,0.63), breaks = c(0,0.2,0.4,0.6)) +
  theme_bw() + theme(legend.title=element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
                     axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8), axis.title.x = element_blank())
Fig_Represent_biome

# combine plots

plot_grid(Fig_Represent_lith, Fig_Represent_biome, align = "h", rel_widths = c(1,2.7),
          labels = c('A','B'))

ggsave("Fig_S4.png", width = 16, height = 10, units = c("cm"), dpi = 600)







## Supplementary Fig. S5 ##
# F14Catm histogram
# code under Fig. 1 and initial analyses






## Supplementary Fig. S6 ##
# Catchment size comparison - manually extracted data v database

Fig_sizecomp = ggplot(Glob_F14C_all, aes(x = watershed_size_indicative_km2, y = catchment_area_km2,
                                         shape = factor(compound, level = c('DIC','CO2','CH4')),
                                         color = factor(compound, level = c('DIC','CO2','CH4')))) +
  geom_point(size = 0.75, alpha=0.7) +
  scale_color_manual(values = c(DIC = "gray70", CO2 = "sienna1", CH4 = "darkslategray"),
                     labels = c('DIC', expression('CO'[2]), expression('CH'[4]))) +
  scale_shape_manual(values = c(16,15,4),
                     labels = c('DIC', expression('CO'[2]), expression('CH'[4]))) +
  
  scale_x_continuous(limits = c(0.001,10000000), breaks = c(0.001,0.1,10,1000,100000,10000000), trans='log10') +
  scale_y_continuous(limits = c(0.001,10000000), breaks = c(0.001,0.1,10,1000,100000,10000000), trans='log10') +
  xlab(expression('Exact/indicative catchment area (km'^2*')')) +
  ylab(expression('HydroATLAS catchment area (km'^2*')')) +
  geom_abline (slope=1, linewidth = 0.3, color="grey") +
  geom_segment(aes(x = 10, y = 0, xend = 10, yend = 10), linetype = "dashed", linewidth = 0.3, color = "black") +
  geom_segment(aes(x = 0, y = 10, xend = 10, yend = 10), linetype = "dashed", linewidth = 0.3, color = "black") +
  annotate("text", x=0.1, y=0.01, size=2.5, color = "grey", label='1:1 line') +
  annotate("text", x=45, y=0.1, size=2.5, color = "black", label=expression('10 km'^2)) +
  annotate("text", x=0.002, y=30, size=2.5, color = "black", label=expression('10 km'^2)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.key = element_rect(fill = "white"),
                     legend.title=element_blank())
Fig_sizecomp

ggsave("Fig_S6.png", width = 13, height = 10, units = c("cm"), dpi = 600)







## Supplementary Fig. S8 ##
# F14C DIC-CO2 vs. pH

# All data v. pH

Glob_F14C_all$pH = as.numeric(Glob_F14C_all$pH)

mean(Glob_F14C_all$pH, na.rm = T) # 7.3
median(Glob_F14C_all$pH, na.rm = T) # 7.6

Fig_F14C_pH = ggplot(data = subset(Glob_F14C_all, !is.na(pH)), aes(x = pH, y = norm_F14C,
                                                                   shape = factor(compound, level = c('DIC','CO2','CH4')),
                                                                   color = factor(compound, level = c('DIC','CO2','CH4')))) +
  geom_point(size = 0.75, alpha=0.7) +
  scale_color_manual(values = c(DIC = "gray70", CO2 = "sienna1", CH4 = "darkslategray"),
                     labels = c('DIC', expression('CO'[2]), expression('CH'[4]))) +
  scale_shape_manual(values = c(16,15,4),
                     labels = c('DIC', expression('CO'[2]), expression('CH'[4]))) +
  scale_y_continuous(limits = c(0,1.4), breaks = c(0,0.2,0.4,0.6,0.8,1,1.2,1.4)) +
  scale_x_continuous(limits = c(4,10), breaks = c(4,5,6,7,8,9,10)) +
  xlab('pH') + ylab(expression(italic('F')^14*'C'[atm])) +
  geom_vline(xintercept = 7.3, linewidth = 0.25, color = "black") +
  geom_vline(xintercept = 7.6, linetype = 'dashed', linewidth = 0.25, color = "black") +
  geom_segment(aes(x = 5.5, y = 0, xend = 8.5, yend = 0), linewidth = 0.5, color = "steelblue1", alpha = 0.025) +
  annotate("text", x=4.8, y=0.12, size=3, label='pH of most', color = "steelblue1") +
  annotate("text", x=5.5, y=0.05, size=3, label='natural waters', color = "steelblue1") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.key = element_rect(fill = "white"),
                     legend.title=element_blank())
Fig_F14C_pH

# x-axis density plot

xplot3 = ggdensity(Glob_F14C_all, "pH", color = 'steelblue1', fill = 'steelblue1', size = 0.25) +
  scale_x_continuous(limits = c(3,10))
xplot3 = xplot3 + clean_theme()
xplot3


## CO2 v pH

CO2_pH.lm = lm(Glob_F14C_CO2$norm_F14C ~ Glob_F14C_CO2$pH)
summary(CO2_pH.lm)$r.squared # R^2 = 0.33293922
summary(CO2_pH.lm) # p-value = 3.421e-13***

Fig_CO2_pH.lm = ggplot(data = Glob_F14C_CO2, aes(x = pH, y = norm_F14C)) + 
  geom_point(color="black", shape = 16, size = 1, alpha = 0.5) +
  geom_smooth(method=lm, level = 0.95, linewidth = 0.3, linetype="dashed", aes(group=FALSE), col = "black", fill = "seagreen1") +
  scale_y_continuous(limits = c(0,1.4), breaks = c(0,0.2,0.4,0.6,0.8,1,1.2,1.4)) +
  scale_x_continuous(limits = c(4,10), breaks = c(4,5,6,7,8,9,10)) +
  xlab('pH') + ylab(expression(italic('F')^14*'C'[atm]*'-CO'[2])) +
  annotate("text", x=5.75, y=1.38, size=3, label=expression('R'^2*' = 0.33, p << 0.001')) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
Fig_CO2_pH.lm


## DIC v pH

DIC_pH.lm = lm(Glob_F14C_DIC$norm_F14C ~ Glob_F14C_DIC$pH)
summary(DIC_pH.lm)$r.squared # R^2 = 0.1199658
summary(DIC_pH.lm) # p-value < 2.2e-16***

Fig_DIC_pH.lm = ggplot(data = Glob_F14C_DIC, aes(x = pH, y = norm_F14C)) + 
  geom_point(color="black", shape = 16, size = 1, alpha = 0.5) +
  geom_smooth(method=lm, level = 0.95, linewidth = 0.3, linetype="dashed", aes(group=FALSE), col = "black", fill = "seagreen1") +
  scale_y_continuous(limits = c(0,1.4), breaks = c(0,0.2,0.4,0.6,0.8,1,1.2,1.4)) +
  scale_x_continuous(limits = c(4,10), breaks = c(4,5,6,7,8,9,10)) +
  xlab('pH') + ylab(expression(italic('F')^14*'C'[atm]*'-DIC')) +
  annotate("text", x=5.75, y=1.38, size=3, label=expression('R'^2*' = 0.12, p << 0.001')) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
Fig_DIC_pH.lm


# combine plots

ggdraw() +
  draw_plot(xplot3,        x = 0.275, y = 0.93, width = 0.47, height = 0.08) +
  draw_plot(Fig_F14C_pH,   x = 0.2,  y = 0.475, width = 0.7,  height = 0.475) +
  draw_plot(Fig_CO2_pH.lm, x = 0,    y = 0,     width = 0.5,  height = 0.475) +
  draw_plot(Fig_DIC_pH.lm, x = 0.5,  y = 0,     width = 0.5,  height = 0.475) +
  draw_plot_label(label = c("A","B","C"), size = 10,
                  x = c(0.2, 0.005, 0.5),
                  y = c(0.95, 0.47,  0.47))

ggsave("Fig_S8.png", width = 14, height = 14, units = c("cm"), dpi = 600)






## Supplementary Fig. S9 ##
# DIC v conc, conc v pH

Glob_F14C_ND_DIC$pH = as.numeric(Glob_F14C_ND_DIC$pH)

Fig_DIC_conc = ggplot(data = subset(Glob_F14C_ND_DIC, !is.na(DIC_conc)), aes(x = DIC_conc, y = norm_F14C)) +
  geom_point(color="black", shape = 16, size = 0.9, alpha = 0.7) +
  scale_y_continuous(limits = c(0,1.2), breaks = c(0,0.2,0.4,0.6,0.8,1,1.2)) +
  scale_x_continuous(limits=c(0,12000), breaks=c(0,2000,4000,6000,8000,10000,12000)) +
  xlab(expression('DIC concentration ('*mu*'mol L'^-1*')')) + ylab(expression(italic('F')^14*'C'[atm]*'-DIC')) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
Fig_DIC_conc

Fig_DIC_pHconc = ggplot(data = subset(Glob_F14C_ND_DIC, !is.na(DIC_conc)), aes(x = DIC_conc, y = pH)) +
  geom_point(color="black", shape = 16, size = 0.9, alpha = 0.7) +
  scale_y_continuous(limits = c(3,10), breaks = c(3,4,5,6,7,8,9,10)) +
  scale_x_continuous(limits=c(0,12000), breaks=c(0,2000,4000,6000,8000,10000,12000)) +
  xlab(expression('DIC concentration ('*mu*'mol L'^-1*')')) + ylab('pH') + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
Fig_DIC_pHconc


## Fig S9 combined ##

ggdraw() +
  draw_plot(Fig_DIC_conc,   x = 0,   y = 0, width = 0.5, height = 1) +
  draw_plot(Fig_DIC_pHconc, x = 0.5, y = 0, width = 0.5, height = 1) +
  draw_plot_label(label = c("A","B"), size = 10,
                  x = c(0.005, 0.5),
                  y = c(1, 1))

ggsave("Fig_S9.png", width = 16, height = 7.5, units = c("cm"), dpi = 600)



####################### END #######################
