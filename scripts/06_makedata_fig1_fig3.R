### Script 6: Additional files for notebook on farm size (fig1) and model events (fig 3)

#load libraries
library(tidyverse)
library(sf)


## Part 1: Figure 1 Abridged farm counts
fs <- st_read("inputdata/africa_2020_2050sizes.shp")
fs_shp <- fs %>% select(-farm_sz, -year, -value) %>% unique()

fs$variable <- ifelse(fs$year==2020, "Current Farms", "Farms 2050")

frm_short <- fs %>% st_drop_geometry()
frm_short <- frm_short %>% group_by(GID_0, NAME_0, NAME_1, nm_slc0, variable) %>% summarize(value=sum(value, na.rm=TRUE))
frm_short$farm_sz <- "All Farms"
frm_short <- left_join(frm_short, fs_shp, by=c("GID_0", "NAME_0", "NAME_1", "nm_slc0"))
frm_short <- frm_short %>% select(GID_0, NAME_0, NAME_1, farm_sz, value, nm_slc0, variable, geometry)


#add percent change
pct <- frm_short %>% st_drop_geometry()
pct <- pct %>% spread(variable, value)
pct$value <- ((pct$`Farms 2050`-pct$`Current Farms`)/pct$`Current Farms`)*100
pct <- pct %>% select(-`Current Farms`, -`Farms 2050`)
pct$variable <- "Percent Change"
pct <- pct %>% select(GID_0, NAME_0, NAME_1, farm_sz, value, nm_slc0, variable)
pct$geometry <- fs_shp$geometry
allfarms <- bind_rows(frm_short, pct)


#add farm size notation
allfarms$farm_sz2 <- allfarms$farm_sz
allfarms$farm_sz2 <- ifelse(allfarms$farm_sz2=="N0_1", "Under 1 HA",
                             ifelse(allfarms$farm_sz=="N1_2", "1-2 HA",
                                    ifelse(allfarms$farm_sz=="N2_5","2-5 HA",
                                           ifelse(allfarms$farm_sz=="N5_10", "5-10 HA",
                                                  ifelse(allfarms$farm_sz=="N10_20", "10-20 HA",
                                                         ifelse(allfarms$farm_sz=="N20_", "20+ HA", "All Farms"))))))

st_write(allfarms, "finaloutput/all_shps.shp", delete_layer = TRUE)




## Part 2: For figure 3 bar graph
ensemble <- read_csv("intermediate/spei_12_ensemble.csv")
gfdl <- read_csv("intermediate/spei_12_gfdlesm4.csv")
ecearth <- read_csv("intermediate/spei_12_ecearth3.csv")
mri <- read_csv("intermediate/spei_12_mriesm2.csv")
mpi <- read_csv("intermediate/spei_12_mpiesmhr.csv")
nor <- read_csv("intermediate/spei_12_noresm2lm.csv")
joint <- bind_rows(ensemble, gfdl, ecearth, mri, mpi, nor)
joint <- joint %>% filter(high==1)
joint2 <- joint %>% group_by(variable, model, gcm) %>% summarize(count=n())
joint2$variable <- ifelse(joint2$variable=="flood", "Very Wet", "Very Dry")
joint2$model <- ifelse(joint2$model=="historical", "Historical (1995-2014)",
                       ifelse(joint2$model=="ssp245", "SSP245 (2040-2060)", "SSP585 (2040-2060)"))
joint2$gcm <- ifelse(joint2$gcm=="ecearth3", "EC-Earth3",
                     ifelse(joint2$gcm=="gfdlesm4", "GFDL-ESM4",
                            ifelse(joint2$gcm=="mriesm2", "MRI-ESM2-0",
                                   ifelse(joint2$gcm=="mpiesmhr", "MPI-ESM1-2-HR",
                                          ifelse(joint2$gcm=="noresm2lm", "NorESM2-LM",
                                                 ifelse(joint2$gcm=="ensemble", "Ensemble Average",NA))))))

write_csv(joint2, "finaloutput/spei_12_high_counts.csv")
