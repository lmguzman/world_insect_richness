library(tidyverse)

#### Load complete data  #####

Costa_rica_data <- read.delim(file = "Data/Raw/CR_all_rec_report_output.tsv", sep = '\t')

#### 01_main analysis ####

##### CORE Traps #####

#### Core AGC all Insecta 

coreACG_all <- Costa_rica_data%>%
  filter((str_detect(extrainfo,"ESG|BSE|PL12|Derrumbe")))%>%
  filter(str_detect(class, "Insecta"))%>%
  filter(!uri=="")

coreACG_BIN <- coreACG_all%>%
  group_by(uri) %>%
  dplyr::summarize(Frequency = n()) %>%
  ungroup() %>% 
  rename(BIN = uri)

write.csv(coreACG_BIN, 'Data/Insecta_ACG_Core_Malaise.csv', row.names = FALSE)


##### Core traps Microgastrinae

coreACG_Micro <- coreACG_all%>%
  filter(str_detect(subfamily, "Microgastrinae"))

write.csv(coreACG_Micro, 'Data/Intermediate/Microgastrinae_ACG_Core_Malaise.csv', row.names = FALSE)

coreACG_Micro_BIN <- coreACG_Micro%>%
  group_by(uri) %>%
  dplyr::summarize(Frequency = n()) %>%
  ungroup() %>% 
  rename(BIN = uri)

write.csv(coreACG_Micro_BIN, 'Data/Microgastrinae_ACG_Core_Malaise.csv', row.names = FALSE)


##### PERIPHERAL TRAPS ####

#### Peripheral traps microgastrinae

periACG_micro <- Costa_rica_data %>%
  filter(str_detect(subfamily, "Microgastrinae"))%>%
  filter((str_detect(extrainfo,"SSM|SMNR|SMNPL|Pedregal|Harold|Circular|Gongora|Cima|Arenales|CJAN|NAR|SGF|SGC|BT0|LDR|MBT|Sombra|LuzSol|Pitilla|malaise-trapped")))%>%
  filter(!uri=="")

periACG_MicroBIN <- periACG_micro%>%
  group_by(uri) %>%
  dplyr::summarize(Frequency = n()) %>%
  ungroup() %>% 
  rename(BIN = uri)

write.csv(periACG_MicroBIN, 'Data/Microgastrinae_ACG_Peripheral_Malaise.csv', row.names = FALSE)

##### REARED SPECIMENS ####

# reared Microgastrinae

rearedACG_all <- Costa_rica_data %>%
  filter(str_detect(subfamily, "Microgastrinae"))%>%
  filter((str_detect(sampleid, "DHJPAR")))%>%
  filter(!str_detect(extrainfo,"malaise|light|trapped"))%>%
  filter(!extrainfo=="")%>%
  filter(!uri=="")

write.csv(rearedACG_all, 'Data/Intermediate/Microgastrinae_ACG_reared.csv', row.names = FALSE)

rearedACG_MicroBIN <- rearedACG_all%>%
  group_by(uri) %>%
  dplyr::summarize(Frequency = n()) %>%
  ungroup() %>% 
  rename(BIN = uri)

write.csv(rearedACG_MicroBIN, 'Data/Microgastrinae_ACG_reared.csv', row.names = FALSE)

#### 02_other subfamilies ####

sub_families <- c("Agathidinae", "Anomaloninae", "Campopleginae", "Cardiochilinae",
                       "Cheloninae", "Euphorinae", "Homolobinae", "Macrocentrinae", "Mesochorinae",
                       "Metopiinae", "Microgastrinae", "Ophioninae", "Orgilinae", "Rogadinae", "Tryphoninae")

for(sf in sub_families){
  
  ## filter core data by subfamily 
  
  coreACG_sf <- coreACG_all%>%
    filter(str_detect(subfamily, sf))
  
  coreACG_sf_BIN <- coreACG_sf%>%
    group_by(uri) %>%
    dplyr::summarize(Core_Frequency = n()) %>%
    ungroup() %>% 
    rename(BIN = uri)
  
  ## filter reared data by subfamily 
  
  rearedACG_sf <- Costa_rica_data %>%
    filter(str_detect(subfamily, sf))%>%
    filter((str_detect(sampleid, "DHJPAR")))%>%
    filter(!str_detect(extrainfo,"malaise|light|trapped"))%>%
    filter(!extrainfo=="")%>%
    filter(!uri=="")
  
  rearedACG_sf_BIN <- rearedACG_sf%>%
    group_by(uri) %>%
    dplyr::summarize(Reared_Frequency = n()) %>%
    ungroup() %>% 
    rename(BIN = uri)
  
  sf_BIN_count <- full_join(coreACG_sf_BIN, rearedACG_sf_BIN)
  
  write.csv(sf_BIN_count, paste0("Data/BIN_abundance_subfamilies/", sf, ".csv"), row.names = FALSE)
}

#### 03_probability malaise trapping ####

insect_order_core_bin <- coreACG_all %>% 
  select(BIN = uri, order) %>% 
  unique() %>% 
  group_by(order) %>% 
  summarise(distinctBINs = n()) 
  
write.csv(insect_order_core_bin, "Data/Insect_Orders_count.csv", row.names = FALSE)


#### 04_supplement ####

## Braconidae

coreACG_Brac <- coreACG_all %>% 
  filter(family == 'Braconidae')

coreACG_BIN_brac <- coreACG_Brac%>%
  group_by(uri) %>%
  dplyr::summarize(Frequency = n()) %>%
  ungroup() %>% 
  rename(BIN = uri)

write.csv(coreACG_BIN_brac, 'Data/Braconidae_ACG_Core_Malaise.csv', row.names = FALSE)

## Ichneumonoidea 

coreACG_Ichn <- coreACG_all %>% 
  filter(family %in% c('Braconidae', 'Ichneumonidae'))

coreACG_BIN_Ichn <- coreACG_Ichn%>%
  group_by(uri) %>%
  dplyr::summarize(Frequency = n()) %>%
  ungroup() %>% 
  rename(BIN = uri)

write.csv(coreACG_BIN_Ichn, 'Data/Ichneumonoidea_ACG_Core_Malaise.csv', row.names = FALSE)

## Hymenoptera

coreACG_Hym <- coreACG_all %>% 
  filter(order == 'Hymenoptera')

coreACG_BIN_Hym <- coreACG_Hym%>%
  group_by(uri) %>%
  dplyr::summarize(Frequency = n()) %>%
  ungroup() %>% 
  rename(BIN = uri)

write.csv(coreACG_BIN_Hym, 'Data/Hymenoptera_ACG_Core_Malaise.csv', row.names = FALSE)


#### Figures ####

## Malaise trap locations

locations_core <- coreACG_all %>% 
  filter(str_detect(subfamily, "Microgastrinae")) %>% 
  select(lat, long) %>% 
  filter(!is.na(lat)) %>%
  unique() %>% 
  mutate(trap = 'core')


## Peripheral locations

peripheral_mapping <- readxl::read_excel("Data/peri_locations_mapping.xlsx", sheet = 2) %>% 
  rename(lat = `lat(old)`, long = `lon (old)`, new_lat = `lat (in map)`, new_long = `lon (in map)`)

original_peripheral_locations <- periACG_micro %>% 
  filter(str_detect(subfamily, "Microgastrinae")) %>% 
  select(lat, long) %>% 
  filter(!is.na(lat)) %>%
  unique() 

locations_peripheral <- original_peripheral_locations %>% 
  left_join(peripheral_mapping) %>% 
  mutate(new_lat = ifelse(`...6` == "CRI|NO02_527|MBT1", 10.92, new_lat)) %>% 
  mutate(new_long = ifelse(`...6` == "CRI|NO02_527|MBT1", -85.72, new_long)) %>% 
  mutate(new_lat = ifelse(is.na(new_lat), 10.80396, new_lat),
         new_long = ifelse(is.na(new_long), -85.32524, new_long)) %>% 
  select(new_lat, new_long) %>% unique() %>% 
  mutate(trap = 'peripheral') %>% 
  rename(lat = new_lat, long = new_long)


locations <- bind_rows(locations_peripheral, locations_core)

write.csv(locations, "Data/Locations/malaise_traps.csv", row.names = FALSE)

## rearing locations 

rearing_locations <- rearedACG_all %>% 
  select(lat, long) %>% unique()

write.csv(rearing_locations, "Data/Locations/rearing.csv", row.names = FALSE)

## peripheral locations for distance decay

periACG_micro_lat_fixed <- periACG_micro %>% 
  left_join(peripheral_mapping) %>% 
  mutate(new_lat = ifelse(`...6` == "CRI|NO02_527|MBT1", 10.92, new_lat)) %>% 
  mutate(new_long = ifelse(`...6` == "CRI|NO02_527|MBT1", -85.72, new_long)) %>% 
  mutate(new_lat = ifelse(is.na(new_lat), 10.80396, new_lat),
         new_long = ifelse(is.na(new_long), -85.32524, new_long)) %>% 
  select(-lat, -long) %>% 
  rename(lat = new_lat, long = new_long)
  
write.csv(periACG_micro_lat_fixed, 'Data/Intermediate/Microgastrinae_ACG_Peripheral_Malaise.csv', row.names = FALSE)

#### Key stats ####

# number of records for core traps
nrow(coreACG_Micro)
#3781

# number of records for peripheral traps
nrow(periACG_micro)
#6515
  
# number of records for rearing micro
nrow(rearedACG_all)
# 11373

# number of caterpillar species 
rearedACG_all$extrainfo %>% unique() %>% length()
# 1523

# total number of records
3781+6515+11373
#21669

# unique number of micro bins
c(coreACG_Micro$uri, periACG_micro$uri, rearedACG_all$uri) %>% unique() %>% length()
c(coreACG_Micro$uri,rearedACG_all$uri) %>% unique() %>% length()

#1414

# total number of parasitic wasps
rearedACG_hym <- Costa_rica_data %>%
  filter(order == 'Hymenoptera') %>% 
  filter((str_detect(sampleid, "DHJPAR")))%>%
  filter(!str_detect(extrainfo,"malaise|light|trapped"))%>%
  filter(!extrainfo=="")%>%
  filter(!uri=="")

rearedACG_hym$uri %>% unique %>% length()
# 2618

# number of reared caterpillars
rearedACG_hym$extrainfo %>% unique () %>% length()
#2967


## shared core, peripheral, rearing

core_bins <- data.frame(BIN = unique(coreACG_Micro$uri), core = 1)
peri_bins <- data.frame(BIN = unique(periACG_micro$uri), peri = 1)
reared_bins <- data.frame(BIN = unique(rearedACG_all$uri), reared =1)

all_bins <- full_join(core_bins,peri_bins) %>% 
  full_join(reared_bins)

## share by all

all_bins %>% filter(core == 1 & peri == 1 & reared == 1) %>% nrow()
#69

# share by reared and core but not peripheral

all_bins %>% filter(core == 1 & is.na(peri) & reared == 1) %>% nrow()
#40

# share by reared and peripheral but not core

all_bins %>% filter(is.na(core) & peri == 1 & reared == 1) %>% nrow()
#114

# share by peripheral and core but not reared 

all_bins %>% filter(core == 1 & peri == 1 & is.na(reared)) %>% nrow()
#147

# unique core

all_bins %>% filter(core == 1 & is.na(peri) & is.na(reared)) %>% nrow()
#132

# unique peripheral 
all_bins %>% filter(is.na(core) & peri == 1 & is.na(reared)) %>% nrow()
#246 

# unique to reared 

all_bins %>% filter(is.na(core) & is.na(peri) & reared == 1) %>% nrow()
#666