library(sf)
library(ggplot2)
library(cowplot)
library(dplyr)
library(ggspatial)
#remotes::install_github("wmgeolab/rgeoboundaries")
library(rgeoboundaries)

### read in points

malaise <- read.csv('Data/locations/Locations for map Figure 1.csv')

#malaise_dan <- read.csv("Data/locations/Janzen Costa Rica full table Malaise_20241121/Sheet1-Table 1.csv")

## comment from Dirk: one set of lat/lon needs to be replaced for CRI|NO02_527|MBT1: 10.92, -85.72 – the data I provided initially was based on wrong information.

malaise_corrected <- malaise %>% 
  mutate(lat = ifelse(CBG.Site.Code == 'CRI|NO02_527|MBT1', 10.92, lat),
         lon = ifelse(CBG.Site.Code == 'CRI|NO02_527|MBT1', -85.72, lon))

lat_lon_malaise <- malaise_corrected %>% 
  st_as_sf(
    coords = c("lon", "lat"),
    agr = "constant",
    crs = 4326,  ##WGS84   
    #crs = 4269,
    stringsAsFactors = FALSE,
    remove = FALSE)

## rearing sites

micro_reared <- read.csv('Data/locations/Microgastrines_reared_sites.csv') %>% 
  filter(!is.na(lat))

lat_lon_reared <- micro_reared %>% 
  st_as_sf(
    coords = c("lon", "lat"),
    agr = "constant",
    crs = 4326,  ##WGS84   
    #crs = 4269,
    stringsAsFactors = FALSE,
    remove = FALSE)

## Costa Rica ## 

CR_boundary <- geoboundaries("Costa Rica")

CR_boundary_crop <- st_crop(CR_boundary
                            , xmin = -86, xmax = -82.5, ymin = 8, ymax = 11.5)

Guanacaste_p <- geoboundaries("Costa Rica", adm_lvl = "adm1") %>% 
  filter(shapeName == "Provincia Guanacaste")


#### conservation areas ###

acg_bc <- read_sf("shapefiles/new/ACG_BC_2025/ACG_BC_2025_wgs.shp")  

acg_zv <- read_sf("shapefiles/ACG_Zonasdevida.gpkg")

acg_ecosystemas <- read_sf("shapefiles/EcosistemasACG_Atlas2008/") %>% 
  mutate(ecosystem_type = case_when(NOMBRE %in% c("BOSQUE SECO TROPICAL", "BOSQUE HUMEDO PREMONTANO TRANSICION A BASAL") ~ "Dry Forest",
                                    NOMBRE %in% c("BOSQUE PLUVIAL MONTANO BAJO", "BOSQUE PLUVIAL PREMONTANO" ) ~ "Cloud Forest",
                                    TRUE ~ "Rain Forest"))

## panel A 

acg_guanacaste <- ggplot() +
  geom_sf(data = CR_boundary_crop, fill = 'transparent') +
  geom_sf(data = Guanacaste_p, fill = 'grey') +
  geom_sf(data = acg_bc, fill = 'black') +
  geom_sf(data = acg_zv, fill = 'black') +
  #geom_sf(data = lat_lon_malaise, aes(color = Category)) +
  theme_cowplot() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        axis.line = element_blank(),
        legend.text = element_text(size = 11)) +
  annotation_scale()

north_arrow_plot <- ggplot()+ 
  annotation_north_arrow(location = 'tl', height = unit(0.7, 'cm'), width = unit(0.7, "cm")) +
  theme_cowplot()

acg_bc_to_use <- acg_bc %>% 
  st_filter(lat_lon_reared)

acg_latlong <- ggplot() +
  #geom_sf(data = Guanacaste_p, fill = 'transparent') +
  geom_sf(data = acg_bc_to_use, fill = '#B1E7CB') +
  #geom_sf(data = acg_zv) +
  geom_sf(data = acg_ecosystemas, aes(fill = ecosystem_type)) +
  geom_sf(data = filter(lat_lon_reared, lat > 10.5), aes(fill = 'black'), size = 0.7, shape = 16, color = 'black') +
  geom_sf(data = st_jitter(filter(lat_lon_malaise, Category == 'Peripherie'), factor = 0), aes(fill = 'blue'), size = 2, shape = 21, color = 'black') +
  geom_sf(data = st_jitter(filter(lat_lon_malaise, Category == 'Core'), factor = 0), aes(fill = 'red'), size = 2, shape = 21, color = 'black') +
  theme_cowplot() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        axis.line = element_blank(),
        legend.text = element_text(size = 11), 
        legend.position = 'bottom',
        legend.box = 'vertical') +
  annotation_scale() +
  #annotation_north_arrow(location = 'tl', height = unit(0.7, 'cm'), width = unit(0.7, "cm")) +
  scale_fill_discrete(breaks = c("black", 'blue', 'red',  "Dry Forest", "Cloud Forest","Rain Forest"),
    labels = c("Reared", "Peripheral\n traps", "Core\n traps", "Dry Forest","Cloud Forest", 
                                 "Rain Forest"), name = '', type = c('black','#E9DF00','#CAD4F7','#FFF1D6','#B1E7CB','#FB5012')) 
  #scale_size_area(breaks = c(1,50,100,150), limits = c(1,200), name = 'N. Bins')

acg_all <- plot_grid(acg_latlong,  acg_guanacaste, labels = c("A", "B"),  rel_widths = c(0.7, 0.3), nrow = 1)   


acg_all_final <- acg_all +
  annotation_north_arrow(location = 'tr', height = unit(0.7, 'cm'), width = unit(0.7, "cm")) 

ggsave(acg_all_final, filename = 'Figures/Figure1.jpeg') 

