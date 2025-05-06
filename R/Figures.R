library(sf)
library(ggplot2)
library(cowplot)
library(dplyr)
library(ggspatial)
#remotes::install_github("wmgeolab/rgeoboundaries")
library(rgeoboundaries)

### read in points

malaise <- read.csv('Data/locations/Locations for map Figure 1.csv')

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

#### conservation areas ###

acg_bc <- read_sf("shapefiles/new/ACG_BC_2025/ACG_BC_2025_wgs.shp")  

acg_zv <- read_sf("shapefiles/ACG_Zonasdevida.gpkg")

Guanacaste_p <- read_sf("shapefiles/Guanacaste_NicoyaPeninsula_boundaries.shp") 


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
  annotation_scale() +
  annotation_north_arrow(location = 'tl', height = unit(0.7, 'cm'), width = unit(0.7, "cm")) 


lat_lon_con <- lat_lon_malaise %>% 
  st_join(acg)

con_areas3 <- con_areas %>% 
  st_filter(lat_lon_malaise)
st_union()

lat_lon_reared %>%
  filter(lat >10.5)

acg_latlong <- ggplot() +
  #geom_sf(data = Guanacaste_p, fill = 'transparent') +
  geom_sf(data = acg_bc, fill = 'black') +
  geom_sf(data = acg_zv) +
  geom_sf(data = st_jitter(lat_lon_malaise, factor = 0.01), aes(color = Category, size = `BIN..`)) +
  geom_sf(data = filter(lat_lon_reared, lat > 10.5), aes(color = 'green'), size = 0.5) +
  theme_cowplot() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        axis.line = element_blank(),
        legend.text = element_text(size = 11), 
        legend.position = 'bottom',
        legend.box = 'vertical') +
  #annotation_scale() +
  #annotation_north_arrow(location = 'tl', height = unit(0.7, 'cm'), width = unit(0.7, "cm")) +
  scale_color_discrete(labels = c("Core\n traps", "Reared", "Peripheral\n traps"), name = '') +
  scale_size_area(breaks = c(1,50,100,150), limits = c(1,200), name = 'N. Bins')

acg_all <- plot_grid(acg_guanacaste,acg_latlong, labels = c("A.", "B."))   

ggsave(acg_all, filename = 'Figures/Figure1.jpeg') 


