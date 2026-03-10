## Range size estimates and maps for  Saturnidae
## in this version, (line 51 and line 209), species with fewer than 3 cases excluded 
## ACG saturnidae begins on line 24
## Global saturnidae records from gbif begins on line 181

#ACG data link: https://ln5.sync.com/dl/e4aea21b0/8z5snjmg-d4ewrbez-uzz3bnbm-7sgrv4cq
#Global GBIF link: https://ln5.sync.com/dl/3cca04c30/pnb2j7ky-ycrvgpbm-dejz8azu-7bcjr363 

rm(list=ls())

library(vegan)
library(ggordiplots)
library(viridis)
library(gridExtra)
library(betapart)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(sf)
library(rnaturalearth)
library(cowplot)



# dan and winnie data from our quicke et al caterpillar paper
# https://doi.org/10.5683/SP3/NX043G 81.1 MB download

dan_and_winnie_data <- read.csv(file = "TableS1.csv",head=TRUE, sep=",")

D_w_d = dan_and_winnie_data[, c("LEP.BIN", "Longitude", "Latitude", "Herbivore.family")]

D_w_d <- na.omit(D_w_d)

data_Saturniidae<- D_w_d[D_w_d$Herbivore.family == "Saturniidae", ]

data_Saturniidae2 <- rename(data_Saturniidae, uri = LEP.BIN, lat = Latitude, long = Longitude, family = Herbivore.family)
head(data_Saturniidae2)

df1_acg_Saturniidae= data_Saturniidae2[, c("uri", "long", "lat")]

#exclude cocos island species for spatial simplicity
# Select only the rows that are NOT in rows_to_remove
df1_acg_Saturniidae<- df1_acg_Saturniidae[!(df1_acg_Saturniidae$long > -82), ]

dim(df1_acg_Saturniidae)
## remove singletons and doubletons
df1_acg_Saturniidae=df1_acg_Saturniidae%>%
  # Count occurrences of each unique uri
  group_by(uri) %>%
  mutate(uri_acg_Saturniidaeount = n()) %>%
  # Filter to keep only rows where the uri appears more than once, or three or 10
  filter(uri_acg_Saturniidaeount > 3) %>%
  # remove the temporary count column
  select(-uri_acg_Saturniidaeount)
dim(df1_acg_Saturniidae)

df1_acg_Saturniidae<- na.omit(df1_acg_Saturniidae)


# 1. Convert the data frame to an sf object
# Define the coordinate reference system (CRS) as WGS 84 (EPSG:4326)
species_points_acg_Saturniidae<- st_as_sf(df1_acg_Saturniidae, coords = c("long", "lat"), crs = 4326)

# 2. Group by species and generate convex hulls (polygons) for each species
# The st_union operation combines points, and st_acg_Saturniidaeonvex_hull creates the polygon
species_ranges_acg_Saturniidae<- species_points_acg_Saturniidae%>%
  group_by(uri) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_convex_hull()

# 3. (Optional) Load world map for context
acg_map <- st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

# Using rnaturalearth for a quick boundary


cr_states <- ne_states(country = "Costa Rica", returnclass = "sf")

guanacaste_alajuela <- cr_states %>%
  filter(name %in% c("Guanacaste", "Alajuela"))

# 3. Filter for Guanacaste
#guanacaste <- cr_states[cr_states$name == "Guanacaste", ]



# 4. Plot the results
map_acg_SaturniidaeAN_Saturniidae= ggplot() +
  geom_sf(data = guanacaste_alajuela, fill = "white", color = "black") +
  geom_sf(data = species_ranges_acg_Saturniidae, aes(fill = uri), alpha = 0.5) + # Add the range polygons
  geom_sf(data = species_points_acg_Saturniidae, aes(color = uri), size = 1) +  # Add the original points
  theme_minimal() +
  labs(title = "acg_Saturniidaeae Range Maps")+
  theme(legend.position = "none")
map_acg_SaturniidaeAN_Saturniidae


# Reproject the resulting ranges to the projected CRS for accurate area calculation
species_ranges_acg_Saturniidae_projected <- st_transform(species_ranges_acg_Saturniidae, crs = 32617)

# Calculate area in square meters
species_ranges_acg_Saturniidae_projected$area_sq_m <- st_area(species_ranges_acg_Saturniidae_projected)

# Convert area to hectares (1 hectare = 10,000 square meters)
species_ranges_acg_Saturniidae_projected$area_hectares <- units::set_units(species_ranges_acg_Saturniidae_projected$area_sq_m, ha)

# View the results
print(species_ranges_acg_Saturniidae_projected[, c("uri", "area_hectares")])

ranges_acg_Saturniidae= as.data.frame(species_ranges_acg_Saturniidae_projected)

head(ranges_acg_Saturniidae)

ranges_acg_Saturniidae$area_hectares = as.numeric(ranges_acg_Saturniidae$area_hectares)

average_range_size = mean(ranges_acg_Saturniidae$area_hectares)
average_range_size
median_range_size = median(ranges_acg_Saturniidae$area_hectares)
median_range_size


options(scipen = 999)
boxplot_acg_SaturniidaeAN_Saturniidae= ggplot(ranges_acg_Saturniidae, aes(x = "", y = log(area_hectares))) +
  geom_boxplot(outlier.shape = NA) + # Hide default outliers
  geom_jitter(width = 0.2, alpha = 0.5) + # Add jittered points+
  labs(title = "acg_Saturniidaeae range size (ha)",
       y = "hectares",
       x = "acg_Saturniidaeae") 
boxplot_acg_SaturniidaeAN_Saturniidae

violin_acg_SaturniidaeAN_Saturniidae= ggplot(ranges_acg_Saturniidae, aes(x = "", y = log(area_hectares))) +
  # Add the violin plot (optional aesthetics like fill/alpha can be added)
  geom_violin(alpha = 0.5, fill = "lightblue") +
  # Add the boxplot, hiding its default outliers (outlier.shape = NA)
  # geom_boxplot(width = 0.2, outlier.shape = NA, fill = "white") +
  # Add jittered points for all data points within the plot area
  geom_jitter(width = 0.15, size = 1.5, alpha = 0.8) +
  # Add labels and theme
  labs(title = "acg_Saturniidaerange size (ha)",
       y = "hectares",
       x = "acg_Saturniidae") +
  theme_minimal()
violin_acg_SaturniidaeAN_Saturniidae


freq_acg_SaturniidaeAN_Saturniidae= ggplot(data = ranges_acg_Saturniidae, aes(x = area_hectares)) +
  geom_histogram(color = "black", fill = "skyblue") +
  labs(title = "Frequency Histogram of acg_Saturniidaeae BIN range size (ha)",
       x = "Range Size (ha)",
       y = "Frequency")
freq_acg_SaturniidaeAN_Saturniidae

mean_acg_SaturniidaeAN_Saturniidae= mean(ranges_acg_Saturniidae$area_hectares)
median_acg_SaturniidaeAN_Saturniidae= median(ranges_acg_Saturniidae$area_hectares)
mean_acg_SaturniidaeAN_Saturniidae
median_acg_SaturniidaeAN_Saturniidae

mean_label <- paste("Mean =", round(mean_acg_SaturniidaeAN_Saturniidae, 0))
median_label <- paste("Median =", round(median_acg_SaturniidaeAN_Saturniidae, 0))

# 3. Create the ggplot frequency distribution with line and text
freq_acg_SaturniidaeAN_Saturniidae_mean = ggplot(ranges_acg_Saturniidae, aes(x = area_hectares)) +
  geom_histogram(fill = "#0099F8", color = "#000000") + # Customize histogram appearance
  geom_vline(xintercept = mean_acg_SaturniidaeAN_Saturniidae, color = "red", linetype = "dashed", size = 1) + # Add a vertical line at the mean
  annotate("text", x = mean_acg_SaturniidaeAN_Saturniidae+ 5, y = 10, label = mean_label, color = "red", size = 4, fontface = "bold", hjust = -0.5) + # Add text annotation
  annotate("text", x = median_acg_SaturniidaeAN_Saturniidae+ 5, y = 8, label = median_label, color = "red", size = 4, fontface = "bold", hjust = -0.5) + # Add text annotation
  labs(title = "Freq Dist of acg_Saturniidaeae BIN range size (ha) w mean", x = "Range Size (ha)", y = "Frequency") + # Add labels and title
  theme_minimal() # Use a clean theme
freq_acg_SaturniidaeAN_Saturniidae_mean




plot_grid(map_acg_SaturniidaeAN_Saturniidae+ theme(legend.position="none"), boxplot_acg_SaturniidaeAN_Saturniidae, violin_acg_SaturniidaeAN_Saturniidae, freq_acg_SaturniidaeAN_Saturniidae_mean, labels = c('A', 'B', 'C', 'D'), label_size = 12, rel_widths = c(1, 1), align = "hv")

pdf("Guzman and Colwell acg_Saturniidaeae rangemap_plots_260121.pdf", width = 12, height = 8) 
plot_grid(map_acg_SaturniidaeAN_Saturniidae+ theme(legend.position="none"), boxplot_acg_SaturniidaeAN_Saturniidae, violin_acg_SaturniidaeAN_Saturniidae, freq_acg_SaturniidaeAN_Saturniidae_mean, labels = c('A', 'B', 'C', 'D'), label_size = 12, rel_widths = c(1, 1), align = "hv")
dev.off() # Close the file

##

# global saturnidae from GBIF
# GBIF.org (21 January 2026) GBIF Occurrence Download https://doi.org/10.15468/dl.qfcdsv

### Synch link to 20Mb file = https://ln5.sync.com/dl/3cca04c30/pnb2j7ky-ycrvgpbm-dejz8azu-7bcjr363

world_saturnidae2 = read.csv("global_saturndiae all from gbif_valid.csv", header = TRUE, sep = ",")
dim(world_saturnidae2)


#df1_WRLD_saturnidae2 = world_saturnidae2[, c("species", "long", "lat")]

#exclude cocos island species for spatial simplicity
# Select only the rows that are NOT in rows_to_remove

df1_WRLD_saturnidae2 <- world_saturnidae2 %>%
  # filter(verification == "Accepted - considered correct")
  filter(!verification %in% c("uncertain", "Unconfirmed", "Unconfirmed - not reviewed", "Unconfirmed - plausible", "Unvalidated", "unverified"))
df1_WRLD_saturnidae2

dim(df1_WRLD_saturnidae2)


## remove singletons and doubletons
df1_WRLD_saturnidae2=df1_WRLD_saturnidae2%>%
  # Count occurrences of each unique species
  group_by(species) %>%
  mutate(species_WRLD_saturnidae2ount = n()) %>%
  # Filter to keep only rows where the species appears more than once
  filter(species_WRLD_saturnidae2ount > 3) %>%
  # Optional: remove the temporary count column
  select(-species_WRLD_saturnidae2ount)
dim(df1_WRLD_saturnidae2)

df1_WRLD_saturnidae2 <- na.omit(df1_WRLD_saturnidae2)
dim(df1_WRLD_saturnidae2)

# 1. Convert the data frame to an sf object
# Define the coordinate reference system (CRS) as WGS 84 (EPSG:4326)
species_points_WRLD_saturnidae2<- st_as_sf(df1_WRLD_saturnidae2, coords = c("long", "lat"), crs = 4326)

# 2. Group by species and generate convex hulls (polygons) for each species
# The st_union operation combines points, and st_WRLD_saturnidae2onvex_hull creates the polygon
species_ranges_WRLD_saturnidae2 <- species_points_WRLD_saturnidae2%>%
  group_by(species) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_convex_hull()

# 3. (Optional) Load world map for context
world_map <- st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

# 4. Plot the results
map_WRLD_saturnidae2AN_saturnidae2 = ggplot() +
  geom_sf(data = world_map, fill = "white", color = "black") +
  geom_sf(data = species_ranges_WRLD_saturnidae2, aes(fill = species), alpha = 0.5) + # Add the range polygons
  geom_sf(data = species_points_WRLD_saturnidae2, aes(color = species), size = 1) +  # Add the original points
  theme_minimal() +
  labs(title = "world_saturnidae2 Range Maps")+
  theme(legend.position = "none")
map_WRLD_saturnidae2AN_saturnidae2


## test map wiht only one species

# luna moth - Actias luna is widespread Apanteles hemara
a_luna2 <- df1_WRLD_saturnidae2 %>%
  filter(species == "Actias luna")  # sharkey's problem bin BOLD:AAA7143
a_luna2 

species_points_a_luna2<- st_as_sf(a_luna2, coords = c("long", "lat"), crs = 4326)

# 2. Group by species and generate convex hulls (polygons) for each species
# The st_union operation combines points, and st_WRLD_saturnidae2onvex_hull creates the polygon
species_ranges_a_luna2 <- species_points_a_luna2%>%
  group_by(species) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_convex_hull()

map_WRLD_a_luna2  = ggplot() +
  geom_sf(data = world_map, fill = "white", color = "black") +
  geom_sf(data = species_ranges_a_luna2, aes(fill = species), alpha = 0.5) + # Add the range polygons
  geom_sf(data = species_points_a_luna2, aes(color = species), size = 1) +  # Add the original points
  theme_minimal() +
  labs(title = "A luna Range Map")+
  theme(legend.position = "none")
map_WRLD_a_luna2 

## still has luna sitings in south america, europe, africa and asia.  

pdf("Guzman and Colwell global_luna moth_260120.pdf", width = 12, height = 8) 
map_WRLD_a_luna2
dev.off() # Close the file


# Reproject the resulting ranges to the projected CRS for accurate area calculation
species_ranges_WRLD_saturnidae2_projected <- st_transform(species_ranges_WRLD_saturnidae2, crs = 32617)

# Calculate area in square meters
species_ranges_WRLD_saturnidae2_projected$area_sq_m <- st_area(species_ranges_WRLD_saturnidae2_projected)

# Convert area to hectares (1 hectare = 10,000 square meters)
species_ranges_WRLD_saturnidae2_projected$area_hectares <- units::set_units(species_ranges_WRLD_saturnidae2_projected$area_sq_m, ha)

# View the results
print(species_ranges_WRLD_saturnidae2_projected[, c("species", "area_hectares")])

ranges_WRLD_saturnidae2 = as.data.frame(species_ranges_WRLD_saturnidae2_projected)

head(ranges_WRLD_saturnidae2)

ranges_WRLD_saturnidae2$area_hectares = as.numeric(ranges_WRLD_saturnidae2$area_hectares)

average_range_size = mean(ranges_WRLD_saturnidae2$area_hectares)
average_range_size
median_range_size = median(ranges_WRLD_saturnidae2$area_hectares)
median_range_size

dim(ranges_WRLD_saturnidae2)



options(scipen = 999)
boxplot_WRLD_saturnidae2AN_saturnidae2 = ggplot(ranges_WRLD_saturnidae2, aes(x = "", y = log(area_hectares))) +
  geom_boxplot(outlier.shape = NA) + # Hide default outliers
  geom_jitter(width = 0.2, alpha = 0.5) + # Add jittered points+
  labs(title = "world_saturnidae2 range size (ha)",
       y = "hectares",
       x = "world_saturnidae2") 
boxplot_WRLD_saturnidae2AN_saturnidae2

violin_WRLD_saturnidae2AN_saturnidae2= ggplot(ranges_WRLD_saturnidae2, aes(x = "", y = log(area_hectares))) +
  # Add the violin plot (optional aesthetics like fill/alpha can be added)
  geom_violin(alpha = 0.5, fill = "lightblue") +
  # Add the boxplot, hiding its default outliers (outlier.shape = NA)
  # geom_boxplot(width = 0.2, outlier.shape = NA, fill = "white") +
  # Add jittered points for all data points within the plot area
  geom_jitter(width = 0.15, size = 1.5, alpha = 0.8) +
  # Add labels and theme
  labs(title = "world_saturnidae2 range size (ha)",
       y = "hectares",
       x = "world_saturnidae2") 
violin_WRLD_saturnidae2AN_saturnidae2

freq_WRLD_saturnidae2AN_saturnidae2 = ggplot(data = ranges_WRLD_saturnidae2, aes(x = area_hectares)) +
  geom_histogram(color = "black", fill = "skyblue") +
  labs(title = "Frequency Histogram of world_saturnidae2 BIN range size (ha)",
       x = "Range Size (ha)",
       y = "Frequency")
freq_WRLD_saturnidae2AN_saturnidae2

mean_WRLD_saturnidae2AN_saturnidae2 = mean(ranges_WRLD_saturnidae2$area_hectares)
median_WRLD_saturnidae2AN_saturnidae2 = median(ranges_WRLD_saturnidae2$area_hectares)
mean_WRLD_saturnidae2AN_saturnidae2
median_WRLD_saturnidae2AN_saturnidae2

mean_label <- paste("Mean =", round(mean_WRLD_saturnidae2AN_saturnidae2, 0))
median_label <- paste("Median =", round(median_WRLD_saturnidae2AN_saturnidae2, 0))

# 3. Create the ggplot frequency distribution with line and text
freq_WRLD_saturnidae2AN_saturnidae2_mean = ggplot(ranges_WRLD_saturnidae2, aes(x = area_hectares)) +
  geom_histogram(fill = "#0099F8", color = "#000000") + # Customize histogram appearance
  geom_vline(xintercept = mean_WRLD_saturnidae2AN_saturnidae2, color = "red", linetype = "dashed", size = 1) + # Add a vertical line at the mean
  annotate("text", x = mean_WRLD_saturnidae2AN_saturnidae2 + 5, y = 500, label = mean_label, color = "red", size = 4, fontface = "bold", hjust = -0.5) + # Add text annotation
  annotate("text", x = median_WRLD_saturnidae2AN_saturnidae2 + 5, y = 400, label = median_label, color = "red", size = 4, fontface = "bold", hjust = -0.5) + # Add text annotation
  labs(title = "Freq Dist of world_saturnidae2 BIN range size (ha) w mean", x = "Range Size (ha)", y = "Frequency") + # Add labels and title
  theme_minimal() # Use a clean theme
freq_WRLD_saturnidae2AN_saturnidae2_mean

library(cowplot)
plot_grid(map_WRLD_saturnidae2AN_saturnidae2 + theme(legend.position="none"), boxplot_WRLD_saturnidae2AN_saturnidae2, violin_WRLD_saturnidae2AN_saturnidae2, freq_WRLD_saturnidae2AN_saturnidae2_mean, labels = c('A', 'B', 'C', 'D'), label_size = 12, rel_widths = c(1, 1), align = "hv")

pdf("Guzman and Colwell world_saturnidae2 rangemap_plots_260120.pdf", width = 12, height = 8) 
plot_grid(map_WRLD_saturnidae2AN_saturnidae2 + theme(legend.position="none"), boxplot_WRLD_saturnidae2AN_saturnidae2, violin_WRLD_saturnidae2AN_saturnidae2, freq_WRLD_saturnidae2AN_saturnidae2_mean, labels = c('A', 'B', 'C', 'D'), label_size = 12, rel_widths = c(1, 1), align = "hv")
dev.off() # Close the file
