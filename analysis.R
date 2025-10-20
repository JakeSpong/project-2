usethis::use_git()

## Setup ----
# Load libraries
library(nlme)
library(dplyr) #for manipulating data
library(ggplot2) #for plotting graphs
library(ggpubr)
library(vegan) #for NMDS
library(tidyverse)
library(car) #for levene test
library(dataspice) #for creating metadata files
library(multcompView) #for significant difference letters
library(gitcreds)
library(broom)
library('FactoMineR') #for PCA visualisation

#### NEw Mapping of Sample Locations ----
# library(ggplot2)
# library(rnaturalearth)
# library(rnaturalearthdata)
# library(sf)
# library(ggmap)
# 
# #load the field data
# all_data <- readr::read_csv(
#   here::here("data", "all-sites_field-data.csv"), show_col_types = FALSE
# ) 
# 
# #extract the sample coordinates from the dataframe
# sample_coordinates <- data.frame(
#   Site = all_data$Site,
#   Vegetation = all_data$Vegetation,
#   Longitude = all_data$LongitudeE,
#   Latitude = all_data$LatitudeN
# )
# #the sample coordinates belonging to each site
# bridestones <- sample_coordinates[1:20, ]
# scarthwood <- sample_coordinates[21:40, ]
# brimham <- sample_coordinates[41:60, ]
# haweswater <- sample_coordinates[61:80,]
# whiteside <- sample_coordinates[81:100, ]
# widdybanks <- sample_coordinates[101:120,]
# 
# # Calculate the bounding box surrounding the 30 sample coordinates
# bridestones_longitude_range_sample <- range(bridestones$Longitude)
# bridestones_latitude_range_sample <- range(bridestones$Latitude)
# # Calculate the bounding box surrounding the 30 sample coordinates
# scarthwood_longitude_range_sample <- range(scarthwood$Longitude)
# scarthwood_latitude_range_sample <- range(scarthwood$Latitude)
# # Calculate the bounding box surrounding the 30 sample coordinates
# brimham_longitude_range_sample <- range(brimham$Longitude)
# brimham_latitude_range_sample <- range(brimham$Latitude)
# # Calculate the bounding box surrounding the 30 sample coordinates
# haweswater_longitude_range_sample <- range(haweswater$Longitude)
# haweswater_latitude_range_sample <- range(haweswater$Latitude)
# # Calculate the bounding box surrounding the 30 sample coordinates
# whiteside_longitude_range_sample <- range(whiteside$Longitude)
# whiteside_latitude_range_sample <- range(whiteside$Latitude)
# # Calculate the bounding box surrounding the 30 sample coordinates
# widdybanks_longitude_range_sample <- range(widdybanks$Longitude)
# widdybanks_latitude_range_sample <- range(widdybanks$Latitude)
# 
# 
# # Register your Google API key
# register_google(key = "AIzaSyA42ZmX_RGv9dTA7PFy4UR0YIjxW3x0rUE")
# 
# # Load the UK map from the rnaturalearth package
# uk_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
#   filter(name == "United Kingdom")
# 
# # Plot the UK map with only the bounding box
# UK_Sites <- ggplot(data = uk_map) +
#   geom_sf(fill = "lightblue") +  # UK map with light blue fill
#   geom_rect(
#     aes(xmin = bridestones_longitude_range_sample[1],
#         xmax = bridestones_longitude_range_sample[2],
#         ymin = bridestones_latitude_range_sample[1],
#         ymax = bridestones_latitude_range_sample[2]
#         ), 
#     color = "red", fill = NA, size = 2) +  # Add bounding box around the points
#   geom_rect(
#     aes(xmin = scarthwood_longitude_range_sample[1],
#         xmax = scarthwood_longitude_range_sample[2],
#         ymin = scarthwood_latitude_range_sample[1],
#         ymax = scarthwood_latitude_range_sample[2]
#     ), 
#     color = "red", fill = NA, size = 2) +
#   geom_rect(
#     aes(xmin = brimham_longitude_range_sample[1],
#         xmax = brimham_longitude_range_sample[2],
#         ymin = brimham_latitude_range_sample[1],
#         ymax = brimham_latitude_range_sample[2]
#     ), 
#     color = "red", fill = NA, size = 2) +
#   geom_rect(
#     aes(xmin = widdybanks_longitude_range_sample[1],
#         xmax = widdybanks_longitude_range_sample[2],
#         ymin = widdybanks_latitude_range_sample[1],
#         ymax = widdybanks_latitude_range_sample[2]
#     ), 
#     color = "red", fill = NA, size = 2) +
#   geom_rect(
#     aes(xmin = haweswater_longitude_range_sample[1],
#         xmax = haweswater_longitude_range_sample[2],
#         ymin = haweswater_latitude_range_sample[1],
#         ymax = haweswater_latitude_range_sample[2]
#     ), 
#     color = "red", fill = NA, size = 2) +
#   geom_rect(
#     aes(xmin = whiteside_longitude_range_sample[1],
#         xmax = whiteside_longitude_range_sample[2],
#         ymin = whiteside_latitude_range_sample[1],
#         ymax = whiteside_latitude_range_sample[2]
#     ), 
#     color = "red", fill = NA, size = 2) +
#   theme_minimal()
# #show the map
# show(UK_Sites)
# #save the UK map with Haweswater highlighted
# ggsave("figures/UK_Sites_boundingbox.svg", width = 8, height = 6)
# 
# 
# #map the sample coordinates at each site
# #bridestones
# # Get the Google Satellite map for the zoomed-in area based on the 30 sample coordinates
# bridestones_satellite <- get_map(location = c(lon = mean(bridestones$Longitude), 
#                                           lat = mean(bridestones$Latitude)),
#                              zoom = 15,  # Adjust zoom level for the desired zoom
#                              maptype = "satellite",  # Use the "satellite" map type
#                              source = "google")
# #plot the map
# bridestones_map <- ggmap(bridestones_satellite) +
#   geom_point(data = bridestones, aes(x = Longitude, y = Latitude, 
#                                  color = Vegetation, shape = Vegetation), size = 2) +
#   scale_color_manual(values = c("Bracken" = "green", "Heather" = "purple")) +  # Color by habitat
#   scale_shape_manual(values = c("Bracken" = 17, "Heather" = 16)) +  # Shape by site
#   theme_minimal() + 
#   theme(legend.position = "right") 
# #show the map
# show(bridestones_map)
# # Save the final map to a file
# ggsave("figures/bridestones_map.svg", plot = last_plot(), width = 7.5, height = 6, dpi = 300)
# 
# #scarthwood
# # Get the Google Satellite map for the zoomed-in area based on the 30 sample coordinates
# scarthwood_satellite <- get_map(location = c(lon = mean(scarthwood$Longitude), 
#                                               lat = mean(scarthwood$Latitude)),
#                                  zoom = 15,  # Adjust zoom level for the desired zoom
#                                  maptype = "satellite",  # Use the "satellite" map type
#                                  source = "google")
# #plot the map
# scarthwood_map <- ggmap(scarthwood_satellite) +
#   geom_point(data = scarthwood, aes(x = Longitude, y = Latitude, 
#                                      color = Vegetation, shape = Vegetation), size = 2) +
#   scale_color_manual(values = c("Bracken" = "green", "Heather" = "purple")) +  # Color by habitat
#   scale_shape_manual(values = c("Bracken" = 17, "Heather" = 16)) +  # Shape by site
#   theme_minimal() + 
#   theme(legend.position = "right") 
# #show the map
# show(scarthwood_map)
# # Save the final map to a file
# ggsave("figures/scarthwood.svg", plot = last_plot(), width = 7.5, height = 6, dpi = 300)
# 
# #brimham 
# # Get the Google Satellite map for the zoomed-in area based on the 30 sample coordinates
# brimham_satellite <- get_map(location = c(lon = mean(brimham$Longitude), 
#                                       lat = mean(brimham$Latitude)),
#                          zoom = 15,  # Adjust zoom level for the desired zoom
#                          maptype = "satellite",  # Use the "satellite" map type
#                          source = "google")
#   #plot the map
# brimham_map <- ggmap(brimham_satellite) +
#   geom_point(data = brimham, aes(x = Longitude, y = Latitude, 
#                                             color = Vegetation, shape = Vegetation), size = 2) +
#   scale_color_manual(values = c("Bracken" = "green", "Heather" = "purple")) +  # Color by habitat
#   scale_shape_manual(values = c("Bracken" = 17, "Heather" = 16)) +  # Shape by site
#   theme_minimal() + 
#   theme(legend.position = "right") 
# #show the map
# #show(brimham_map)
# # Save the final map to a file
# ggsave("figures/brimham_map.svg", plot = last_plot(), width = 7.5, height = 6, dpi = 300)
# 
# #widdybanks
# # Get the Google Satellite map for the zoomed-in area based on the 30 sample coordinates
# widdybanks_satellite <- get_map(location = c(lon = mean(widdybanks$Longitude), 
#                                           lat = mean(widdybanks$Latitude)),
#                              zoom = 17,  # Adjust zoom level for the desired zoom
#                              maptype = "satellite",  # Use the "satellite" map type
#                              source = "google")
# #plot the map
# widdybanks_map <- ggmap(widdybanks_satellite) +
#   geom_point(data = widdybanks, aes(x = Longitude, y = Latitude, 
#                                  color = Vegetation, shape = Vegetation), size = 2) +
#   scale_color_manual(values = c("Bracken" = "green", "Heather" = "purple")) +  # Color by habitat
#   scale_shape_manual(values = c("Bracken" = 17, "Heather" = 16)) +  # Shape by site
#   theme_minimal() + 
#   theme(legend.position = "right") 
# #show the map
# show(widdybanks_map)
# # Save the final map to a file
# ggsave("figures/widdybanks_map.svg", plot = last_plot(), width = 7.5, height = 6, dpi = 300)
# 
# #Haweswater
# # Get the Google Satellite map for the zoomed-in area based on the 30 sample coordinates
# haweswater_satellite <- get_map(location = c(lon = mean(haweswater$Longitude), 
#                                              lat = mean(haweswater$Latitude)),
#                                 zoom = 15,  # Adjust zoom level for the desired zoom
#                                 maptype = "satellite",  # Use the "satellite" map type
#                                 source = "google")
# #plot the map
# haweswater_map <- ggmap(haweswater_satellite) +
#   geom_point(data = haweswater, aes(x = Longitude, y = Latitude, 
#                                     color = Vegetation, shape = Vegetation), size = 2) +
#   scale_color_manual(values = c("Bracken" = "green", "Heather" = "purple")) +  # Color by habitat
#   scale_shape_manual(values = c("Bracken" = 17, "Heather" = 16)) +  # Shape by site
#   theme_minimal() + 
#   theme(legend.position = "right") 
# #show the map
# show(haweswater_map)
# # Save the final map to a file
# ggsave("figures/haweswater_map.svg", plot = last_plot(), width = 7.5, height = 6, dpi = 300)
# 
# #Whiteside
# # Get the Google Satellite map for the zoomed-in area based on the 30 sample coordinates
# whiteside_satellite <- get_map(location = c(lon = mean(whiteside$Longitude), 
#                                              lat = mean(whiteside$Latitude)),
#                                 zoom = 17,  # Adjust zoom level for the desired zoom
#                                 maptype = "satellite",  # Use the "satellite" map type
#                                 source = "google")
# #plot the map
# whiteside_map <- ggmap(whiteside_satellite) +
#   geom_point(data = whiteside, aes(x = Longitude, y = Latitude, 
#                                     color = Vegetation, shape = Vegetation), size = 2) +
#   scale_color_manual(values = c("Bracken" = "green", "Heather" = "purple")) +  # Color by habitat
#   scale_shape_manual(values = c("Bracken" = 17, "Heather" = 16)) +  # Shape by site
#   theme_minimal() + 
#   theme(legend.position = "right") 
# #show the map
# show(whiteside_map)
# # Save the final map to a file
# ggsave("figures/whiteside_map.svg", plot = last_plot(), width = 7.5, height = 6, dpi = 300)
# 
# 
# #combine the two maps into a single figure
# maps_figure <- ggarrange(bridestones_map, scarthwood_map, brimham_map,                      widdybanks_map, haweswater_map, whiteside_map,
#                          labels = c("A", "B", "C", "D", "E", "F"),
#                          ncol = 2, nrow = 3,
#                          #the width of each panel of the multifigure plot
#                          widths = c(7,7),
#                          common.legend = TRUE
# )
# #show the plot in the Plots window
# show(maps_figure)
# #save the figure
# #ggsave("figures/Maps_panel_figure.svg", plot = last_plot(), width = 7.5, height = 6, dpi = 300)
# 


#### Get average annaul rainfall at our sample locations ----
library(ncdf4)
library(terra)
library(raster)
# Open the NetCDF file
nc_file <- nc_open("C:/Users/jakef/Documents/York/Project 2 Analysis/project-2/data/CEH_GEAR_monthly_GB_2019.nc")
# Open the NetCDF file
nc <- nc_open(nc_file)

# Check the variables and dimensions in the NetCDF file
print(nc$var)
print(nc$dim)


# Example coordinates (replace these with your specific coordinates)
latitudes <- c(51.5074, 52.2053, 53.4084, 54.9784, 55.3781, 50.8503)  # Example latitudes (in degrees)
longitudes <- c(-0.1278, 0.1218, -2.9916, -1.6174, -3.4360, -4.3517)  # Example longitudes (in degrees)

# extract the variable "rainfall_amount"
rainfall_data <- ncvar_get(nc_file, "rainfall_amount")

# Get latitude and longitude arrays
latitudes <- ncvar_get(nc_file, "lat")
longitudes <- ncvar_get(nc_file, "lon")

# Example coordinates (replace with your actual coordinates)
coords <- cbind(c(-0.1278, 0.1218, -2.9916, -1.6174, -3.4360, -4.3517), 
                c(51.5074, 52.2053, 53.4084, 54.9784, 55.3781, 50.8503))

# Convert latitude and longitude coordinates to indices
# (This is an approximation. You may need to refine this step depending on your data.)
latitude_indices <- match(coords[,2], latitudes)
longitude_indices <- match(coords[,1], longitudes)

# Extract the rainfall values for these coordinates (this is just for one time point, adjust as needed)
rainfall_values <- rainfall_data[longitude_indices, latitude_indices]

# Print the extracted rainfall values
print(rainfall_values)

# Close the NetCDF file
nc_close(nc)


# Create a RasterBrick of the rainfall data
rainfall_brick <- brick(rainfall_var)

# Set up the coordinate system for the raster (assuming lat and lon are the coordinates)
extent(rainfall_brick) <- c(min(lon), max(lon), min(lat), max(lat))
crs(rainfall_brick) <- CRS("+proj=longlat +datum=WGS84")

# Loop through each coordinate and extract the average rainfall
rainfall_values <- sapply(1:length(latitudes), function(i) {
  # Find the closest point
  point <- SpatialPoints(cbind(longitudes[i], latitudes[i]), proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Extract the rainfall value for the closest grid point
  value <- extract(rainfall_brick, point)
  
  # Return the extracted value
  return(value)
})

# Print extracted rainfall values
print(rainfall_values)



print("haha")

#### Alpha diversity analysis of vegetation survey data ----
#### Load vegetation abundance data
d <- readr::read_csv(
  here::here("data", "all-sites_field-data.csv"), show_col_types = FALSE
) 
#add in site names
d$Site <- c(rep("Bridestones",20), rep("Scarth Wood Moor",20), rep("Brimham Moor",20), rep("Haweswater",20), rep("Whiteside", 20), rep("Widdybanks",20))
#order samples by bracken or heather
#d <- arrange(d, d["Vegetation"])
#ensure d is a dataframe
d <- as.data.frame(d)
#reorder the sites so they show up on the plot from west (LHS) to east (RHS)
d$Site <- factor(d$Site, levels = c("Whiteside", "Haweswater", "Widdybanks", "Brimham Moor", "Scarth Wood Moor", "Bridestones"))
#replace null (empty excell cell) with "0"
d[is.na(d)] <- 0
#create a subset containing only species abundance values
#just the species counts
spe <- d[,(7:21)]

#species richness
d$richness <- apply(spe[,]>0,1,sum)
#calculate diversity
d$shannon <- diversity(spe[,], "shannon")
d$simpson <- diversity(spe[,], "simpson")
d$evenness <- d$shannon / log(d$richness)


#boxplot of grassland vs heathland, species richness
richness_bxp <- ggboxplot(d, x = "Site", y = "richness", color = "Vegetation", ylab = "Species Richness", palette = c("limegreen", "#AA4499"), lwd = 0.75) + theme(
  #remove x axis label
  axis.title.x=element_blank(),
  axis.text.x=element_blank(),
  axis.ticks.x=element_blank(),
  # Remove panel border
  panel.border = element_blank(),  
  # Remove panel grid lines
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Remove panel background
  panel.background = element_blank(),
  # Add axis line
  axis.line = element_line(colour = "black", linewidth = 0.5),
  #change colour and thickness of axis ticks
  axis.ticks = element_line(colour = "black", linewidth = 0.5),
  #change axis labels colour
  axis.title.y = element_text(colour = "black"),
  #change tick labels colour
  axis.text.y = element_text(colour = "black"),
) 
#graphing boxplots with bracken split from nonbracken
shannon_bxp <-ggboxplot(d, x = "Site", y = "shannon", color = "Vegetation", ylab = "Shannon Diversity", palette = c("limegreen", "#AA4499"), lwd = 0.75) + theme(
  #remove x axis label
  axis.title.x=element_blank(),
  # Remove panel border
  panel.border = element_blank(),  
  # Remove panel grid lines
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Remove panel background
  panel.background = element_blank(),
  # Add axis line
  axis.line = element_line(colour = "black", linewidth = 0.5),
  #change colour and thickness of axis ticks
  axis.ticks = element_line(colour = "black", linewidth = 0.5),
  #change axis labels colour
  axis.title.y = element_text(colour = "black"),
  #change tick labels colour
  axis.text.y = element_text(colour = "black"),
) 
#graphing boxplots with bracken split from nonbracken
simpson_bxp <- ggboxplot(d, x = "Site", y = "simpson", color = "Vegetation", ylab = "Simpson Diversity", palette = c("limegreen", "#AA4499"), lwd = 0.75) + theme(
  #remove x axis label
  axis.title.x=element_blank(),
  # Remove panel border
  panel.border = element_blank(),  
  # Remove panel grid lines
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Remove panel background
  panel.background = element_blank(),
  # Add axis line
  axis.line = element_line(colour = "black", linewidth = 0.5),
  #change colour and thickness of axis ticks
  axis.ticks = element_line(colour = "black", linewidth = 0.5),
  #change axis labels colour
  axis.title.y = element_text(colour = "black"),
  #change tick labels colour
  axis.text.y = element_text(colour = "black"),
) 
#boxplot of grassland vs heathland, species richness
evenness_bxp <- ggboxplot(d, x = "Site", y = "evenness", color = "Vegetation", ylab = "Pielou's Evenness", palette = c("limegreen", "#AA4499"), lwd = 0.75) + theme(
  #remove x axis label
  axis.title.x=element_blank(),
  axis.text.x=element_blank(),
  axis.ticks.x=element_blank(),
  # Remove panel border
  panel.border = element_blank(),  
  # Remove panel grid lines
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Remove panel background
  panel.background = element_blank(),
  # Add axis line
  axis.line = element_line(colour = "black", linewidth = 0.5),
  #change colour and thickness of axis ticks
  axis.ticks = element_line(colour = "black", linewidth = 0.5),
  #change axis labels colour
  axis.title.y = element_text(colour = "black"),
  #change tick labels colour
  axis.text.y = element_text(colour = "black"),
) 

#three sites have only 1 species, meaning the eveness is NaN.  Do we make this 0, or omit as the code is currently doing?
all_bxp <- ggarrange(richness_bxp, evenness_bxp, shannon_bxp, simpson_bxp, 
                     labels = c("A", "B", "C", "D"),
                     ncol = 2, nrow = 2,
                     common.legend = TRUE)
show(all_bxp)

#### Vegetation richness ANOVAs ----
hist(d$richness)
#Type 1 two-way anova using data from all sites
anova <- aov(d$richness ~ d$Vegetation*d$Site)
#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$richness ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)


#now analyse at each site

#Bridestones
bri <- d[(1:20),]
#Type 1 two-way anova using data from all sites
anova <- aov(bri$richness ~ bri$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Scarth Wood Moor
swm <- d[(21:40),]
#Type 1 two-way anova using data from all sites
anova <- aov(swm$richness ~ swm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Brimham
bhm <- d[(41:60),]
#Type 1 two-way anova using data from all sites
anova <- aov(bhm$richness ~ bhm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Widdybanks
wdy <- d[(101:120),]
#Type 1 two-way anova using data from all sites
anova <- aov(wdy$richness ~ wdy$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Haweswater
haw <- d[(61:80),]
#Type 1 two-way anova using data from all sites
anova <- aov(haw$richness ~ haw$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Whiteside
whi <- d[(81:100),]
#Type 1 two-way anova using data from all sites
anova <- aov(whi$richness ~ whi$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)


#### Vegetation evenness ANOVAs----
hist(d$evenness)

#Type 1 two-way anova using data from all sites
anova <- aov(d$evenness ~ d$Vegetation*d$Site)
#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$evenness ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)


#now analyse at each site

#Bridestones
bri <- d[(1:20),]
#Type 1 two-way anova using data from all sites
anova <- aov(bri$evenness ~ bri$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Scarth Wood Moor
swm <- d[(21:40),]
#Type 1 two-way anova using data from all sites
anova <- aov(swm$evenness ~ swm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Brimham
bhm <- d[(41:60),]
#Type 1 two-way anova using data from all sites
anova <- aov(bhm$evenness ~ bhm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Widdybanks
wdy <- d[(101:120),]
#Type 1 two-way anova using data from all sites
anova <- aov(wdy$evenness ~ wdy$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Haweswater
haw <- d[(61:80),]
#Type 1 two-way anova using data from all sites
anova <- aov(haw$evenness ~ haw$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Whiteside
whi <- d[(81:100),]
#Type 1 two-way anova using data from all sites
anova <- aov(whi$evenness ~ whi$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)


#### Veg shannon ANOVAs----
hist(d$shannon)

#Type 1 two-way anova using data from all sites
anova <- aov(d$shannon ~ d$Vegetation*d$Site)

#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$shannon ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)


summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)

#now analyse at each site

#Bridestones
bri <- d[(1:20),]
#Type 1 two-way anova using data from all sites
anova <- aov(bri$shannon ~ bri$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Scarth Wood Moor
swm <- d[(21:40),]
#Type 1 two-way anova using data from all sites
anova <- aov(swm$shannon ~ swm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Brimham
bhm <- d[(41:60),]
#Type 1 two-way anova using data from all sites
anova <- aov(bhm$shannon ~ bhm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Widdybanks
wdy <- d[(101:120),]
#Type 1 two-way anova using data from all sites
anova <- aov(wdy$shannon ~ wdy$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Haweswater
haw <- d[(61:80),]
#Type 1 two-way anova using data from all sites
anova <- aov(haw$shannon ~ haw$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Whiteside
whi <- d[(81:100),]
#Type 1 two-way anova using data from all sites
anova <- aov(whi$shannon ~ whi$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)

#### Veg simpson ANOVAs----
hist(d$simpson)

#Type 1 two-way anova using data from all sites
anova <- aov(d$simpson ~ d$Vegetation*d$Site)
#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$simpson ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)


summary(anova)


#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)

#now analyse at each site

#Bridestones
bri <- d[(1:20),]
#Type 1 two-way anova using data from all sites
anova <- aov(bri$simpson ~ bri$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Scarth Wood Moor
swm <- d[(21:40),]
#Type 1 two-way anova using data from all sites
anova <- aov(swm$simpson ~ swm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Brimham
bhm <- d[(41:60),]
#Type 1 two-way anova using data from all sites
anova <- aov(bhm$simpson ~ bhm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Widdybanks
wdy <- d[(101:120),]
#Type 1 two-way anova using data from all sites
anova <- aov(wdy$simpson ~ wdy$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Haweswater
haw <- d[(61:80),]
#Type 1 two-way anova using data from all sites
anova <- aov(haw$simpson ~ haw$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Whiteside
whi <- d[(81:100),]
#Type 1 two-way anova using data from all sites
anova <- aov(whi$simpson ~ whi$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)

#### Beta diversity analysis of vegetation survey data ----
#### Load vegetation abundance data
d <- readr::read_csv(
  here::here("data", "all-sites_field-data.csv"), show_col_types = FALSE) 
#ensure d is a dataframe
d <- as.data.frame(d)
#order samples by veg
d <- arrange(d, d["Vegetation"])
#replace row index with sample names
rownames(d) <- c(d$SampleID)
#replace all NAs with 0s
d[is.na(d)] <- 0
#make sure our variables are coded as factors
d$Vegetation <- factor(d$Vegetation, levels = c("Bracken", "Heather"), labels = c("Bracken", "Heather"))
#just the species counts
spe <- d[,(7:21)]

#k is the number of reduced dimensions
#trymax sets the default number of iterations
example_NMDS <- metaMDS(spe, distance = "bray", k = 2, maxit = 999, trymax = 500)
#Shephard plot shows scatter around the regession between the interpoint distances in the final configuration (i.e. the distances between each pair of communities) against their original dissimilarities.  Large scatter around the line suggests the original dissimilarities are not well preserved in the reduced number of dimensions
stressplot(example_NMDS)
#set dimensions of new graphics window
#dev.new(width = 719, height = 412, unit = "px")
#plot the NMDS
plot(example_NMDS, col = "white")
#assign the treatments to relevant rows of the dataframe
treat=c(rep("Bracken",60),rep("Heather",60))
#set the colour for each treatment
colors=c(rep("#117733",60), rep("#AA4499", 60))
text(-1,2, paste("Stress = ", round(example_NMDS$stress, 3)))

for(i in unique(treat)) {
  #we have added an if statement so we can chose which points and ellipses to plot at a time e.g. i == "Grassland Bracken".  If we want to plot all ellipses simultaneously, set i == i
  if(i == i){
    #change the colour of each site name so samples from the same treatment have the same colour
    orditorp(example_NMDS$point[grep(i,treat),],display="sites", col=colors[grep(i,treat)], cex=0.7,air=0.01)
    #plots ellipse with ellipse centered on the centroid of the samples from the same treatment (and thus encapsulating 95% of the variance)
    ordiellipse(example_NMDS$point[grep(i,treat),],draw="polygon",
                groups=treat[treat==i],col=colors[grep(i,treat)],label=F) } }
#specify legend manually
legend(1.25,-0.65, legend = c("Bracken", "Heather"), fill = c("#117733",  "#AA4499"))

#save the file using Export -> Save As Image -> Width = 655, Height = 500 

#data frame containing the independent variables (Site, Vegetation) we shall be using in our PERMANOVA
idvs <- d[,c(2,5)]
#run the permanova
veg_permanova <- adonis2(spe ~ Site*Vegetation, idvs, permutations = 999, method = "bray", by = "terms")
veg_permanova
#veg_permanova indicates that Habitat and Vegetation have significant effects, with habitat explainng 26.7% of the variation and Vegetation explaining 36.0 %

#run an anosim - when grouping by habitat
ano = anosim(as.matrix(spe), grouping = idvs$Site, permutations = 9999, distance = "bray")
#check output of anosim
ano
#run an anosim - when grouping by vegetation
ano = anosim(as.matrix(spe), grouping = idvs$Vegetation, permutations = 9999, distance = "bray")
#check output of anosim
ano







#### Soil pH analysis ----
d <- readr::read_csv(
  here::here("data-raw", "project-2-data-master", "individual", "2) Soil pH.csv")
)
#reorder sites
d$Site <- factor(d$Site, levels = c("Whiteside", "Haweswater", "Widdybanks", "Brimham Moor", "Scarth Wood Moor", "Bridestones"))


#boxplot the data. Use aes() with backticks (``) so avoid an error with our column name
pH_bxp <- ggboxplot(d, x = "Site", aes(y = `pH`), color = "Vegetation", palette = c("limegreen", "#AA4499"), lwd = 0.75)  + 
  labs(x = "Site",
       y = "pH") + theme(
         # Remove panel border
         panel.border = element_blank(),  
         # Remove panel grid lines
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         # Remove panel background
         panel.background = element_blank(),
         # Add axis line
         axis.line = element_line(colour = "black", linewidth = 0.5),
         #change colour and thickness of axis ticks
         axis.ticks = element_line(colour = "black", linewidth = 0.5),
         #change axis labels colour
         axis.title.x = element_text(colour = "black"),
         axis.title.y = element_text(colour = "black"),
         #change tick labels colour
         axis.text.x = element_text(colour = "black"),
         axis.text.y = element_text(colour = "black"),
       ) 

show(pH_bxp)  
#save our plot
ggsave(path = "figures", paste0(Sys.Date(), "_pH.svg"), width = 10, height= 5, pH_bxp)

#Type 1 two-way anova using data from all sites
anova <- aov(d$`pH` ~ d$Vegetation*d$Site)
#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$pH ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
print(cld)


#now analyse at each site

#Bridestones
bri <- d[(101:120),]
#Type 1 two-way anova using data from all sites
anova <- aov(bri$pH ~ bri$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Scarth Wood Moor
swm <- d[(81:100),]
#Type 1 two-way anova using data from all sites
anova <- aov(swm$pH ~ swm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Brimham
bhm <- d[(61:80),]
#Type 1 two-way anova using data from all sites
anova <- aov(bhm$pH ~ bhm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Widdybanks
wdy <- d[(41:60),]
#Type 1 two-way anova using data from all sites
anova <- aov(wdy$pH ~ wdy$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Haweswater
haw <- d[(21:40),]
#Type 1 two-way anova using data from all sites
anova <- aov(haw$pH ~ haw$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Whiteside
whi <- d[(1:20),]
#Type 1 two-way anova using data from all sites
anova <- aov(whi$pH ~ whi$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)

#### Soil Moisture analysis ----
#read in the data
d <- readr::read_csv(
  here::here("data-raw", "project-2-data-master", "individual", "1) Soil Moisture Content.csv")
) 
#order the sites as they should appear on the graph from west to east
d$Site <- factor(d$Site, levels = c("Whiteside", "Haweswater", "Widdybanks", "Brimham Moor", "Scarth Wood Moor", "Bridestones"))

#boxplot the data. Use aes() with backticks (``) so avoid an error with our column name
gsmc_bxp <- ggboxplot(d, x = "Site", aes(y = `Soil Moisture (% fresh soil mass)`), color = "Vegetation", palette = c("limegreen", "#AA4499"), lwd = 0.75)  +
  labs(x = "Site",
       y = "Water Content (%)") + theme(
         # Remove panel border
         panel.border = element_blank(),  
         # Remove panel grid lines
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         # Remove panel background
         panel.background = element_blank(),
         # Add axis line
         axis.line = element_line(colour = "black", linewidth = 0.5),
         #change colour and thickness of axis ticks
         axis.ticks = element_line(colour = "black", linewidth = 0.5),
         #change axis labels colour
         axis.title.x = element_text(colour = "black"),
         axis.title.y = element_text(colour = "black"),
         #change tick labels colour
         axis.text.x = element_text(colour = "black"),
         axis.text.y = element_text(colour = "black"),
       ) 

show(gsmc_bxp)  
#save our plot
ggsave(path = "figures", paste0(Sys.Date(), "_water-content.svg"), width = 10, height= 5, gsmc_bxp)


#Type 1 two-way anova using data from all sites
anova <- aov(d$`Soil Moisture (% fresh soil mass)` ~ d$Vegetation*d$Site)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
print(cld)

#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$`Soil Moisture (% fresh soil mass)` ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

#now analyse at each site

#Bridestones
bri <- d[(1:20),]
#Type 1 two-way anova using data from all sites
anova <- aov(bri$`Soil Moisture (% fresh soil mass)` ~ bri$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Scarth Wood Moor
swm <- d[(21:40),]
#Type 1 two-way anova using data from all sites
anova <- aov(swm$`Soil Moisture (% fresh soil mass)` ~ swm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Brimham
bhm <- d[(41:60),]
#Type 1 two-way anova using data from all sites
anova <- aov(bhm$`Soil Moisture (% fresh soil mass)` ~ bhm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Widdybanks
wdy <- d[(61:80),]
#Type 1 two-way anova using data from all sites
anova <- aov(wdy$`Soil Moisture (% fresh soil mass)` ~ wdy$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Haweswater
haw <- d[(81:100),]
#Type 1 two-way anova using data from all sites
anova <- aov(haw$`Soil Moisture (% fresh soil mass)` ~ haw$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Whiteside
whi <- d[(101:120),]
#Type 1 two-way anova using data from all sites
anova <- aov(whi$`Soil Moisture (% fresh soil mass)` ~ whi$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)



#### Total C ----
d <- readr::read_csv(
  here::here("data", "8) drift corrected-C-N.csv")
) 

#order the sites as they should appear on the graph from west to east
d$Site <- factor(d$Site, levels = c("Whiteside", "Haweswater", "Widdybanks", "Brimham Moor", "Scarth Wood Moor", "Bridestones"))

#boxplot the data. Use aes() with backticks (``) so avoid an error with our column name
c_bxp <- ggboxplot(d, x = "Site", aes(y = `Drift Corr C (g per kg)`), color = "Vegetation", palette = c("limegreen", "#AA4499"), lwd = 0.75)  +
  labs(x = "Site",
       y = expression("Total Soil Carbon (g kg"^-1*")")) + theme(
         # Remove panel border
         panel.border = element_blank(),  
         # Remove panel grid lines
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         # Remove panel background
         panel.background = element_blank(),
         # Add axis line
         axis.line = element_line(colour = "black", linewidth = 0.5),
         #change colour and thickness of axis ticks
         axis.ticks = element_line(colour = "black", linewidth = 0.5),
         #change axis labels colour
         axis.title.x = element_text(colour = "black"),
         axis.title.y = element_text(colour = "black"),
         #change tick labels colour
         axis.text.x = element_text(colour = "black"),
         axis.text.y = element_text(colour = "black"),
       ) 

show(c_bxp)  
#save our plot
ggsave(path = "figures", paste0(Sys.Date(), "_total-C.svg"), width = 10, height= 5, c_bxp)


#Type 1 two-way anova using data from all sites
hist(d$`Drift Corr C (g per kg)`)
anova <- aov(d$`Drift Corr C (g per kg)` ~ d$Vegetation*d$Site)

#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$`Drift Corr C (g per kg)` ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)


summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
print(cld)



#now analyse at each site

#Bridestones
bri <- d[(1:20),]
#Type 1 two-way anova using data from all sites
anova <- aov(bri$`Drift Corr C (g per kg)` ~ bri$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Scarth Wood Moor
swm <- d[(21:40),]
#Type 1 two-way anova using data from all sites
anova <- aov(swm$`Drift Corr C (g per kg)` ~ swm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Brimham
bhm <- d[(41:60),]
#Type 1 two-way anova using data from all sites
anova <- aov(bhm$`Drift Corr C (g per kg)` ~ bhm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Widdybanks
wdy <- d[(61:80),]
#Type 1 two-way anova using data from all sites
anova <- aov(wdy$`Drift Corr C (g per kg)` ~ wdy$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Haweswater
haw <- d[(81:100),]
#Type 1 two-way anova using data from all sites
anova <- aov(haw$`Drift Corr C (g per kg)` ~ haw$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Whiteside
whi <- d[(101:120),]
#Type 1 two-way anova using data from all sites
anova <- aov(whi$`Drift Corr C (g per kg)` ~ whi$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)




#### DOM Quantity Data Formatting ----
#read in the raw data
d <- readr::read_csv(
  here::here("data-raw", "project-2-data-master", "individual", "04) Project-2_all-sites_Vario-TOC.csv")
) 
# Group by 'Sample ID' and calculate the mean of 'concentration', putting the output in a new dataframe
averaged_df <- d %>%
  group_by(`Name`) %>%
  summarise(`NPOC (mg/l)` = mean(`NPOC [mg/l]`), `TNb (mg/l)` = mean(`TNb [mg/l]`) )
#change the name of column 1
names(averaged_df)[1] <- "Sample ID"

#save our processed data file.  Once we have the data for all samples, we can pool into one file and add Site and Vegetation columns
write.csv(averaged_df, file =  "C:/Users/jakef/Documents/York/Project 2 Analysis/project-2/data/6) Averaged-TOC-Data.csv", row.names =FALSE)
#in this spreadsheet, standardize all sample names and add in the Site and Vegetation columns, and save it as 7) Formatted-Averaged-TOC-Data.csv

#### WEOC and TNb Quantity Data Analysis ----
#read in the formatted data
d <- readr::read_csv(
  here::here("data", "7) Formatted-Averaged-TOC-Data.csv"))
#Convert mg per L to mg C per g dry soil
d_pH <- readr::read_csv(
  here::here("data-raw", "project-2-data-master", "individual", "2) Soil pH.csv")
) 
#add dry soil mass and water added columns from d_pH to d
d <- merge(d, d_pH[, c("Sample ID", "Soil Mass (g)", "Water added (ml)")], by = "Sample ID", all.x = TRUE)
#standardise NPOC and TNb by multiplying by volume of extract (in liters), and dividing by grams of dry soil added
d$`NPOC (mg C g-1)` <- (d$`NPOC (mg/l)`*(d$`Water added (ml)`/1000))/d$`Soil Mass (g)`
d$`TNb (mg N g-1)` <- (d$`TNb (mg/l)`*(d$`Water added (ml)`/1000))/d$`Soil Mass (g)`
#reorder the sites so they show up on the plot from west (LHS) to east (RHS)
d$Site <- factor(d$Site, levels = c("Whiteside", "Haweswater", "Widdybanks", "Brimham Moor", "Scarth Wood Moor", "Bridestones"))
#boxplot the data. Use aes() with backticks (``) so avoid an error with our column name
dom_bxp <- ggboxplot(d, x = "Site", aes(y = `NPOC (mg C g-1)`), color = "Vegetation", palette = c("limegreen", "#AA4499"), lwd = 0.75)  + 
  labs(x = "Site",
       y = expression("NPOC (mg C g"^-1*")")) + theme(
         # Remove panel border
         panel.border = element_blank(),  
         # Remove panel grid lines
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         # Remove panel background
         panel.background = element_blank(),
         # Add axis line
         axis.line = element_line(colour = "black", linewidth = 0.5),
         #change colour and thickness of axis ticks
         axis.ticks = element_line(colour = "black", linewidth = 0.5),
         #change axis labels colour
         axis.title.x = element_text(colour = "black"),
         axis.title.y = element_text(colour = "black"),
         #change tick labels colour
         axis.text.x = element_text(colour = "black"),
         axis.text.y = element_text(colour = "black"),
       ) 


#boxplot the data. Use aes() with backticks (``) so avoid an error with our column name
dom_bxp <- ggboxplot(d, x = "Site", aes(y = `NPOC (mg C g-1)`), color = "Vegetation", palette = c("limegreen", "#AA4499"), lwd = 0.75)  + 
  labs(x = "Site",
       y = expression("NPOC (mg C g"^-1*")")) + theme(
         # Remove panel border
         panel.border = element_blank(),  
         # Remove panel grid lines
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         # Remove panel background
         panel.background = element_blank(),
         # Add axis line
         axis.line = element_line(colour = "black", linewidth = 0.5),
         #change colour and thickness of axis ticks
         axis.ticks = element_line(colour = "black", linewidth = 0.5),
         #change axis labels colour
         axis.title.x = element_text(colour = "black"),
         axis.title.y = element_text(colour = "black"),
         #change tick labels colour
         axis.text.x = element_text(colour = "black"),
         axis.text.y = element_text(colour = "black"),
       ) 
show(dom_bxp)  
#save our plot
ggsave(path = "figures", paste0(Sys.Date(), "_DOM.svg"), width = 10, height= 5, dom_bxp)


#Type 1 two-way anova using data from all sites
anova <- aov(d$`NPOC (mg C g-1)` ~ d$Vegetation*d$Site)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
print(cld)

#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$`NPOC (mg C g-1)` ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

#now analyse at each site

#Bridestones
bri <- d[(21:40),]
#Type 1 two-way anova using data from all sites
anova <- aov(bri$`NPOC (mg C g-1)` ~ bri$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Scarth Wood Moor
swm <- d[(61:80),]
#Type 1 two-way anova using data from all sites
anova <- aov(swm$`NPOC (mg C g-1)` ~ swm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Brimham
bhm <- d[(1:20),]
#Type 1 two-way anova using data from all sites
anova <- aov(bhm$`NPOC (mg C g-1)` ~ bhm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Widdybanks
wdy <- d[(81:100),]
#Type 1 two-way anova using data from all sites
anova <- aov(wdy$`NPOC (mg C g-1)` ~ wdy$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Haweswater
haw <- d[(41:60),]
#Type 1 two-way anova using data from all sites
anova <- aov(haw$`NPOC (mg C g-1)` ~ haw$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Whiteside
whi <- d[(101:120),]
#Type 1 two-way anova using data from all sites
anova <- aov(whi$`NPOC (mg C g-1)` ~ whi$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)




#boxplot the data. Use aes() with backticks (``) so avoid an error with our column name
tnb_bxp <- ggboxplot(d, x = "Site", aes(y = `TNb (mg N g-1)`), color = "Vegetation", palette = c("limegreen", "#AA4499"), lwd = 0.75)  + 
  labs(x = "Site",
       y = expression("TNb (mg N g"^-1*")")) + theme(
         # Remove panel border
         panel.border = element_blank(),  
         # Remove panel grid lines
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         # Remove panel background
         panel.background = element_blank(),
         # Add axis line
         axis.line = element_line(colour = "black", linewidth = 0.5),
         #change colour and thickness of axis ticks
         axis.ticks = element_line(colour = "black", linewidth = 0.5),
         #change axis labels colour
         axis.title.x = element_text(colour = "black"),
         axis.title.y = element_text(colour = "black"),
         #change tick labels colour
         axis.text.x = element_text(colour = "black"),
         axis.text.y = element_text(colour = "black"),
       ) 

tnb_bxp <- ggboxplot(d, x = "Vegetation", aes(y = `TNb (mg N g-1)`), color = "Site", lwd = 0.75)  + 
  labs(x = "Vegetation",
       y = expression("TNb (mg N g"^-1*")")) + theme(
         # Remove panel border
         panel.border = element_blank(),  
         # Remove panel grid lines
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         # Remove panel background
         panel.background = element_blank(),
         # Add axis line
         axis.line = element_line(colour = "black", linewidth = 0.5),
         #change colour and thickness of axis ticks
         axis.ticks = element_line(colour = "black", linewidth = 0.5),
         #change axis labels colour
         axis.title.x = element_text(colour = "black"),
         axis.title.y = element_text(colour = "black"),
         #change tick labels colour
         axis.text.x = element_text(colour = "black"),
         axis.text.y = element_text(colour = "black"),
       ) 


show(tnb_bxp)  
#save our plot
ggsave(path = "figures", paste0(Sys.Date(), "_TNb.svg"), width = 10, height= 5, tnb_bxp)


#Type 1 two-way anova using data from all sites
anova <- aov(d$`TNb (mg N g-1)` ~ d$Vegetation*d$Site)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
print(cld)

#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$`NPOC (mg C g-1)` ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

#now analyse at each site

#Bridestones
bri <- d[(21:40),]
#Type 1 two-way anova using data from all sites
anova <- aov(bri$`TNb (mg N g-1)` ~ bri$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Scarth Wood Moor
swm <- d[(61:80),]
#Type 1 two-way anova using data from all sites
anova <- aov(swm$`TNb (mg N g-1)` ~ swm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Brimham
bhm <- d[(1:20),]
#Type 1 two-way anova using data from all sites
anova <- aov(bhm$`TNb (mg N g-1)` ~ bhm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Widdybanks
wdy <- d[(81:100),]
#Type 1 two-way anova using data from all sites
anova <- aov(wdy$`TNb (mg N g-1)` ~ wdy$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Haweswater
haw <- d[(41:60),]
#Type 1 two-way anova using data from all sites
anova <- aov(haw$`TNb (mg N g-1)` ~ haw$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Whiteside
whi <- d[(101:120),]
#Type 1 two-way anova using data from all sites
anova <- aov(whi$`TNb (mg N g-1)` ~ whi$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)






#### Total N ----

d <- readr::read_csv(
  here::here("data", "8) drift corrected-C-N.csv")) 
#order the sites as they should appear on the graph from west to east
d$Site <- factor(d$Site, levels = c("Whiteside", "Haweswater", "Widdybanks", "Brimham Moor", "Scarth Wood Moor", "Bridestones"))

#boxplot the data. Use aes() with backticks (``) so avoid an error with our column name
n_bxp <- ggboxplot(d, x = "Site", aes(y = `Drift Corr N (g per kg)`), color = "Vegetation", palette = c("limegreen", "#AA4499"), lwd = 0.75)  +
  labs(x = "Site",
       y = expression("Total Soil Nitrogen (g kg"^-1*")")) + theme(
         # Remove panel border
         panel.border = element_blank(),  
         # Remove panel grid lines
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         # Remove panel background
         panel.background = element_blank(),
         # Add axis line
         axis.line = element_line(colour = "black", linewidth = 0.5),
         #change colour and thickness of axis ticks
         axis.ticks = element_line(colour = "black", linewidth = 0.5),
         #change axis labels colour
         axis.title.x = element_text(colour = "black"),
         axis.title.y = element_text(colour = "black"),
         #change tick labels colour
         axis.text.x = element_text(colour = "black"),
         axis.text.y = element_text(colour = "black"),
       ) 

show(n_bxp)  
#save our plot
ggsave(path = "figures", paste0(Sys.Date(), "_total-N.svg"), width = 10, height= 5, n_bxp)

#Type 1 two-way anova using data from all sites
anova <- aov(d$`Drift Corr N (g per kg)` ~ d$Vegetation*d$Site)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
print(cld)

#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$`Drift Corr N (g per kg)` ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

#now analyse at each site


#Bridestones
bri <- d[(1:20),]
#Type 1 two-way anova using data from all sites
anova <- aov(bri$`Drift Corr N (g per kg)` ~ bri$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Scarth Wood Moor
swm <- d[(21:40),]
#Type 1 two-way anova using data from all sites
anova <- aov(swm$`Drift Corr N (g per kg)` ~ swm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Brimham
bhm <- d[(41:60),]
#Type 1 two-way anova using data from all sites
anova <- aov(bhm$`Drift Corr N (g per kg)` ~ bhm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Widdybanks
wdy <- d[(61:80),]
#Type 1 two-way anova using data from all sites
anova <- aov(wdy$`Drift Corr N (g per kg)` ~ wdy$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Haweswater
haw <- d[(81:100),]
#Type 1 two-way anova using data from all sites
anova <- aov(haw$`Drift Corr N (g per kg)` ~ haw$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Whiteside
whi <- d[(101:120),]
#Type 1 two-way anova using data from all sites
anova <- aov(whi$`Drift Corr N (g per kg)` ~ whi$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)




#### C:N ratio ----

d <- readr::read_csv(
  here::here("data", "8) drift corrected-C-N.csv")
) 
#trim off empty rows
d <- d[1:120,1:17]

#order the sites as they should appear on the graph from west to east
d$Site <- factor(d$Site, levels = c("Whiteside", "Haweswater", "Widdybanks", "Brimham Moor", "Scarth Wood Moor", "Bridestones"))


#boxplot the data. Use aes() with backticks (``) so avoid an error with our column name
cn_bxp <- ggboxplot(d, x = "Site", aes(y = `CN ratio`), color = "Vegetation", palette = c("limegreen", "#AA4499"), lwd = 0.75)  +
  labs(x = "Site",
       y = expression("C:N Ratio")) + theme(
         # Remove panel border
         panel.border = element_blank(),  
         # Remove panel grid lines
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         # Remove panel background
         panel.background = element_blank(),
         # Add axis line
         axis.line = element_line(colour = "black", linewidth = 0.5),
         #change colour and thickness of axis ticks
         axis.ticks = element_line(colour = "black", linewidth = 0.5),
         #change axis labels colour
         axis.title.x = element_text(colour = "black"),
         axis.title.y = element_text(colour = "black"),
         #change tick labels colour
         axis.text.x = element_text(colour = "black"),
         axis.text.y = element_text(colour = "black"),
       ) 

show(cn_bxp)  
#save our plot
ggsave(path = "figures", paste0(Sys.Date(), "_CN_ratio.svg"), width = 10, height= 5, cn_bxp)


#Type 1 two-way anova using data from all sites
anova <- aov(d$`CN ratio` ~ d$Vegetation*d$Site)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
print(cld)

#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$`Drift Corr N (g per kg)` ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

#now analyse at each site


#Bridestones
bri <- d[(1:20),]
#Type 1 two-way anova using data from all sites
anova <- aov(bri$`CN ratio` ~ bri$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Scarth Wood Moor
swm <- d[(21:40),]
#Type 1 two-way anova using data from all sites
anova <- aov(swm$`CN ratio` ~ swm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Brimham
bhm <- d[(41:60),]
#Type 1 two-way anova using data from all sites
anova <- aov(bhm$`CN ratio` ~ bhm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Widdybanks
wdy <- d[(61:80),]
#Type 1 two-way anova using data from all sites
anova <- aov(wdy$`CN ratio` ~ wdy$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Haweswater
haw <- d[(81:100),]
#Type 1 two-way anova using data from all sites
anova <- aov(haw$`CN ratio` ~ haw$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Whiteside
whi <- d[(101:120),]
#Type 1 two-way anova using data from all sites
anova <- aov(whi$`CN ratio` ~ whi$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)



#### Soil DOM Quality Data formatting ----

#load the raw data
d <- readr::read_csv(
  here::here("data-raw", "project-2-data-master", "individual", "3) Undiluted DOM Spectra.csv")
) 
#transpose the dataframe, and convert to matrix
d <- as.data.frame(t(d))
# Set the first row as column names
colnames(d) <- as.character(d[1, ])
# Remove the first row from the dataframe
d <- d[-1, ]
#make a column containg the sample ID names
# Assuming your dataframe is named 'df'
d <- cbind(`Sample ID` = rownames(d), d)
#reset the row names to default
rownames(d) <- NULL
#order samples by ID alphabetically
d <- arrange(d, d["Sample ID"])
#add character to column names else we can't covnert from wide to long
# adding suffix to column names  
colnames(d) <- paste("new",sep="_",colnames(d)) 
#convert the numbers (which are datatype character for some reason) to integers
d[,2:582] <- sapply(d[,2:582],as.numeric)
#add wavelength (nm) as a column
d_long <- pivot_longer(d, cols = new_220:new_800, names_to = "Wavelength (nm)", values_to = "Absorbance")
#remove the characters we added so we can plot the data
d_long$`Wavelength (nm)` <- gsub("new_","",d_long$`Wavelength (nm)`)
colnames(d_long)[1] <- "Sample ID"
#convert the wavelength numbers (which are datatype character for some reason) to integers
d_long[,2] <- sapply(d_long[,2],as.numeric)
#save our processed data file
write.csv(d_long, "4) Processed Undiluted DOM Spectra.csv", row.names =FALSE)

#### Soil DOM Quality Data Analysis - alpha parameter generation ----

#read in the processed absorbance data
d <- readr::read_csv(
  here::here("data", "4) Processed Undiluted DOM Spectra.csv")
) 
#read in data containing DOM concentration
d_conc <- readr::read_csv(
  here::here("data", "7) Formatted-Averaged-TOC-Data.csv"))
#add the DOM concentration column to our absorbance dataframe
d <- merge(d, d_conc[, c("Sample ID", "NPOC (mg/l)")], by = "Sample ID", all.x = TRUE)
#divide absorbance by concentration
d$`Standardized Absorbance` <- d$Absorbance/d$`NPOC (mg/l)`
#eliminate all absorbance values above 600 nm
d <- d %>% filter(`Wavelength (nm)`<= 600)

#let's remove all rows which have an absorbance value <= to 0, just to see if this makes the code work
#eliminate all absorbance values above 600 nm
d <- d %>% filter(`Absorbance` > 0)

# Fit a two-component exponential decay curve through the data for all our curves
fitted <- d %>%
  nest(-`Sample ID`) %>%
  mutate(
    fit = map(data, ~nls(Absorbance ~ SSasymp(`Wavelength (nm)`, yf, y0, log_alpha), data = .)),
    tidied = map(fit, tidy),
    augmented = map(fit, augment),
  )

# Produce a table of fit parameters: y0, yf, alpha
table <- fitted %>% 
  unnest(tidied) %>% 
  select(`Sample ID`, term, estimate) %>% 
  spread(term, estimate) %>% 
  mutate(alpha = exp(log_alpha))
#display table of fit parameters
table
#plot each absorbance curve along with the line of best fit
augmented <- fitted %>% 
  unnest(augmented)
qplot(`Wavelength (nm)`, Absorbance, data = augmented, geom = 'point', colour = `Sample ID`) +
  geom_line(aes(y=.fitted))
#add columns to our new table for the Site and Vegetation parameters
table$Site <- c(rep("Brimham Moor",20), rep("Bridestones",20), rep("Haweswater",20), rep("Scarth Wood Moor",20), rep("Widdybanks",20), rep("Whiteside",20))
table$Vegetation <- c(rep("Bracken",10), rep("Heather", 10), rep("Bracken",10), rep("Heather", 10),rep("Bracken",10), rep("Heather", 10),rep("Bracken",10), rep("Heather", 10),rep("Bracken",10), rep("Heather", 10),rep("Bracken",10), rep("Heather", 10))
#save our processed data file in the "data" folder
write.csv(table, "data\\5) alpha paramater of DOM curve fitting.csv", row.names =FALSE)
#read in the processed absorbance data
table <- readr::read_csv(
  here::here("data", "5) alpha paramater of DOM curve fitting.csv")) 
#reorder the sites so they show up on the plot from west (LHS) to east (RHS)
table$Site <- factor(table$Site, levels = c("Whiteside", "Haweswater", "Widdybanks", "Brimham Moor", "Scarth Wood Moor", "Bridestones"))
#boxplot the alpha, which describes the curve ie how quicky we go from low wavelength (high mass C compounds) to high wavelength (low mass C compounds)
alpha_bxp <- ggboxplot(table, x = "Site", y = 'alpha', color = "Vegetation", palette = c("limegreen", "#AA4499"), lwd = 0.75)  +
  labs(x = "Site",
       y = "DOM fitted curve alpha parameter") + theme(
         # Remove panel border
         panel.border = element_blank(),  
         # Remove panel grid lines
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         # Remove panel background
         panel.background = element_blank(),
         # Add axis line
         axis.line = element_line(colour = "black", linewidth = 0.5),
         #change colour and thickness of axis ticks
         axis.ticks = element_line(colour = "black", linewidth = 0.5),
         #change axis labels colour
         axis.title.x = element_text(colour = "black"),
         axis.title.y = element_text(colour = "black"),
         #change tick labels colour
         axis.text.x = element_text(colour = "black"),
         axis.text.y = element_text(colour = "black"),
       ) 
show(alpha_bxp)

#save our plot
ggsave(path = "C:/Users/jakef/Documents/York/Project 2 Analysis/project-2/figures", paste0(Sys.Date(), "_DOM-curve-alpha-parameter_green-purple.svg"), width = 10, height= 5, alpha_bxp)
- alp
#now plot the DOM fitted curve alpha parameter against longitude

individual <- readr::read_csv(
  here::here("data", "all-sites_field-data.csv"), show_col_types = FALSE
) 
#rename the common column
colnames(individual)[1] <- "Sample ID"
#replqce BRM with BHM
individual$`Sample ID` <- gsub("BRM", "BHM", individual$`Sample ID`)

# copy "LongitudeE" over to the "table" dataframe
result <- merge(table, individual[, c("Sample ID", "LongitudeE")], by = "Sample ID", all.x = TRUE)

# Create the scatterplot with a linear regression line
scatter <- ggplot(result, aes(x = `LongitudeE`, y = `alpha`)) +
  geom_point() +                            # Scatterplot points
  geom_smooth(method = "lm", se = FALSE) +   # Linear regression line
  labs(x = "Longitude",
       y = "DOM fitted curve alpha parameter") +
  theme_minimal()                            # Minimal theme for better aesthetics

#save our plot
ggsave(path = "C:/Users/jakef/Documents/York/Project 2 Analysis/project-2/figures", paste0(Sys.Date(), "_longitude-vs-DOM-curve-alpha-paramter.svg"), scatter)

# Fit a linear model to the data
model <- lm(alpha ~ LongitudeE, data = result)
# Get the summary of the model
summary_model <- summary(model)
# Extract the R-squared value
r_squared <- summary_model$r.squared
# Print the R-squared value
print(r_squared)

#### Alpha parameter analysis ----
d <- readr::read_csv(
  here::here("data", "5) alpha paramater of DOM curve fitting.csv")
) 


#Type 1 two-way anova using data from all sites
anova <- aov(d$alpha ~ d$Vegetation*d$Site)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
print(cld)

#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$alpha ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

#now analyse at each site

#Bridestones
bri <- d[(21:40),]
#Type 1 two-way anova using data from all sites
anova <- aov(bri$`alpha` ~ bri$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Scarth Wood Moor
swm <- d[(61:80),]
#Type 1 two-way anova using data from all sites
anova <- aov(swm$alpha ~ swm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Brimham
bhm <- d[(1:20),]
#Type 1 two-way anova using data from all sites
anova <- aov(bhm$alpha ~ bhm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Widdybanks
wdy <- d[(81:100),]
#Type 1 two-way anova using data from all sites
anova <- aov(wdy$alpha ~ wdy$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Haweswater
haw <- d[(41:60),]
#Type 1 two-way anova using data from all sites
anova <- aov(haw$alpha ~ haw$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Whiteside
whi <- d[(101:120),]
#Type 1 two-way anova using data from all sites
anova <- aov(whi$alpha ~ whi$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)







#### DOM Specific Wavelength of Interest boxplots ----

#read in the processed absorbance data
d <- readr::read_csv(
  here::here("data", "4) Processed Undiluted DOM Spectra.csv")
) 

#read in data containing DOM concentration
d_conc <- readr::read_csv(
  here::here("data", "7) Formatted-Averaged-TOC-Data.csv"))
#add the DOM concentration column to our absorbance dataframe
d <- merge(d, d_conc[, c("Sample ID", "NPOC (mg/l)")], by = "Sample ID", all.x = TRUE)
#divide absorbance by concentration to standardize the measurements
d$`Standardized Absorbance` <- d$Absorbance/d$`NPOC (mg/l)`
# repeat at wavelengths of 250 (aromaticity, apparent molecular weight), 254 (aromaticity), 260 (hydrophobic C content), 265 (relative abundance of functional groups), 272 (aromaticity), 280 (hydrophobic C content, humification index, apparent molecular size), 285 (humification index), 300 (characterization of humic substances), 340 (colour), 350 (apparent molecular size), 365 (aromaticity, apparent molecular weight), 400 (humic substances characterization), 436 (quality indicator), 465 (relative abundance of functional groups)

#function to plot data for DOM, requires dataframe d (with columns `Sample ID`, `Wavelength (nm)`, Absorbance, Standardized Absorbance) and wavelenth of interest
DOMboxplotter <- function(d, wavelength){
  
  #define the wavelength of interest
  wavelength_of_interest <- wavelength
  
  
  #filter data to extract absorbance at wavelength of interest
  abs <- d %>%
    filter(`Wavelength (nm)` == wavelength_of_interest) %>% #filter for specific wavelength
    select(`Sample ID`, `Standardized Absorbance`) #select the relevant columns
  
  
  #order sites
  abs <- arrange(abs, abs["Sample ID"])
  abs$Site <- c(rep("Brimham Moor",20), rep("Bridestones",20), rep("Haweswater",20), rep("Scarth Wood Moor",20), rep("Widdybanks",20), rep("Whiteside",20))
  
  
  abs$Vegetation <- c(rep("Bracken",10), rep("Heathland", 10),rep("Bracken",10), rep("Heathland", 10),rep("Bracken",10), rep("Heathland", 10),rep("Bracken",10), rep("Heathland", 10),rep("Bracken",10), rep("Heathland", 10),rep("Bracken",10), rep("Heathland", 10))
  
  abs <- as.data.frame(abs)
  
  #create string for y axis label
  yaxis_label <- paste("Absorbance at ", wavelength_of_interest, "nm")
  #reorder the sites so they show up on the plot from west (LHS) to east (RHS)
  abs$Site <- factor(abs$Site, levels = c("Whiteside", "Haweswater", "Widdybanks", "Brimham Moor", "Scarth Wood Moor", "Bridestones"))
  
  #plot absorbance
  abs_bxp <- ggboxplot(abs, x = "Site", y = 'Standardized Absorbance', color = "Vegetation", palette = c("limegreen", "#AA4499"), lwd = 0.75)  +
    labs(x = "Site",
         y = yaxis_label) + theme(
           # Remove panel border
           panel.border = element_blank(),  
           # Remove panel grid lines
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           # Remove panel background
           panel.background = element_blank(),
           # Add axis line
           axis.line = element_line(colour = "black", linewidth = 0.5),
           #change colour and thickness of axis ticks
           axis.ticks = element_line(colour = "black", linewidth = 0.5),
           #change axis labels colour
           axis.title.x = element_text(colour = "black"),
           axis.title.y = element_text(colour = "black"),
           #change tick labels colour
           axis.text.x = element_text(colour = "black"),
           axis.text.y = element_text(colour = "black"),
         ) 
  
  show(abs_bxp)
  #create file name for our plot.  Use paste0 so there are no spaces between each item in the list
  filename <- paste0(Sys.Date(), "_absorbance_at_", wavelength_of_interest, ".svg")
  #save our plot.  As this is a function, we need specify the entire file path
  ggsave(path = "C:/Users/jakef/Documents/York/Project 2 Analysis/project-2/figures", filename , width = 10, height= 5, abs_bxp)
  
  #statistical analysis
  
  #Type 1 two-way anova using data from all sites
  print(wavelength_of_interest)
  anova <- aov(abs$`Standardized Absorbance` ~ abs$Vegetation*abs$Site)
  print(summary(anova))
  #tukey's test to identify significant interactions
  tukey <- TukeyHSD(anova)
  #compact letter display
  cld <- multcompLetters4(anova, tukey)
  print(cld)
  print(tukey)
  
  #check homogeneity of variance
  plot(anova, 1)
  #levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
  print(leveneTest(abs$`Standardized Absorbance` ~ abs$Vegetation*abs$Site))
  #check normality.  
  plot(anova, 2)
  #conduct shapiro-wilk test on ANOVA residuals to test for normality
  #extract the residuals
  aov_residuals <- residuals(object = anova)
  #run shapiro-wilk test.  if p > 0.05 the data is normal
  print(shapiro.test(x = aov_residuals))
  
  
  #Bridestones
  bri <- abs[(21:40),]
  #Type 1 two-way anova using data from all sites
  anova <- aov(bri$`Standardized Absorbance` ~ bri$Vegetation)
  summary(anova)
  #tukey's test to identify significant interactions
  tukey <- TukeyHSD(anova)
  #print(tukey)
  #compact letter display
  cld <- multcompLetters4(anova, tukey)
  #compact letter display
  print(cld)
  #Scarth Wood Moor
  swm <- abs[(61:80),]
  #Type 1 two-way anova using data from all sites
  anova <- aov(swm$`Standardized Absorbance` ~ swm$Vegetation)
  summary(anova)
  #tukey's test to identify significant interactions
  tukey <- TukeyHSD(anova)
  #print(tukey)
  #compact letter display
  cld <- multcompLetters4(anova, tukey)
  #compact letter display
  print(cld)
  #Brimham
  bhm <- abs[(1:20),]
  #Type 1 two-way anova using data from all sites
  anova <- aov(bhm$`Standardized Absorbance` ~ bhm$Vegetation)
  summary(anova)
  #tukey's test to identify significant interactions
  tukey <- TukeyHSD(anova)
  #print(tukey)
  #compact letter display
  cld <- multcompLetters4(anova, tukey)
  #compact letter display
  print(cld)
  #Widdybanks
  wdy <- abs[(81:100),]
  #Type 1 two-way anova using data from all sites
  anova <- aov(wdy$`Standardized Absorbance`~ wdy$Vegetation)
  summary(anova)
  #tukey's test to identify significant interactions
  tukey <- TukeyHSD(anova)
  #print(tukey)
  #compact letter display
  cld <- multcompLetters4(anova, tukey)
  #compact letter display
  print(cld)
  #Haweswater
  haw <- abs[(41:60),]
  #Type 1 two-way anova using data from all sites
  anova <- aov(haw$`Standardized Absorbance` ~ haw$Vegetation)
  summary(anova)
  #tukey's test to identify significant interactions
  tukey <- TukeyHSD(anova)
  #print(tukey)
  #compact letter display
  cld <- multcompLetters4(anova, tukey)
  #compact letter display
  print(cld)
  #Whiteside
  whi <- abs[(101:120),]
  #Type 1 two-way anova using data from all sites
  anova <- aov(whi$`Standardized Absorbance` ~ whi$Vegetation)
  summary(anova)
  #tukey's test to identify significant interactions
  tukey <- TukeyHSD(anova)
  #print(tukey)
  #compact letter display
  cld <- multcompLetters4(anova, tukey)
  #compact letter display
  print(cld)
  
}
#list of wavelengths of interest
wavelength_of_interest <- list(465)#list(250, 254, 260, 265, 272, 280, 285, 300, 340, 350, 365, 400, 436, 465)
#plot the absorbance boxplot at the following given wavelengths, and gives stats
for (wavelength in wavelength_of_interest){
  DOMboxplotter(d, wavelength)
}

#### Save SUVA data ----
#read in the processed absorbance data
d <- readr::read_csv(
  here::here("data", "4) Processed Undiluted DOM Spectra.csv")
) 

#read in data containing DOM concentration
d_conc <- readr::read_csv(
  here::here("data", "7) Formatted-Averaged-TOC-Data.csv"))
#add the DOM concentration column to our absorbance dataframe
d <- merge(d, d_conc[, c("Sample ID", "NPOC (mg/l)")], by = "Sample ID", all.x = TRUE)
#divide absorbance by concentration to standardize the measurements
d$`SUVA (L mg-1 cm-1)` <- d$Absorbance/d$`NPOC (mg/l)`
# repeat at wavelengths of 250 (aromaticity, apparent molecular weight), 254 (aromaticity), 260 (hydrophobic C content), 265 (relative abundance of functional groups), 272 (aromaticity), 280 (hydrophobic C content, humification index, apparent molecular size), 285 (humification index), 300 (characterization of humic substances), 340 (colour), 350 (apparent molecular size), 365 (aromaticity, apparent molecular weight), 400 (humic substances characterization), 436 (quality indicator), 465 (relative abundance of functional groups)

#list of wavelengths of interest
wavelength_of_interest <- list(254)

abs <- d %>%
  filter(`Wavelength (nm)` == wavelength_of_interest) %>% #filter for specific wavelength
  select(`Sample ID`, `SUVA (L mg-1 cm-1)`) #select the relevant columns

abs <- as.data.frame(abs)

write.csv(abs, "data/SUVA data.csv")

#### SUVA data analysis ----
d <- readr::read_csv(
  here::here("data", "SUVA data_all-factors.csv")
) 


#Type 1 two-way anova using data from all sites
anova <- aov(d$`SUVA (L mg-1 cm-1)` ~ d$Vegetation*d$Site)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
print(cld)

#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$`SUVA (L mg-1 cm-1)` ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

#now analyse at each site

#Bridestones
bri <- d[(21:40),]
#Type 1 two-way anova using data from all sites
anova <- aov(bri$`SUVA (L mg-1 cm-1)` ~ bri$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Scarth Wood Moor
swm <- d[(61:80),]
#Type 1 two-way anova using data from all sites
anova <- aov(swm$`SUVA (L mg-1 cm-1)` ~ swm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Brimham
bhm <- d[(1:20),]
#Type 1 two-way anova using data from all sites
anova <- aov(bhm$`SUVA (L mg-1 cm-1)` ~ bhm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Widdybanks
wdy <- d[(81:100),]
#Type 1 two-way anova using data from all sites
anova <- aov(wdy$`SUVA (L mg-1 cm-1)`~ wdy$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Haweswater
haw <- d[(41:60),]
#Type 1 two-way anova using data from all sites
anova <- aov(haw$`SUVA (L mg-1 cm-1)` ~ haw$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Whiteside
whi <- d[(101:120),]
#Type 1 two-way anova using data from all sites
anova <- aov(whi$`SUVA (L mg-1 cm-1)` ~ whi$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)




#### generate PCA data file containing all data we need ----
d <- readr::read_csv(
  here::here("data", "2025-03-19_PCA-data.csv"), show_col_types = FALSE
) 
#add in site names
d$Site <- c(rep("Bridestones",20), rep("Scarth Wood Moor",20), rep("Brimham Moor",20), rep("Haweswater",20), rep("Whiteside", 20), rep("Widdybanks",20))
#rename "Land" column to "Vegetation"
names(d)[4] <- "Vegetation"
#order samples by bracken or heather
d <- arrange(d, d["Vegetation"])
#ensure d is a dataframe
d <- as.data.frame(d)
#replace veg data null (empty excell cell) with "0"
d[is.na(d)] <- 0
#create a subset containing only species abundance values
#just the species counts
spe <- d[,-(1:5)]
spe <- spe[,-(16:18)]
#species richness
d$`Veg. richness` <- apply(spe[,]>0,1,sum)
#calculate diversity
d$`Veg. shannon` <- diversity(spe[,], "shannon")
d$`Veg. simpson` <- diversity(spe[,], "simpson")
d$`Veg. evenness` <- d$`Veg. shannon` / log(d$`Veg. richness`)

#import moisture data
d_moisture <- readr::read_csv(
  here::here("data-raw", "project-2-data-master", "individual", "1) Soil Moisture Content.csv")
)
#append the data
d <- merge(d, d_moisture[, c("Sample ID", "Soil Moisture (% fresh soil mass)")], by = "Sample ID", all.x = TRUE)

#pH data
d_pH <- readr::read_csv(
  here::here("data-raw", "project-2-data-master", "individual", "2) Soil pH.csv")
)
d <- merge(d, d_pH[, c("Sample ID", "pH")], by = "Sample ID", all.x = TRUE)

#WEOC/TNB data
d_toc <- readr::read_csv(
  here::here("data", "7) Formatted-Averaged-TOC-Data.csv"))
#Convert mg per L to mg C per g dry soil
#add dry soil mass and water added columns from d_pH to d
d_toc <- merge(d_toc, d_pH[, c("Sample ID", "Soil Mass (g)", "Water added (ml)")], by = "Sample ID", all.x = TRUE)
#standardise NPOC and TNb by multiplying by volume of extract (in liters), and dividing by grams of dry soil added
d_toc$`NPOC (mg C g-1)` <- (d_toc$`NPOC (mg/l)`*(d_toc$`Water added (ml)`/1000))/d_toc$`Soil Mass (g)`
d_toc$`TNb (mg N g-1)` <- (d_toc$`TNb (mg/l)`*(d_toc$`Water added (ml)`/1000))/d_toc$`Soil Mass (g)`
#merge the data
d <- merge(d, d_toc[, c("Sample ID", "NPOC (mg C g-1)")], by = "Sample ID", all.x = TRUE)
d <- merge(d, d_toc[, c("Sample ID", "TNb (mg N g-1)")], by = "Sample ID", all.x = TRUE)

#total carbon, N, C:N
d_totc <- readr::read_csv(
  here::here("data", "8) drift corrected-C-N.csv")) 
#trim off empty rows
d_totc <- d_totc[1:120,1:17]
d <- merge(d, d_totc[, c("Sample ID", "Drift Corr C (g per kg)")], by = "Sample ID", all.x = TRUE)
d <- merge(d, d_totc[, c("Sample ID", "Drift Corr N (g per kg)")], by = "Sample ID", all.x = TRUE)
d <- merge(d, d_totc[, c("Sample ID", "CN ratio")], by = "Sample ID", all.x = TRUE)

#WEOC quality alpha parameter
table <- readr::read_csv(
  here::here("data", "5) alpha paramater of DOM curve fitting.csv")) 
d <- merge(d, table[, c("Sample ID", "alpha")], by = "Sample ID", all.x = TRUE)

#SUVA
suva <- readr::read_csv(
  here::here("data", "SUVA data.csv")) 
d <- merge(d, suva[, c("Sample ID", "SUVA (L mg-1 cm-1)")], by = "Sample ID", all.x = TRUE)

#save our master datafile
write.csv(d, "data/2025-03-19_PCA-data.csv")

#### PCA analysis ----
d <- readr::read_csv(
  here::here("data", "2025-03-19_PCA-data.csv"), show_col_types = FALSE
) 
numerical_d <- d[,-c(1:5)]
numerical_d <- d_numerical[, -19]
numerical_d <- na.omit(numerical_d)
# Remove columns that are entirely zeros
numerical_d <- numerical_d[, colSums(numerical_d != 0) > 0]


#normalize the data
data_normalized <- scale(numerical_d)
#compute the PCA
pca_result <- prcomp(data_normalized)
# View the results
summary(pca_result)  # Summary of the PCA (explained variance, etc.)

# Principal components
pca_result$x  # The scores (the projections of the original data onto the PCs)

# The loadings (eigenvectors).
pca_result$rotation  # The loadings (coefficients of the principal components)

# Variance explained by each principal component
pca_result$sdev^2 / sum(pca_result$sdev^2)  # Proportion of variance explained


#needed for fviz_eig() function
library(factoextra)
#generate scree plot using fviz_eig() function.  This shows the eigenvalues from highest to lowest, i.e. from the components which explain the most variance to the components which explain the least
fviz_eig(pca_result, addlabels = TRUE)


# Plotting the PCA results (optional)
# First PC vs. Second PC
plot(pca_result$x[, 1], pca_result$x[,2], 
     xlab = "PC1", ylab = "PC2", 
     main = "PCA - PC1 vs PC2")


# Biplot to visualize the PCA results.  We are vizualising the similarities and differences between samples, and shows the impact of each attribute on each of the principal components.  Variables that are grouped together are positively correlated with one another.  The further the distance between the variable and the origin, the better represented the variaible is.  Variables that are negatively correlated are displayed to the opposite side of the biplot's origin.
biplot(pca_result)

#now determine the variable's contribution to principal components.  This representation is called the Cos2, and corresponds to the square cosine.  A low value means the variable is not perfectly represented by that component, whilst a high value means a good representation of the variable on that component.
fviz_cos2(pca_result, choice = "var", axes = 1:2)

#combine biplot and attribute importance.  Attributes with similar cos2 scores will have similar colours.
fviz_pca_var(pca_result, col.var = "cos2", 
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)






#### Invertebrate catch percetnage breakdown  and standardozed abundances ----
d <- readr::read_csv(
  here::here("data", "n = 10 Tullgren Extracts - Week 1 + 2 Extracts.csv")
) 
#order samples by ID alphabetically
d <- arrange(d, d["Sample ID"])
d <- as.data.frame(d)
#replace null (empty excell cell) with "0"
d[is.na(d)] <- 0
#order the sites as they should appear on the graph from west to east
d$Site <- factor(d$Site, levels = c("Whiteside", "Haweswater", "Widdybanks", "Brimham Moor", "Scarth Wood Moor", "Bridestones"))
d$Vegetation <- factor(d$Vegetation, levels = c("Bracken", "Heather"))
#total mesofauna abundances
d$`Total Mesofauna Catch`<- rowSums(d[,4:11])
#total invertebrate abundances
d$`Total Invertebrate Catch`<- rowSums(d[,4:27])


print(sum(d$`Total Mesofauna Catch`))
print(sum(d$`Total Invertebrate Catch`))

#get percentages of mites/springtails of all morphospecies
d$`Percentage Mites` = ((d$Mesostigmata + d$Oribatida + d$Astigmatina + d$Prostigmata)/(d$`Total Invertebrate Catch`))*100
d$`Percentage Collembola` = ((d$Symphypleona + d$Neelidae + d$Entomobryomorpha + d$Poduromorpha)/d$`Total Invertebrate Catch`)*100
d$`Percentage Other` = (100-d$`Percentage Collembola` - d$`Percentage Mites`)

#we are printing multiple lines so use cat()
cat("Min mite percentage: ", min(d$`Percentage Mites`), "Max mite percentage: ", max(d$`Percentage Mites`),"Min collembola percentage: ", min(d$`Percentage Collembola`), "Max collembola percentage: ", max(d$`Percentage Collembola`), "Min other invert percentage: ", min(d$`Percentage Other`), "Max other invert percentage: ", max(d$`Percentage Other`))

#standardize abundances to standard depth
#extract the morphospecies count data
#volume of the soil corer
volume <- 0.1 * (pi*(0.025^2))
d$CoreVolume <- volume
#number of mesofauna per m3, to a depth of 10cm
d$`1000s Individuals per m2 to 10 cm depth` <- ((d$`Total Mesofauna Catch`/d$CoreVolume)*0.1)/1000
#standardize to dry soil mass
#load data containg tullgren dry soil mass
d_tdsm <- readr::read_csv(
  here::here("data", "all-sites_dry-tullgren-soil-mass.csv")
) 
d$`Sample ID` <- gsub(" ", "-", d$`Sample ID`)
#add the dry soil mass column
d <- d %>%
  left_join(d_tdsm %>% select(`Sample ID`, `Dry soil mass (g)`), by = "Sample ID")
#calculate  individuals per g dry soil
d$`Individuals per 100g dry soil` <- (d$`Total Mesofauna Catch`/d$`Dry soil mass (g)`)*100

#plot abundances

library(scales) # displays 100,000 as 100,000, not as 1e5

figure <- ggboxplot(d, x = "Site", y = 'Total Mesofauna Catch', color = "Vegetation", palette = c("limegreen", "#AA4499"), lwd = 0.75)  +
  labs(y = expression("Total Mesofauna Catch")) + theme( #remove x axis label
    axis.title.x=element_blank(),
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "black", linewidth = 0.5),
    #change colour and thickness of axis ticks
    axis.ticks = element_line(colour = "black", linewidth = 0.5),
    #change axis labels colour
    axis.title.y = element_text(colour = "black"),
    #change tick labels colour
    axis.text.y = element_text(colour = "black"),
    legend.title = element_blank()
  ) + scale_y_continuous(labels = label_comma())

#display our plot
figure

ggsave(path = "figures", paste0(Sys.Date(), "_mesofauna_abundance_per_100_g_dry_soil.svg"), width = 10, height= 5, figure)


#stacked barcharts showing how communities change across site, as % of total number of organisms.

#d$`Mesostigmata per 100 g dry soil mass` <- (d$Mesostigmata/d$`Dry soil mass (g)`)*100
#d$`Oribatida per 100 g dry soil mass` <- (d$Oribatida/d$`Dry soil mass (g)`)*100
#d$`Astigmatina per 100 g dry soil mass` <- (d$Astigmatina/d$`Dry soil mass (g)`)*100
#d$`Prostigmata per 100 g dry soil mass` <- (d$Prostigmata/d$`Dry soil mass (g)`)*100
#d$`Entomobryomorpha per 100 g dry soil mass` <- (d$Entomobryomorpha/d$`Dry soil mass (g)`)*100
#d$`Poduromorpha per 100 g dry soil mass` <- (d$Poduromorpha/d$`Dry soil mass (g)`)*100
#d$`Symphypleona per 100 g dry soil mass` <- (d$Symphypleona/d$`Dry soil mass (g)`)*100
#d$`Neelidae per 100 g dry soil mass` <- (d$Neelidae/d$`Dry soil mass (g)`)*100


# Load necessary libraries
library(tidyverse)

spe <- d[,c(1,2,3, 4,5,6,7,8,9,10,11)]

# Identify species columns (exclude metadata)
species_cols <- setdiff(names(spe), c("Sample ID", "Site", "Vegetation"))

# Step 1: Pivot to long format
df_long <- spe %>%
  pivot_longer(cols = all_of(species_cols),
               names_to = "Species",
               values_to = "Abundance")

# Step 2: Calculate proportional abundance within each Sample
df_prop <- df_long %>%
  group_by(`Sample ID`) %>%
  mutate(Proportion = Abundance / sum(Abundance)) %>%
  ungroup()

# Step 3: Merge in Site and Vegetation info
df_meta <- spe %>% select(`Sample ID`, Site, Vegetation) %>% distinct()
df_prop <- df_prop %>% left_join(df_meta, by = "Sample ID")

# Step 4: Aggregate to Site × Vegetation: compute mean proportion per species
df_summary <- df_prop %>%
  group_by(Site.x, Vegetation.x, Species) %>%
  summarise(Mean_Proportion = mean(Proportion, na.rm = TRUE), .groups = "drop")
#reorder the sites so they are plotted in the order we want
df_summary$Site.x <- factor(df_summary$Site.x, levels = c(
 "Whiteside",  "Haweswater",  "Widdybanks", 
  "Brimham Moor", "Scarth Wood Moor", "Bridestones"
))
# Set custom species stacking order (bottom to top)
df_summary$Species <- factor(df_summary$Species, levels = rev(c(
  "Mesostigmata", "Oribatida", "Astigmatina", "Prostigmata", "Entomobryomorpha", "Poduromorpha", "Symphypleona",  "Neelidae")))


# Split into two datasets: Heath and Bracken
df_heath <- df_summary %>% filter(Vegetation.x == "Heather")
df_bracken <- df_summary %>% filter(Vegetation.x == "Bracken")

# Plot 1: Heath
plot_heath <- ggplot(df_heath, aes(x = Site.x, y = Mean_Proportion, fill = Species)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Species Composition in Heath",
       x = "Site", y = "Mean Proportional Abundance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

# Plot 2: Bracken
plot_bracken <- ggplot(df_bracken, aes(x = Site.x, y = Mean_Proportion, fill = Species)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Species Composition in Bracken",
       x = "Site", y = "Mean Proportional Abundance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

show(plot_heath)
show(plot_bracken)

# Save plots
ggsave(path = "figures", paste0(Sys.Date(), "_mean_species_composition_raw_heath.svg"),
       plot = plot_heath, width = 7, height = 5, dpi = 300)

ggsave(path = "figures", paste0(Sys.Date(), "_mean_species_composition_raw_bracken.svg"),
       plot = plot_bracken, width = 7, height = 5, dpi = 300)


#run a binomial GLMM
library(lme4)

glmm_model <- glmer(
  Mean_Proportion ~ Species * Vegetation.x + (1 | Site.x),
  family = binomial,
  data = df_summary
)

summary(glmm_model)








#save our datafile containg standardized mesofauna abundances
write.csv(d, "data/2025-08-19_standardized-mesofauna-data.csv")

####analyse mesofauna abundance per 100g dry soil ----
hist(d$`Individuals per 100g dry soil`)
#transform the data for a normal distribution
d$`Mesofauna Individuals er 100 g dry soil log x plus 10 transformed` <- log(d$`Individuals per 100g dry soil` + 10)
#the tranformation seems to have worked, giving us something resembling a normal distribution
hist(d$`Mesofauna Individuals er 100 g dry soil log x plus 10 transformed`)

#Type 1 two-way anova using data from all sites
anova <- aov(d$`Mesofauna Individuals er 100 g dry soil log x plus 10 transformed` ~ d$Vegetation * d$Site)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)

#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$`Mesofauna Individuals er 100 g dry soil log x plus 10 transformed` ~ d$Vegetation * d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

#### mesofauna abundance per 100 g dry soil, at each site ----

#brimham 1:20, bridestones 21:40, hawswater 41:60, scarth wood 61:80, widdybanks 81:100, whiteside 101:120
site <- d[(101:120),]
hist(site$`Individuals per 100g dry soil`)
#transform the data for a normal distribution
site$`Mesofauna Individuals per 100 g dry soil log x plus 10 transformed` <- log(site$`Individuals per 100g dry soil` + 10)
#the tranformation seems to have worked, giving us something resembling a normal distribution
hist(site$`Mesofauna Individuals per 100 g dry soil log x plus 10 transformed`)

#Type 1 two-way anova using data from all sites
anova <- aov(site$`Mesofauna Individuals per 100 g dry soil` ~ site$Vegetation)
#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(site$`Mesofauna Individuals per 100 g dry soil log x plus 10 transformed` ~ site$Vegetation)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)


figure <- ggboxplot(site, x = "Vegetation", y = 'Individuals per 100g dry soil', palette = c("limegreen", "#AA4499"), lwd = 0.75)  +
  labs(y = expression("Individuals per 100 g dry soil")) + theme( #remove x axis label
    axis.title.x=element_blank(),
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "black", linewidth = 0.5),
    #change colour and thickness of axis ticks
    axis.ticks = element_line(colour = "black", linewidth = 0.5),
    #change axis labels colour
    axis.title.y = element_text(colour = "black"),
    #change tick labels colour
    axis.text.y = element_text(colour = "black"),
    legend.title = element_blank()
  ) + scale_y_continuous(labels = label_comma())

#display our plot
figure





#### mesofauna abundance per m2 to 10 cm depth analysis ----

figure <- ggboxplot(d, x = "Site", y = '1000s Individuals per m2 to 10 cm depth', color = "Vegetation", palette = c("limegreen", "#AA4499"), lwd = 0.75)  +
  labs(y = expression("1000 individuals per m"^2*" soil (to 10 cm depth)")) + theme( #remove x axis label
    axis.title.x=element_blank(),
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "black", linewidth = 0.5),
    #change colour and thickness of axis ticks
    axis.ticks = element_line(colour = "black", linewidth = 0.5),
    #change axis labels colour
    axis.title.y = element_text(colour = "black"),
    #change tick labels colour
    axis.text.y = element_text(colour = "black"),
    legend.title = element_blank()
  ) + scale_y_continuous(labels = label_comma())

#display our plot
figure

ggsave(path = "figures", paste0(Sys.Date(), "_mesofauna_abundance_per_m2_to_10cm_depth.svg"), width = 10, height= 5, figure)



#Type 1 two-way anova using data from all sites
anova <- aov(d$`1000s Individuals per m2 to 10 cm depth` ~ d$Vegetation*d$Site)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
print(cld)

#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$`1000s Individuals per m2 to 10 cm depth` ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

#now analyse at each site

#Bridestones
bri <- d[(21:40),]
#Type 1 two-way anova using data from all sites
anova <- aov(bri$`SUVA (L mg-1 cm-1)` ~ bri$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Scarth Wood Moor
swm <- d[(61:80),]
#Type 1 two-way anova using data from all sites
anova <- aov(swm$`SUVA (L mg-1 cm-1)` ~ swm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Brimham
bhm <- d[(1:20),]
#Type 1 two-way anova using data from all sites
anova <- aov(bhm$`SUVA (L mg-1 cm-1)` ~ bhm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Widdybanks
wdy <- d[(81:100),]
#Type 1 two-way anova using data from all sites
anova <- aov(wdy$`SUVA (L mg-1 cm-1)`~ wdy$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Haweswater
haw <- d[(41:60),]
#Type 1 two-way anova using data from all sites
anova <- aov(haw$`SUVA (L mg-1 cm-1)` ~ haw$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#Whiteside
whi <- d[(101:120),]
#Type 1 two-way anova using data from all sites
anova <- aov(whi$`SUVA (L mg-1 cm-1)` ~ whi$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)


#also standardize per dry mass soil
d_moisture <- readr::read_csv(
  here::here("data-raw", "project-2-data-master", "individual", "1) Soil Moisture Content.csv")
) 
#read in data for wet mass of tullgren soil - or did we note down dry?
#missing this data 



#### Mesostigmata abundance ANOVAs ----
#transform mesostig abundance data as there are many 0s
d$`Mesostigmata (log (x + 1) transformed)` <- log(d$Mesostigmata + 1)
#anova to see if key metrics differ between site/vegetation
#anova
anova <- aov(d$`Mesostigmata (log (x + 1) transformed)` ~ d$Vegetation * d$Site)
#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$Mesostigmata ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)



#now analyse at each site

#Bridestones
bri <- d[(11:20),]
#Type 1 two-way anova using data from all sites
anova <- aov(bri$`Mesostigmata (log (x + 1) transformed)` ~ bri$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)

#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(bri$Mesostigmata ~ bri$Vegetation*bri$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)


#Scarth Wood Moor
swm <- d[(31:40),]
#Type 1 two-way anova using data from all sites
anova <- aov(swm$Mesostigmata ~ swm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)

#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(swm$Mesostigmata ~ swm$Vegetation*swm$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)


#Brimham
bhm <- d[(1:10),]
#Type 1 two-way anova using data from all sites
anova <- aov(bhm$Mesostigmata ~ bhm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)

#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(bhm$Mesostigmata ~ bhm$Vegetation*bhm$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)



#Widdybanks
wdy <- d[(41:50),]
#Type 1 two-way anova using data from all sites
anova <- aov(wdy$Mesostigmata ~ wdy$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)


#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(wdy$Mesostigmata ~ wdy$Vegetation*wdy$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

#Haweswater
haw <- d[(21:30),]
#Type 1 two-way anova using data from all sites
anova <- aov(haw$Mesostigmata ~ haw$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)

#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(haw$Mesostigmata ~ haw$Vegetation*haw$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)


#Whiteside
whi <- d[(51:60),]
#Type 1 two-way anova using data from all sites
anova <- aov(whi$Mesostigmata ~ whi$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)


#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(whi$Mesostigmata ~ whi$Vegetation*whi$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)


#### Oribatida abundance ANOVAs----
#transform mesostig abundance data as there are many 0s
d$`Oribatida transformed` <- log(d$Oribatida + 10)
#anova to see if key metrics differ between site/vegetation
#anova
anova <- aov(d$`Oribatida transformed` ~ d$Vegetation * d$Site)
#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$`Oribatida transformed` ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)

#graphing boxplots with bracken split from nonbracken
oribatid_bxp <-ggboxplot(d, x = "Site", y = "Oribatida transformed", color = "Vegetation", ylab = "Oribatid Abundances (log(x + 10) transformed)", palette = c("limegreen", "#AA4499"), lwd = 0.75) + theme(
  #remove x axis label
  axis.title.x=element_blank(),
  # Remove panel border
  panel.border = element_blank(),  
  # Remove panel grid lines
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Remove panel background
  panel.background = element_blank(),
  # Add axis line
  axis.line = element_line(colour = "black", linewidth = 0.5),
  #change colour and thickness of axis ticks
  axis.ticks = element_line(colour = "black", linewidth = 0.5),
  #change axis labels colour
  axis.title.y = element_text(colour = "black"),
  #change tick labels colour
  axis.text.y = element_text(colour = "black"),
  legend.title = element_blank()
) 
show(oribatid_bxp)

#save our plot
ggsave(path = "figures", paste0(Sys.Date(), "_oribatid_abundance.svg"), width = 10, height= 5, oribatid_bxp)




#now analyse at each site

#Bridestones
bri <- d[(11:20),]
#Type 1 two-way anova using data from all sites
anova <- aov(bri$`Oribatida` ~ bri$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)

#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(bri$Oribatida ~ bri$Vegetation*bri$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)


#Scarth Wood Moor
swm <- d[(31:40),]
#Type 1 two-way anova using data from all sites
anova <- aov(swm$`Oribatida (log (x + 1) transformed)` ~ swm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)

#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(swm$Oribatida ~ swm$Vegetation*swm$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)


#Brimham
bhm <- d[(1:10),]
#Type 1 two-way anova using data from all sites
anova <- aov(bhm$Oribatida ~ bhm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)

#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(bhm$Oribatida ~ bhm$Vegetation*bhm$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)



#Widdybanks
wdy <- d[(41:50),]
#Type 1 two-way anova using data from all sites
anova <- aov(wdy$Oribatida ~ wdy$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)


#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(wdy$Oribatida ~ wdy$Vegetation*wdy$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

#Haweswater
haw <- d[(21:30),]
#Type 1 two-way anova using data from all sites
anova <- aov(haw$Oribatida ~ haw$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)

#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(haw$Oribatida ~ haw$Vegetation*haw$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)


#Whiteside
whi <- d[(51:60),]
#Type 1 two-way anova using data from all sites
anova <- aov(whi$Oribatida ~ whi$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)


#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(whi$Oribatida ~ whi$Vegetation*whi$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)


#### Astigmatina abundance ANOVAs ----
#transform mesostig abundance data as there are many 0s
d$`Astigmatina (log (x + 10) transformed)` <- log(d$Astigmatina + 10)
hist(d$`Astigmatina (log (x + 10) transformed)`)
#anova to see if key metrics differ between site/vegetation
#anova
anova <- aov(d$`Astigmatina (log (x + 10) transformed)` ~ d$Vegetation * d$Site)
#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$Mesostigmata ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)





#graphing boxplots with bracken split from nonbracken
astig_bxp <-ggboxplot(d, x = "Site", y = "`Astigmatina (log (x + 10) transformed)`", color = "Vegetation", ylab = "Astigmatina (log (x + 10) transformed) Abundances", palette = c("limegreen", "#AA4499"), lwd = 0.75) + theme(
  #remove x axis label
  axis.title.x=element_blank(),
  # Remove panel border
  panel.border = element_blank(),  
  # Remove panel grid lines
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Remove panel background
  panel.background = element_blank(),
  # Add axis line
  axis.line = element_line(colour = "black", linewidth = 0.5),
  #change colour and thickness of axis ticks
  axis.ticks = element_line(colour = "black", linewidth = 0.5),
  #change axis labels colour
  axis.title.y = element_text(colour = "black"),
  #change tick labels colour
  axis.text.y = element_text(colour = "black"),
  legend.title = element_blank()
) 
show(astig_bxp)

#now analyse at each site

#Bridestones
bri <- d[(11:20),]
#Type 1 two-way anova using data from all sites
anova <- aov(bri$Astigmatina ~ bri$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)

#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(bri$Astigmatina ~ bri$Vegetation*bri$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)


#Scarth Wood Moor
swm <- d[(31:40),]
#Type 1 two-way anova using data from all sites
anova <- aov(swm$`Astigmatina (log (x + 10) transformed)` ~ swm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)

#check homogeneity of variance
plot(anova, 1)
colnames(swm)[30] <- "Astigmatina transformed"
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(swm$`Astigmatina transformed` ~ swm$Vegetation*swm$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)


#Brimham
bhm <- d[(1:10),]
#Type 1 two-way anova using data from all sites
anova <- aov(bhm$`Astigmatina (log (x + 10) transformed)` ~ bhm$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)

#check homogeneity of variance
plot(anova, 1)
#rename otherwise levene test won't work
colnames(bhm)[30] <- "Astigmatina transformed"
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(bhm$`Astigmatina transformed` ~ bhm$Vegetation*bhm$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)



#Widdybanks
wdy <- d[(41:50),]
#Type 1 two-way anova using data from all sites
anova <- aov(wdy$`Astigmatina (log (x + 10) transformed)` ~ wdy$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)


#check homogeneity of variance
plot(anova, 1)
colnames(wdy)[30] <- "Astigmatina transformed"
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(wdy$`Astigmatina transformed` ~ wdy$Vegetation*wdy$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

#Haweswater
haw <- d[(21:30),]
#Type 1 two-way anova using data from all sites
anova <- aov(haw$Astigmatina ~ haw$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)

#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(haw$Astigmatina ~ haw$Vegetation*haw$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)


#Whiteside
whi <- d[(51:60),]
#Type 1 two-way anova using data from all sites
anova <- aov(whi$Astigmatina ~ whi$Vegetation)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)


#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(whi$Astigmatina ~ whi$Vegetation*whi$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)


#### Prostigmata abundance ANOVAs ----

#transform the data to pass Shapiro
d$`Prostigmata (log transformed)` <- log(d$Prostigmata + 10)
#anova to see if key metrics differ between site/vegetation
#anova
anova <- aov(d$`Prostigmata (log transformed)` ~ d$Vegetation * d$Site)
#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$`Prostigmata (log transformed)` ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)




#graphing boxplots with bracken split from nonbracken
prostig_bxp <-ggboxplot(d, x = "Site", y = "`Prostigmata (log transformed)`", color = "Vegetation", ylab = "Prostigmata (log (x + 10) transformed) Abundances", palette = c("limegreen", "#AA4499"), lwd = 0.75) + theme(
  #remove x axis label
  axis.title.x=element_blank(),
  # Remove panel border
  panel.border = element_blank(),  
  # Remove panel grid lines
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Remove panel background
  panel.background = element_blank(),
  # Add axis line
  axis.line = element_line(colour = "black", linewidth = 0.5),
  #change colour and thickness of axis ticks
  axis.ticks = element_line(colour = "black", linewidth = 0.5),
  #change axis labels colour
  axis.title.y = element_text(colour = "black"),
  #change tick labels colour
  axis.text.y = element_text(colour = "black"),
  legend.title = element_blank()
) 
show(prostig_bxp)



#### Entomobryomorpha abundance ANOVAs ----

#transform the data to pass Shapiro
hist(d$Entomobryomorpha)
d$`Entomobryomorpha (log transformed)` <- log(d$Entomobryomorpha + 10)
hist(d$`Entomobryomorpha (log transformed)`)
#anova to see if key metrics differ between site/vegetation
#anova
anova <- aov(d$`Entomobryomorpha (log transformed)` ~ d$Vegetation * d$Site)
#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$`Entomobryomorpha (log transformed)` ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)







#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)




#graphing boxplots with bracken split from nonbracken
entomo_bxp <-ggboxplot(d, x = "Site", y = "`Entomobryomorpha (log transformed)`", color = "Vegetation", ylab = "Entomobryomorpha abundance (log (x + 10) transformed)", palette = c("limegreen", "#AA4499"), lwd = 0.75) + theme(
  #remove x axis label
  axis.title.x=element_blank(),
  # Remove panel border
  panel.border = element_blank(),  
  # Remove panel grid lines
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Remove panel background
  panel.background = element_blank(),
  # Add axis line
  axis.line = element_line(colour = "black", linewidth = 0.5),
  #change colour and thickness of axis ticks
  axis.ticks = element_line(colour = "black", linewidth = 0.5),
  #change axis labels colour
  axis.title.y = element_text(colour = "black"),
  #change tick labels colour
  axis.text.y = element_text(colour = "black"),
  legend.title = element_blank()
) 
show(entomo_bxp)

#save our plot
ggsave(path = "figures", paste0(Sys.Date(), "_entomobryomorpha_abundance.svg"), width = 10, height= 5, entomo_bxp)






#### Poduromorpha abundance ANOVAs ----

#transform the data to pass Shapiro
hist(d$Poduromorpha)
d$`Poduromorpha (log transformed)` <- log(d$Poduromorpha + 1)
hist(d$`Poduromorpha (log transformed)`)
#anova to see if key metrics differ between site/vegetation
#anova
anova <- aov(d$`Poduromorpha (log transformed)` ~ d$Vegetation * d$Site)
#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$`Poduromorpha (log transformed)` ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#get the p values
print(tukey)



#graphing boxplots with bracken split from nonbracken
poduro_bxp <-ggboxplot(d, x = "Site", y = "`Poduromorpha (log transformed)`", color = "Vegetation", ylab = "Poduromorpha abundance (log (x + 1) transformed)", palette = c("limegreen", "#AA4499"), lwd = 0.75) + theme(
  #remove x axis label
  axis.title.x=element_blank(),
  # Remove panel border
  panel.border = element_blank(),  
  # Remove panel grid lines
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Remove panel background
  panel.background = element_blank(),
  # Add axis line
  axis.line = element_line(colour = "black", linewidth = 0.5),
  #change colour and thickness of axis ticks
  axis.ticks = element_line(colour = "black", linewidth = 0.5),
  #change axis labels colour
  axis.title.y = element_text(colour = "black"),
  #change tick labels colour
  axis.text.y = element_text(colour = "black"),
  legend.title = element_blank()
) 
show(poduro_bxp)

#save our plot
ggsave(path = "figures", paste0(Sys.Date(), "_poduromorpha_abundance.svg"), width = 10, height= 5, poduro_bxp)










#fucking space!

#### Symphypleona abundance ANOVAs ----

#transform the data to pass Shapiro
hist(d$Symphypleona)
d$`Symphypleona (log transformed)` <- (d$Symphypleona)^(1/3)
hist(d$`Symphypleona (log transformed)`)
#anova to see if key metrics differ between site/vegetation
#anova
anova <- aov(d$`Symphypleona (log transformed)` ~ d$Vegetation * d$Site)
#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$`Symphypleona (log transformed)` ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#get the p values
print(tukey)



#graphing boxplots with bracken split from nonbracken
symphy_bxp <-ggboxplot(d, x = "Site", y = "`Symphypleona (log transformed)`", color = "Vegetation", ylab = "Symphypleona abundance (cube root transformed)", palette = c("limegreen", "#AA4499"), lwd = 0.75) + theme(
  #remove x axis label
  axis.title.x=element_blank(),
  # Remove panel border
  panel.border = element_blank(),  
  # Remove panel grid lines
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Remove panel background
  panel.background = element_blank(),
  # Add axis line
  axis.line = element_line(colour = "black", linewidth = 0.5),
  #change colour and thickness of axis ticks
  axis.ticks = element_line(colour = "black", linewidth = 0.5),
  #change axis labels colour
  axis.title.y = element_text(colour = "black"),
  #change tick labels colour
  axis.text.y = element_text(colour = "black"),
  legend.title = element_blank()
) 
show(symphy_bxp)

#save our plot
ggsave(path = "figures", paste0(Sys.Date(), "_symphypleona_abundance.svg"), width = 10, height= 5, symphy_bxp)










#fucking space!



#space break

#### Neelidae abundance ANOVAs ----

#transform the data to pass Shapiro
hist(d$Neelidae)
d$`Neelidae (log transformed)` <- log(d$Neelidae + 1)
hist(d$`Neelidae (log transformed)`)
#anova to see if key metrics differ between site/vegetation
#anova
anova <- aov(d$`Neelidae (log transformed)` ~ d$Vegetation * d$Site)
#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$`Neelidae (log transformed)` ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)
#get the p values
print(tukey)



#graphing boxplots with bracken split from nonbracken
neeli_bxp <-ggboxplot(d, x = "Site", y = "`Neelidae (log transformed)`", color = "Vegetation", ylab = "Neelidae (log(x + 1) transformed)", palette = c("limegreen", "#AA4499"), lwd = 0.75) + theme(
  #remove x axis label
  axis.title.x=element_blank(),
  # Remove panel border
  panel.border = element_blank(),  
  # Remove panel grid lines
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Remove panel background
  panel.background = element_blank(),
  # Add axis line
  axis.line = element_line(colour = "black", linewidth = 0.5),
  #change colour and thickness of axis ticks
  axis.ticks = element_line(colour = "black", linewidth = 0.5),
  #change axis labels colour
  axis.title.y = element_text(colour = "black"),
  #change tick labels colour
  axis.text.y = element_text(colour = "black"),
  legend.title = element_blank()
) 
show(neeli_bxp)

#save our plot
ggsave(path = "figures", paste0(Sys.Date(), "_neelidae_abundance.svg"), width = 10, height= 5, neeli_bxp)





#### Week 1 + 2 alpha diversity metrics ----
d <- readr::read_csv(
  here::here("data", "n = 10 Tullgren Extracts - Week 1 + 2 Extracts.csv")
) 
#order samples by ID alphabetically
d <- arrange(d, d["Sample ID"])
d <- as.data.frame(d)
#order the sites as they should appear on the graph from west to east
d$Site <- factor(d$Site, levels = c("Whiteside", "Haweswater", "Widdybanks", "Brimham Moor", "Scarth Wood Moor", "Bridestones"))
#replace null (empty excell cell) with "0"
d[is.na(d)] <- 0
#total mesofauna abundances
d$`Total Mesofauna Catch`<- rowSums(d[,4:11])
#total invertebrate abundances
d$`Total Invertebrate Catch`<- rowSums(d[,4:27])
#shannon diversity of the 8 mesofauna groups
d$`Mesofauna Shannon` <- diversity(d[,4:11], "shannon")
#simpson diversity of the 8 mesofauna groups
d$`Mesofauna Simpson` <- diversity(d[,4:11], "simpson")

#anova to see if key metrics differ between site/vegetation
#anova
anova <- aov(d$`Total Mesofauna Catch` ~ d$Vegetation * d$Site)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)

#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$`Total Mesofauna Catch` ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)



anova <- aov(d$`Total Invertebrate Catch` ~ d$Vegetation * d$Site)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)

anova <- aov(d$`Mesofauna Shannon` ~ d$Vegetation * d$Site)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)

anova <- aov(d$`Mesofauna Simpson` ~ d$Vegetation * d$Site)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)

anova <- aov(d$`Neelidae` ~ d$Vegetation * d$Site)
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)


#graphing boxplots with bracken split from nonbracken
shannon_bxp <-ggboxplot(d, x = "Site", y = "Mesofauna Shannon", color = "Vegetation", ylab = "Shannon Diversity", palette = c("limegreen", "#AA4499"), lwd = 0.75) + theme(
  #remove x axis label
  axis.title.x=element_blank(),
  # Remove panel border
  panel.border = element_blank(),  
  # Remove panel grid lines
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Remove panel background
  panel.background = element_blank(),
  # Add axis line
  axis.line = element_line(colour = "black", linewidth = 0.5),
  #change colour and thickness of axis ticks
  axis.ticks = element_line(colour = "black", linewidth = 0.5),
  #change axis labels colour
  axis.title.y = element_text(colour = "black"),
  #change tick labels colour
  axis.text.y = element_text(colour = "black"),
  legend.title = element_blank()
) 
show(shannon_bxp)

#save our plot
ggsave(path = "figures", paste0(Sys.Date(), "_mesofauna_shannon.svg"), width = 10, height= 5, shannon_bxp)

#graphing boxplots with bracken split from nonbracken
simpson_bxp <-ggboxplot(d, x = "Site", y = "Mesofauna Simpson", color = "Vegetation", ylab = "Simpson Diversity", palette = c("limegreen", "#AA4499"), lwd = 0.75) + theme(
  #remove x axis label
  axis.title.x=element_blank(),
  # Remove panel border
  panel.border = element_blank(),  
  # Remove panel grid lines
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Remove panel background
  panel.background = element_blank(),
  # Add axis line
  axis.line = element_line(colour = "black", linewidth = 0.5),
  #change colour and thickness of axis ticks
  axis.ticks = element_line(colour = "black", linewidth = 0.5),
  #change axis labels colour
  axis.title.y = element_text(colour = "black"),
  #change tick labels colour
  axis.text.y = element_text(colour = "black"),
  legend.title = element_blank()
) 
show(simpson_bxp)

#save our plot
ggsave(path = "figures", paste0(Sys.Date(), "_mesofauna_simpson.svg"), width = 10, height= 5, simpson_bxp)

#### all sites mesofauna Shannon ANOVAs ----
#check data distribution
hist(d$`Mesofauna Shannon`)
anova <- aov(d$`Mesofauna Shannon` ~ d$Vegetation * d$Site)
#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$`Mesofauna Shannon` ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)





#### site specific mesofauna Shannon ANOVAS ----
#brimham 1:20, bridestones 21:40, hawswater 41:60, scarth wood 61:80, widdybanks 81:100, whiteside 101:120
site <- d[(101:120),]
hist(site$`Mesofauna Shannon`)
#transform the data for a normal distribution
site$`Mesofauna Shannon trans` <- log(site$`Mesofauna Shannon` + 10)
#the tranformation seems to have worked, giving us something resembling a normal distribution
hist(site$`Mesofauna Shannon trans`)

#Type 1 two-way anova using data from all sites
anova <- aov(site$`Mesofauna Shannon trans` ~ site$Vegetation)
#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(site$`Mesofauna Shannon trans` ~ site$Vegetation)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)


figure <- ggboxplot(site, x = "Vegetation", y = 'Individuals per 100g dry soil', palette = c("limegreen", "#AA4499"), lwd = 0.75)  +
  labs(y = expression("Individuals per 100 g dry soil")) + theme( #remove x axis label
    axis.title.x=element_blank(),
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "black", linewidth = 0.5),
    #change colour and thickness of axis ticks
    axis.ticks = element_line(colour = "black", linewidth = 0.5),
    #change axis labels colour
    axis.title.y = element_text(colour = "black"),
    #change tick labels colour
    axis.text.y = element_text(colour = "black"),
    legend.title = element_blank()
  ) + scale_y_continuous(labels = label_comma())

#display our plot
figure

 #or do t tests for site specific?
#### all sites mesofauna simpson ANOVAs ----

#anova to see if key metrics differ between site/vegetation
#anova
hist(d$`Mesofauna Simpson`)
#transform the data - data transformations not working for SImpson
d$`Mesofauna Simpson transformed` <- log10(d$`Mesofauna Simpson`)
hist(d$`Mesofauna Simpson transformed`)

anova <- aov(d$`Mesofauna Simpson` ~ d$Vegetation * d$Site)
#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$`Mesofauna Simpson` ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)


summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)





#### site specific mesofauna Simpson ANOVAS ----
#brimham 1:20, bridestones 21:40, hawswater 41:60, scarth wood 61:80, widdybanks 81:100, whiteside 101:120
site <- d[(101:120),]
hist(site$`Mesofauna Simpson`)
#transform the data for a normal distribution
site$`Mesofauna Simpson trans` <- log(site$`Mesofauna Simpson` + 10)
#the tranformation seems to have worked, giving us something resembling a normal distribution
hist(site$`Mesofauna Simpson trans`)

#Type 1 two-way anova using data from all sites
anova <- aov(site$`Mesofauna Simpson` ~ site$Vegetation)
#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(site$`Mesofauna Simpson` ~ site$Vegetation)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

summary(anova)


#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)


figure <- ggboxplot(site, x = "Vegetation", y = 'Mesofauna Simpson', palette = c("limegreen", "#AA4499"), lwd = 0.75)  +
  labs(y = expression("Simpson")) + theme( #remove x axis label
    axis.title.x=element_blank(),
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "black", linewidth = 0.5),
    #change colour and thickness of axis ticks
    axis.ticks = element_line(colour = "black", linewidth = 0.5),
    #change axis labels colour
    axis.title.y = element_text(colour = "black"),
    #change tick labels colour
    axis.text.y = element_text(colour = "black"),
    legend.title = element_blank()
  ) + scale_y_continuous(labels = label_comma())

#display our plot
figure

#or do t tests for site specific?



#### compare week 1 vs week 2 extracts----
wk1 <- readr::read_csv(
  here::here("data", "n = 10 Tullgren Extracts - Week 1 Extracts.csv")
) 
wk1$Week <- c(rep("Week 1", 120))
wk1 <- as.data.frame(wk1)
#replace null (empty excell cell) with "0"
wk1[is.na(wk1)] <- 0

wk1plus2 <- readr::read_csv(
  here::here("data", "n = 10 Tullgren Extracts - Week 1 + 2 Extracts.csv")
) 
wk1plus2$Week <- c(rep("Week 1 + 2", 120))
wk1plus2 <- as.data.frame(wk1plus2)
#replace null (empty excell cell) with "0"
wk1plus2[is.na(wk1plus2)] <- 0

#compare abundances
t.test(wk1$Mesostigmata, wk1plus2$Mesostigmata)
t.test(wk1$Oribatida, wk1plus2$Oribatida)
t.test(wk1$Astigmatina, wk1plus2$Astigmatina)
t.test(wk1$Prostigmata, wk1plus2$Prostigmata)
t.test(wk1$Entomobryomorpha, wk1plus2$Entomobryomorpha)
t.test(wk1$Poduromorpha, wk1plus2$Poduromorpha)
t.test(wk1$Symphypleona, wk1plus2$Symphypleona)
t.test(wk1$Neelidae, wk1plus2$Neelidae)

#compare totals
wk1$`Total Mesofauna` <- rowSums(wk1[, 4:11])
wk1plus2$`Total Mesofauna` <- rowSums(wk1plus2[, 4:11])
t.test(wk1$`Total Mesofauna`, wk1plus2$`Total Mesofauna`)
wk1$`Total Invert` <- rowSums(wk1[, 4:27])
wk1plus2$`Total Invert` <- rowSums(wk1plus2[, 4:27])
t.test(wk1$`Total Invert`, wk1plus2$`Total Invert`)

#compare community composition


combined_data <- rbind(wk1, wk1plus2)

# Separate species data and metadata
species_data <- combined_data[, !(names(combined_data) %in% "Week")]
#just the counts
species_data <- species_data[, 4:11]
group <- combined_data$Week

#test whether community composition differs between the weeks on all inverts
adonis_result <- adonis2(species_data ~ group, method = "bray")
print(adonis_result)
#visualise the two
nmds <- metaMDS(species_data, distance = "bray", k = 2, trymax = 100)
plot(nmds, type = "n")

# Add points by group
points(nmds, display = "sites", col = as.factor(group), pch = 19)
legend("topright", legend = unique(group), col = 1:length(unique(group)), pch = 19)

#run an anosim - when grouping by vegetation
ano = anosim(as.matrix(species_data), grouping = group, permutations = 9999, distance = "bray")
# When interpreting these results you want to look at the ANOSIM statistic R and the Significance values. A Significance value less than 0.05 is generally considered to be statistically significant, and means the null hypothesis can be rejected. “The ANOSIM statistic “R” compares the mean of ranked dissimilarities between groups to the mean of ranked dissimilarities within groups. An R value close to “1.0” suggests dissimilarity between groups while an R value close to “0” suggests an even distribution of high and low ranks within and between groups” (GUSTAME). In other words, the higher the R value, the more dissimilar your groups are in terms of microbial community composition.
ano
plot(ano)

#space

#### Week 1 extracts mesofauna NMDS ----
d <- readr::read_csv(
  here::here("data", "n = 10 Tullgren Extracts - Week 1 Extracts.csv")
) 
#order samples by ID alphabetically
d <- arrange(d, d["Sample ID"])
d <- as.data.frame(d)
#replace null (empty excell cell) with "0"
d[is.na(d)] <- 0
#replace row index with sample names
rownames(d) <- d[,1]
#just the morphospecies counts
#spe <- d[,-(1:3)]
#just the mite and springtail groups
spe <- d[, (4:11)]
spe <- as.matrix(spe)

#all morphospecies
#spe <- all_data[,51:436]
#replace row index with sample names
rownames(spe) <- d[,1]
spe <- as.matrix(spe)

#k is the number of reduced dimensions
#trymax sets the default number of iterations
example_NMDS <- metaMDS(spe, distance = "bray", k = 2, maxit = 999, trymax = 500)
#Shephard plot shows scatter around the regession between the interpoint distances in the final configuration (i.e. the distances between each pair of communities) against their original dissimilarities.  Large scatter around the line suggests the original dissimilarities are not well preserved in the reduced number of dimensions
stressplot(example_NMDS)

#plot the NMDS
plot(example_NMDS, col = "white")


#assign the treatments to relevant rows of the dataframe
treat=c(rep("Brimham Bracken",10),rep("Brimham Heath",10), rep("Bridestones Bracken",10),rep("Bridestones Heath",10), rep("Haweswater Bracken", 10), rep("Haweswater Heath", 10), rep("Widdybanks Bracken", 10), rep("Widdybanks Heath", 10), rep("Whiteside Bracken", 10), rep("Whiteside Heath", 10))
#set the colour for each treatment
#colors =c(rep("#44AA99",5),rep("#117733",5), rep("#88CCEE",5),rep("#332288",5), rep("#AA4499", 5), rep("#882255", 5)) 
colors =c(rep("#999999",10),rep("#E69F00",10), rep("#56B4E9",10),rep("#009E73",10), rep("#CC79A7", 10), rep("#0072B2", 10), rep("black",10),rep("green",10), rep("purple", 10), rep("red", 10)) 
#shapes for point codes
pchs<- c(rep(15, 10), rep(0, 10), rep(16, 10), rep(1, 10), rep(17, 10), rep(2, 10), rep(18, 10), rep(3, 10), rep(19, 10), rep(4, 10))
#display the stress for all morphotypes
#text(-0.8,1.4, paste("Stress = ", round(example_NMDS$stress, 3)))
#display the stress for only mites and springtails
text(-2,1.3, paste("Stress = ", round(example_NMDS$stress, 3)))
#visualise the points and ellipses
for(i in unique(treat)) {
  #we have added an if statement so we can chose which points and ellipses to plot at a time e.g. i == "Grassland Bracken".  If we want to plot all ellipses simultaneously, set i == i
  if(i == i){
    #plot the sample IDs on the NMDS, with the colour specific to the treatment
    # orditorp(example_NMDS$point[grep(i,treat),],display="sites", col=colors[grep(i,treat)], cex=0.7, air=0.01)
    #plot point codes for each site
    points(example_NMDS$point[grep(i,treat),], pch = pchs[grep(i,treat)], col = colors[grep(i,treat)], cex = 0.7)
    #plots ellipse with ellipse centered on the centroid of the samples from the same treatment (and thus encapsulating 95% of the variance)
    ordiellipse(example_NMDS$point[grep(i,treat),],kind = "se", conf = 0.95, draw="polygon",
                groups=treat[treat==i],col=colors[grep(i,treat)],label=F) } }



#overlay mesofauna morphotypes (instrinsic variables)
meso.intfit <- envfit(example_NMDS, spe, permutations = 999)
dev.new()
ordiplot(example_NMDS, type = "n", main = "intrinsic variables")
#orditorp(example_NMDS, display = "sites", labels = F, pch = c(16, 8, 17, 18) [as.numeric(env$`CN ratio`)], col = c("green", "blue", "orange", "black") [as.numeric(env$`CN ratio`)], cex = 1)
for(i in unique(treat)) {
  #we have added an if statement so we can chose which points and ellipses to plot at a time e.g. i == "Grassland Bracken".  If we want to plot all ellipses simultaneously, set i == i
  if(i == i){
    #plot the sample IDs on the NMDS, with the colour specific to the treatment
    # orditorp(example_NMDS$point[grep(i,treat),],display="sites", col=colors[grep(i,treat)], cex=0.7, air=0.01)
    #plot point codes for each site
    points(example_NMDS$point[grep(i,treat),], pch = pchs[grep(i,treat)], col = colors[grep(i,treat)], cex = 0.7)
    #plots ellipse with ellipse centered on the centroid of the samples from the same treatment (and thus encapsulating 95% of the variance)
    ordiellipse(example_NMDS$point[grep(i,treat),],kind = "se", conf = 0.95, draw="polygon",
                groups=treat[treat==i],col=colors[grep(i,treat)],label=F) } }

plot(meso.intfit, col = "black", cex = 0.7)
legend(-1,0, legend=c("Brimham Bracken", "Brimham Heath", "Bridestones Bracken", "Bridestones Heath", "Haweswater Bracken", "Haweswater Heath", "Widdybanks Bracken", "Widdybanks Heath", "Whiteside Bracken", "Whiteside Heath"), col = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#0072B2", "black", "green", "purple", "red"), pch = c(15, 0,16,1,17,2, 18, 3, 19, 4))



#overlay environmental variables
#load the field data
all_data <- readr::read_csv(
  here::here("data", "all-sites_field-data.csv"), show_col_types = FALSE
) 
#order samples by ID alphabetically
all_data <- arrange(all_data, all_data["SampleID"])
all_data <- as.data.frame(all_data)
#replace null (empty excell cell) with "0"
all_data[is.na(all_data)] <- 0

env <- all_data[, 6:21]
rownames(env) <- all_data[, 1]
meso.envfit <- envfit(example_NMDS, env, permutations = 999)

ordiplot(example_NMDS, type = "n")
#orditorp(example_NMDS, display = "sites", labels = F, pch = c(16, 8, 17, 18) [as.numeric(env$`CN ratio`)], col = c("green", "blue", "orange", "black") [as.numeric(env$`CN ratio`)], cex = 1)
for(i in unique(treat)) {
  #we have added an if statement so we can chose which points and ellipses to plot at a time e.g. i == "Grassland Bracken".  If we want to plot all ellipses simultaneously, set i == i
  if(i == i){
    #plot the sample IDs on the NMDS, with the colour specific to the treatment
    # orditorp(example_NMDS$point[grep(i,treat),],display="sites", col=colors[grep(i,treat)], cex=0.7, air=0.01)
    #plot point codes for each site
    points(example_NMDS$point[grep(i,treat),], pch = pchs[grep(i,treat)], col = colors[grep(i,treat)], cex = 0.7)
    #plots ellipse with ellipse centered on the centroid of the samples from the same treatment (and thus encapsulating 95% of the variance)
    ordiellipse(example_NMDS$point[grep(i,treat),],kind = "se", conf = 0.95, draw="polygon",
                groups=treat[treat==i],col=colors[grep(i,treat)],label=F) } }

plot(meso.envfit, col = "black", cex = 0.7)


#save the file using Export -> Save As Image -> Width = 655, Height = 500 

# do PERMANOVA analysis
#data frame containing the independent variables (Habitat, Vegetation) we shall be using in our PERMANOVA
idvs <- d[,(2:3)]
#run the permanova
morph_permanova <- adonis2(spe ~ Site*Vegetation, idvs, permutations = 999, method = "bray", by = "terms")
morph_permanova


#run an ANOSIM. The ANOSIM test is similar to an ANOVA hypothesis test, but it uses a dissimilarity matrix as input instead of raw data. It is also non-parametric, meaning it doesn’t assume much about your data (like normal distribution etc), so it’s a good bet for often-skewed microbial abundance data. As a non-parametric test, ANOSIM uses ranked dissimilarities instead of actual distances, and in this way it’s a very nice complement to an NMDS plot. The main point of the ANOSIM test is to determine if the differences between two or more groups are significant.
#run an anosim - when grouping by habitat
ano = anosim(as.matrix(spe), grouping = all_data$Site, permutations = 9999, distance = "bray")
#check output of anosim
ano
plot(ano)
#run an anosim - when grouping by vegetation
ano = anosim(as.matrix(spe), grouping = all_data$Vegetation, permutations = 9999, distance = "bray")
# When interpreting these results you want to look at the ANOSIM statistic R and the Significance values. A Significance value less than 0.05 is generally considered to be statistically significant, and means the null hypothesis can be rejected. “The ANOSIM statistic “R” compares the mean of ranked dissimilarities between groups to the mean of ranked dissimilarities within groups. An R value close to “1.0” suggests dissimilarity between groups while an R value close to “0” suggests an even distribution of high and low ranks within and between groups” (GUSTAME). In other words, the higher the R value, the more dissimilar your groups are in terms of microbial community composition.
ano
plot(ano)





#### Week 1 + 2 extracts mesofauna NMDS ----
d <- readr::read_csv(
  here::here("data", "2025-08-19_dbRDA_masterfile.csv")
) 
d <- d[, c(1, 70, 71, 72, 73, 74, 75, 76,77)]

#order samples by ID alphabetically
d <- arrange(d, d["Sample ID"])
d <- as.data.frame(d)
#replace null (empty excell cell) with "0"
d[is.na(d)] <- 0
#replace row index with sample names
rownames(d) <- d[,1]
#just the morphospecies counts
#spe <- d[,-(1:3)]
#just the mite and springtail groups
#spe <- d[, (4:11)]
spe <- d[, (2:9)]
spe <- as.matrix(spe)

#some initial data exploration

#MANOVA - to determine if the treatment (bracken vs heath) signficiantly affects the functional groups
#Mesostigmata, Oribatida, Astigmatina, Prostigmata, Symphypleona, Entomobryomorpha, Poduromorpha, Neelidae
#result = manova(cbind(Symphypleona, Entomobryomorpha, Poduromorpha, Neelidae) ~ Vegetation*Site, data = d)
#summary(result)

#k is the number of reduced dimensions
#trymax sets the default number of iterations
example_NMDS <- metaMDS(spe, distance = "bray", k = 2, maxit = 999, trymax = 500)
#Shephard plot shows scatter around the regession between the interpoint distances in the final configuration (i.e. the distances between each pair of communities) against their original dissimilarities.  Large scatter around the line suggests the original dissimilarities are not well preserved in the reduced number of dimensions
stressplot(example_NMDS)

dev.new()
#plot the NMDS
plot(example_NMDS, col = "white")



#assign the treatments to relevant rows of the dataframe
treat=c(rep("Brimham Bracken",10),rep("Brimham Heath",10), rep("Bridestones Bracken",10),rep("Bridestones Heath",10), rep("Haweswater Bracken", 10), rep("Haweswater Heath", 10), rep("Scarth Wood Bracken", 10), rep("Scarth Wood Heath", 10), rep("Widdybanks Bracken", 10), rep("Widdybanks Heath", 10), rep("Whiteside Bracken", 10), rep("Whiteside Heath", 10))
#set the colour for each treatment
#colors =c(rep("#44AA99",5),rep("#117733",5), rep("#88CCEE",5),rep("#332288",5), rep("#AA4499", 5), rep("#882255", 5)) 
colors =c(rep("#999999",20),rep("#E69F00",20), rep("#56B4E9",20),rep("#009E73",20), rep("#CC79A7", 20), rep("#0072B2", 20)) 
#shapes for point codes
pchs<- c(rep(15, 10), rep(0, 10), rep(16, 10), rep(1, 10), rep(17, 10), rep(2, 10), rep(18, 10), rep(3, 10), rep(19, 10), rep(4, 10), rep(20, 10), rep(5, 10))

#colors =c(rep("green",10),rep("purple",10), rep("green",10),rep("purple",10),rep("green",10),rep("purple",10),rep("green",10),rep("purple",10),rep("green",10),rep("purple",10),rep("green",10),rep("purple",10)) 

#display the stress for all morphotypes
#text(-0.8,1.4, paste("Stress = ", round(example_NMDS$stress, 3)))
#display the stress for only mites and springtails
text(-1,0.5, paste("Stress = ", round(example_NMDS$stress, 3)))
#visualise the points and ellipses
for(i in unique(treat)) {
  #we have added an if statement so we can chose which points and ellipses to plot at a time e.g. i == "Grassland Bracken".  If we want to plot all ellipses simultaneously, set i == i
  if(i == i){
    #plot the sample IDs on the NMDS, with the colour specific to the treatment
    # orditorp(example_NMDS$point[grep(i,treat),],display="sites", col=colors[grep(i,treat)], cex=0.7, air=0.01)
    #plot point codes for each site
    points(example_NMDS$point[grep(i,treat),], pch = pchs[grep(i,treat)], col = colors[grep(i,treat)], cex = 0.7)
    #plots ellipse with ellipse centered on the centroid of the samples from the same treatment (and thus encapsulating 95% of the variance)
    ordiellipse(example_NMDS$point[grep(i,treat),],kind = "se", conf = 0.95, draw="polygon",
                groups=treat[treat==i],col=colors[grep(i,treat)],label=F) } }

legend(-1.2,-0.1, legend=c("Brimham Bracken", "Brimham Heath", "Bridestones Bracken", "Bridestones Heath", "Haweswater Bracken", "Haweswater Heath", "Scarth Wood Bracken", "Scarth Wood Heath", "Widdybanks Bracken", "Widdybanks Heath", "Whiteside Bracken", "Whiteside Heath"), col = c("#999999", "#999999","#E69F00","#E69F00", "#56B4E9", "#56B4E9", "#009E73","#009E73", "#CC79A7","#CC79A7", "#0072B2","#0072B2"), pch = c(15, 0,16,1,17,2, 18, 3, 19, 4, 20, 5))


#split into one NMDS for bracken to see if there is convergence, one for heather to see if there is convergence

####now NMDS just with bracken vs non bracken, never mind the site----
d <- readr::read_csv(
  here::here("data", "2025-08-19_dbRDA_masterfile.csv")
) 
d <- d[, c(1, 4, 70, 71, 72, 73, 74, 75, 76,77)]

#order samples by ID alphabetically
d <- arrange(d, d["Vegetation"])
d <- as.data.frame(d)
#replace null (empty excell cell) with "0"
d[is.na(d)] <- 0
#replace row index with sample names
rownames(d) <- d[,1]
#just the morphospecies counts
#spe <- d[,-(1:3)]
#just the mite and springtail groups
#spe <- d[, (4:11)]
spe <- d[, (3:10)]
spe <- as.matrix(spe)

#make sure our variables are coded as factors
d$Vegetation <- factor(d$Vegetation, levels = c("Bracken", "Heather"), labels = c("Bracken", "Heather"))

#k is the number of reduced dimensions
#trymax sets the default number of iterations
example_NMDS <- metaMDS(spe, distance = "bray", k = 2, maxit = 999, trymax = 500)
#Shephard plot shows scatter around the regession between the interpoint distances in the final configuration (i.e. the distances between each pair of communities) against their original dissimilarities.  Large scatter around the line suggests the original dissimilarities are not well preserved in the reduced number of dimensions
stressplot(example_NMDS)
#set dimensions of new graphics window
#dev.new(width = 719, height = 412, unit = "px")
#plot the NMDS
plot(example_NMDS, col = "white")
#assign the treatments to relevant rows of the dataframe
treat=c(rep("Bracken",60),rep("Heather",60))
#set the colour for each treatment
colors=c(rep("#117733",60), rep("#AA4499", 60))
text(-1,0.3, paste("Stress = ", round(example_NMDS$stress, 3)))

for(i in unique(treat)) {
  #we have added an if statement so we can chose which points and ellipses to plot at a time e.g. i == "Grassland Bracken".  If we want to plot all ellipses simultaneously, set i == i
  if(i == i){
    #change the colour of each site name so samples from the same treatment have the same colour
    orditorp(example_NMDS$point[grep(i,treat),],display="sites", col=colors[grep(i,treat)], cex=0.7,air=0.01)
    #plots ellipse with ellipse centered on the centroid of the samples from the same treatment (and thus encapsulating 95% of the variance)
    ordiellipse(example_NMDS$point[grep(i,treat),],draw="polygon",
                groups=treat[treat==i],col=colors[grep(i,treat)],label=F) } 
}
#specify legend manually
legend(-1.2,-0.3, legend = c("Bracken", "Heather"), fill = c("#117733",  "#AA4499"))

#save the file using Export -> Save As Image -> 



# do PERMANOVA analysis
#data frame containing the independent variables (Habitat, Vegetation) we shall be using in our PERMANOVA
idvs <- d[,(2:3)]
#run the permanova
morph_permanova <- adonis2(spe ~ Vegetation, idvs, permutations = 999, method = "bray", by = "terms")
morph_permanova


#run an anosim - when grouping by vegetation
ano = anosim(as.matrix(spe), grouping = idvs$Vegetation, permutations = 9999, distance = "bray")
# When interpreting these results you want to look at the ANOSIM statistic R and the Significance values. A Significance value less than 0.05 is generally considered to be statistically significant, and means the null hypothesis can be rejected. “The ANOSIM statistic “R” compares the mean of ranked dissimilarities between groups to the mean of ranked dissimilarities within groups. An R value close to “1.0” suggests dissimilarity between groups while an R value close to “0” suggests an even distribution of high and low ranks within and between groups” (GUSTAME). In other words, the higher the R value, the more dissimilar your groups are in terms of microbial community composition.
ano
plot(ano)





####now NMDS with just the sites, no vegetation----
d <- readr::read_csv(
  here::here("data", "2025-08-19_dbRDA_masterfile.csv")
)
#using standardized msofauna abundances
#d <- d[, c(1, 23, 70, 71, 72, 73, 74, 75, 76,77)]
#using raw abundances
d <- d[, c(1, 23, 37, 38, 39, 40, 41, 42, 43, 44)]


#order samples from east to west
d$Site <- factor(d$Site, levels = c("Bridestones", "Scarth Wood Moor", "Brimham Moor", "Widdybanks", "Haweswater", "Whiteside"), labels = c("Bridestones", "Scarth Wood Moor", "Brimham Moor", "Widdybanks", "Haweswater", "Whiteside"))
d <- arrange(d, d["Site"])
d <- as.data.frame(d)
#replace null (empty excell cell) with "0"
d[is.na(d)] <- 0
#replace row index with sample names
rownames(d) <- d[,1]
#just the morphospecies counts
#spe <- d[,-(1:3)]
#just the mite and springtail groups
#spe <- d[, (4:11)]
spe <- d[, (3:10)]
spe <- as.matrix(spe)


#k is the number of reduced dimensions
#trymax sets the default number of iterations
example_NMDS <- metaMDS(spe, distance = "bray", k = 2, maxit = 999, trymax = 500)
#Shephard plot shows scatter around the regession between the interpoint distances in the final configuration (i.e. the distances between each pair of communities) against their original dissimilarities.  Large scatter around the line suggests the original dissimilarities are not well preserved in the reduced number of dimensions
stressplot(example_NMDS)
#set dimensions of new graphics window
#dev.new(width = 719, height = 412, unit = "px")
#plot the NMDS
plot(example_NMDS, col = "white")
#assign the treatments to relevant rows of the dataframe
treat=c(rep("Bridestones",20),rep("Scarth Wood Moor",20),rep("Brimham Moor",20),rep("Widdybanks",20), rep("Haweswater",20),rep("Whiteside",20))
#set the colour for each treatment
colors=c(rep("red",20), rep("yellow", 20), rep("green", 20), rep("blue", 20), rep("purple", 20), rep("violet", 20))
text(-1,0.3, paste("Stress = ", round(example_NMDS$stress, 3)))

for(i in unique(treat)) {
  #we have added an if statement so we can chose which points and ellipses to plot at a time e.g. i == "Grassland Bracken".  If we want to plot all ellipses simultaneously, set i == i
  if(i == i){
    #change the colour of each site name so samples from the same treatment have the same colour
    orditorp(example_NMDS$point[grep(i,treat),],display="sites", col=colors[grep(i,treat)], cex=0.7,air=0.01)
    #plots ellipse with ellipse centered on the centroid of the samples from the same treatment (and thus encapsulating 95% of the variance)
    ordiellipse(example_NMDS$point[grep(i,treat),],draw="polygon",
                groups=treat[treat==i],col=colors[grep(i,treat)],label=F) } }
#specify legend manually
legend(-1.2,-0.15, legend = c("Bridestones", "Scarth Wood", "Brimham", "Widdybanks", "Haweswater", "Whiteside"), fill = c("red",  "yellow", "green", "blue", "purple", "violet"))

#save the file using Export -> Save As Image -> 



# do PERMANOVA analysis
#data frame containing the independent variables (Habitat, Vegetation) we shall be using in our PERMANOVA
idvs <- d[,(1:2)]
#run the permanova
morph_permanova <- adonis2(spe ~ Site, idvs, permutations = 999, method = "bray", by = "terms")
morph_permanova


#run an anosim - when grouping by vegetation
ano = anosim(as.matrix(spe), grouping = idvs$Site, permutations = 9999, distance = "bray")
# When interpreting these results you want to look at the ANOSIM statistic R and the Significance values. A Significance value less than 0.05 is generally considered to be statistically significant, and means the null hypothesis can be rejected. “The ANOSIM statistic “R” compares the mean of ranked dissimilarities between groups to the mean of ranked dissimilarities within groups. An R value close to “1.0” suggests dissimilarity between groups while an R value close to “0” suggests an even distribution of high and low ranks within and between groups” (GUSTAME). In other words, the higher the R value, the more dissimilar your groups are in terms of microbial community composition.
ano
plot(ano)


#SIMPER analysis
simper.site <- simper(spe, idvs$Site)
#check the output, rounded to 3 dp
summary(simper.eastwest)$Haweswater_Whiteside %>%
  round(3)





#### NMDS eastern three vs western three sites----
d <- readr::read_csv(
  here::here("data", "2025-08-19_dbRDA_masterfile.csv")
)
#using standardized msofauna abundances
#d <- d[, c(1, 23, 70, 71, 72, 73, 74, 75, 76,77)]
#using raw abundances
d <- d[, c(1, 23, 37, 38, 39, 40, 41, 42, 43, 44)]


#order samples from east to west
d$Site <- factor(d$Site, levels = c("Bridestones", "Scarth Wood Moor", "Brimham Moor", "Widdybanks", "Haweswater", "Whiteside"), labels = c("Bridestones", "Scarth Wood Moor", "Brimham Moor", "Widdybanks", "Haweswater", "Whiteside"))
d <- arrange(d, d["Site"])
d <- as.data.frame(d)
#replace null (empty excell cell) with "0"
d[is.na(d)] <- 0
#replace row index with sample names
rownames(d) <- d[,1]
#just the morphospecies counts
#spe <- d[,-(1:3)]
#just the mite and springtail groups
#spe <- d[, (4:11)]
spe <- d[, (3:10)]
spe <- as.matrix(spe)


#k is the number of reduced dimensions
#trymax sets the default number of iterations
example_NMDS <- metaMDS(spe, distance = "bray", k = 2, maxit = 999, trymax = 500)
#Shephard plot shows scatter around the regession between the interpoint distances in the final configuration (i.e. the distances between each pair of communities) against their original dissimilarities.  Large scatter around the line suggests the original dissimilarities are not well preserved in the reduced number of dimensions
stressplot(example_NMDS)
#set dimensions of new graphics window
#dev.new(width = 719, height = 412, unit = "px")
#plot the NMDS
plot(example_NMDS, col = "white")
#assign the treatments to relevant rows of the dataframe
treat=c(rep("East",60),rep("West",60))
#set the colour for each treatment
colors=c(rep("red",60), rep("blue", 60))
text(-1,0.3, paste("Stress = ", round(example_NMDS$stress, 3)))

for(i in unique(treat)) {
  #we have added an if statement so we can chose which points and ellipses to plot at a time e.g. i == "Grassland Bracken".  If we want to plot all ellipses simultaneously, set i == i
  if(i == i){
    #change the colour of each site name so samples from the same treatment have the same colour
    orditorp(example_NMDS$point[grep(i,treat),],display="sites", col=colors[grep(i,treat)], cex=0.7,air=0.01)
    #plots ellipse with ellipse centered on the centroid of the samples from the same treatment (and thus encapsulating 95% of the variance)
    ordiellipse(example_NMDS$point[grep(i,treat),],draw="polygon",
                groups=treat[treat==i],col=colors[grep(i,treat)],label=F) } }
#specify legend manually
legend(-1.2,-0.3, legend = c("East", "West"), fill = c("red",  "blue"))
#save the file using Export -> Save As Image -> 



# do PERMANOVA analysis
#data frame containing the independent variables (Habitat, Vegetation) we shall be using in our PERMANOVA
idvs <- d[,(1:2)]
idvs$Treatment <- treat
#run the permanova
morph_permanova <- adonis2(spe ~ Treatment, idvs, permutations = 999, method = "bray", by = "terms")
morph_permanova


#run an anosim - when grouping by vegetation
ano = anosim(as.matrix(spe), grouping = idvs$Treatment, permutations = 9999, distance = "bray")
# When interpreting these results you want to look at the ANOSIM statistic R and the Significance values. A Significance value less than 0.05 is generally considered to be statistically significant, and means the null hypothesis can be rejected. “The ANOSIM statistic “R” compares the mean of ranked dissimilarities between groups to the mean of ranked dissimilarities within groups. An R value close to “1.0” suggests dissimilarity between groups while an R value close to “0” suggests an even distribution of high and low ranks within and between groups” (GUSTAME). In other words, the higher the R value, the more dissimilar your groups are in terms of microbial community composition.
ano
plot(ano)

#SIMPER analysis
simper.eastwest <- simper(spe, idvs$Treatment)
#check the output, rounded to 3 dp
summary(simper.eastwest)$East_West %>%
  round(3)






####overlay mesofauna morphotypes (instrinsic variables)----
meso.intfit <- envfit(example_NMDS, spe, permutations = 999)
dev.new()
ordiplot(example_NMDS, type = "n", main = "intrinsic variables")
#orditorp(example_NMDS, display = "sites", labels = F, pch = c(16, 8, 17, 18) [as.numeric(env$`CN ratio`)], col = c("green", "blue", "orange", "black") [as.numeric(env$`CN ratio`)], cex = 1)
for(i in unique(treat)) {
  #we have added an if statement so we can chose which points and ellipses to plot at a time e.g. i == "Grassland Bracken".  If we want to plot all ellipses simultaneously, set i == i
  if(i == i){
    #plot the sample IDs on the NMDS, with the colour specific to the treatment
    # orditorp(example_NMDS$point[grep(i,treat),],display="sites", col=colors[grep(i,treat)], cex=0.7, air=0.01)
    #plot point codes for each site
    points(example_NMDS$point[grep(i,treat),], pch = pchs[grep(i,treat)], col = colors[grep(i,treat)], cex = 0.7)
    #plots ellipse with ellipse centered on the centroid of the samples from the same treatment (and thus encapsulating 95% of the variance)
    ordiellipse(example_NMDS$point[grep(i,treat),],kind = "se", conf = 0.95, draw="polygon",
                groups=treat[treat==i],col=colors[grep(i,treat)],label=F) } }

plot(meso.intfit, col = "black", cex = 0.7)
legend(0.7,2, legend=c("Brimham Bracken", "Brimham Heath", "Bridestones Bracken", "Bridestones Heath", "Haweswater Bracken", "Haweswater Heath", "Widdybanks Bracken", "Widdybanks Heath", "Whiteside Bracken", "Whiteside Heath"), col = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#0072B2", "black", "green", "purple", "red"), pch = c(15, 0,16,1,17,2, 18, 3, 19, 4))



#overlay environmental variables
env <- all_data[, 12:20]
rownames(env) <- all_data[, 1]


meso.envfit <- envfit(example_NMDS, env, permutations = 999)

ordiplot(example_NMDS, type = "n")
#orditorp(example_NMDS, display = "sites", labels = F, pch = c(16, 8, 17, 18) [as.numeric(env$`CN ratio`)], col = c("green", "blue", "orange", "black") [as.numeric(env$`CN ratio`)], cex = 1)
for(i in unique(treat)) {
  #we have added an if statement so we can chose which points and ellipses to plot at a time e.g. i == "Grassland Bracken".  If we want to plot all ellipses simultaneously, set i == i
  if(i == i){
    #plot the sample IDs on the NMDS, with the colour specific to the treatment
    # orditorp(example_NMDS$point[grep(i,treat),],display="sites", col=colors[grep(i,treat)], cex=0.7, air=0.01)
    #plot point codes for each site
    points(example_NMDS$point[grep(i,treat),], pch = pchs[grep(i,treat)], col = colors[grep(i,treat)], cex = 0.7)
    #plots ellipse with ellipse centered on the centroid of the samples from the same treatment (and thus encapsulating 95% of the variance)
    ordiellipse(example_NMDS$point[grep(i,treat),],kind = "se", conf = 0.95, draw="polygon",
                groups=treat[treat==i],col=colors[grep(i,treat)],label=F) } }

plot(meso.envfit, col = "black", cex = 0.7)


#save the file using Export -> Save As Image -> Width = 655, Height = 500 

# do PERMANOVA analysis
#data frame containing the independent variables (Habitat, Vegetation) we shall be using in our PERMANOVA
idvs <- d[,(2:3)]
#run the permanova
morph_permanova <- adonis2(spe ~ Site*Vegetation, idvs, permutations = 999, method = "bray", by = "terms")
morph_permanova


#run an ANOSIM. The ANOSIM test is similar to an ANOVA hypothesis test, but it uses a dissimilarity matrix as input instead of raw data. It is also non-parametric, meaning it doesn’t assume much about your data (like normal distribution etc), so it’s a good bet for often-skewed microbial abundance data. As a non-parametric test, ANOSIM uses ranked dissimilarities instead of actual distances, and in this way it’s a very nice complement to an NMDS plot. The main point of the ANOSIM test is to determine if the differences between two or more groups are significant.
#run an anosim - when grouping by habitat
ano = anosim(as.matrix(spe), grouping = all_data$Site, permutations = 9999, distance = "bray")
#check output of anosim
ano
plot(ano)
#run an anosim - when grouping by vegetation
ano = anosim(as.matrix(spe), grouping = all_data$Vegetation, permutations = 9999, distance = "bray")
# When interpreting these results you want to look at the ANOSIM statistic R and the Significance values. A Significance value less than 0.05 is generally considered to be statistically significant, and means the null hypothesis can be rejected. “The ANOSIM statistic “R” compares the mean of ranked dissimilarities between groups to the mean of ranked dissimilarities within groups. An R value close to “1.0” suggests dissimilarity between groups while an R value close to “0” suggests an even distribution of high and low ranks within and between groups” (GUSTAME). In other words, the higher the R value, the more dissimilar your groups are in terms of microbial community composition.
ano
plot(ano)






#### create new master doc containg mesofauna data added to the existing master file ----
d <- readr::read_csv(
  here::here("data", "2025-08-19_standardized-mesofauna-data.csv")
) 
pca <- readr::read_csv(
  here::here("data", "2025-03-19_PCA-data.csv"), show_col_types = FALSE
) 

df2_subset <- d[, c("Sample ID", names(d)[5:45])]

# Step 2: Merge df1 with the selected columns from df2
df_combined <- merge(pca, df2_subset, by = "Sample ID", all.x = TRUE)
#remove unnecessary columns
df_combined <- df_combined %>% select(-...1)   

#save our master datafile
write.csv(df_combined, "data/2025-08-19_dbRDA_masterfile.csv")

#### re analyse data following first paper results review 20/10/2025 onwards ----
d <- readr::read_csv(
  here::here("data", "2025-08-19_dbRDA_masterfile.csv")
) 
d <- as.data.frame(d)

#first, check which environmental variables differ between bracken and heather across all samples





#### dbRDA ----
d <- readr::read_csv(
  here::here("data", "2025-08-19_dbRDA_masterfile.csv")
) 

#define the community data frame
#order samples by ID alphabetically

d <- as.data.frame(d)
#convert the sample date to an appropriate format
d$`Sample Date` <- as.Date(d$`Date Sampled`, format = "%d/%m/%Y")
#convert to Julian date for linear temporal trends
d$julian <- scale(as.numeric(d$`Sample Date`))


#replace row index with sample names
rownames(d) <- d[,1]
#the community data frame for standardized abundances - with thsi we can explain approx 35% of the variance
cdf <- d[,(70:77)]
#community dataframe for raw abundances - with this we can explain only 15% of the variance
#cdf <- d[, (37:44)]

#replace null (empty excell cell) with "0"
cdf[is.na(cdf)] <- 0
cdf <- as.matrix(cdf)

#explanatory data frame: paramters that differed significantly.  THese have different base units so we may want to standardize them e.g. by z scoring
#altitude, %bracken, %heather, moisture, pH, latitude, longitude, TNb, total C, total N, C:N ratio, alpha, SUVA.  Add WEOC, WEN to dataframe too, and date
edf <- d[, c(5, 6, 7, 21, 22, 28, 29, 30, 31, 32, 33, 34, 35, 36, 79)]

#assign the treatments to relevant rows of the dataframe
edf$treatment <- c(rep("Brimham Bracken",10),rep("Brimham Heather",10), rep("Bridestones Bracken",10),rep("Bridestones Heather",10), rep("Haweswater Bracken", 10), rep("Haweswater Heather", 10), rep("Scarth Wood Bracken",10),rep("Scarth Wood Heather",10), rep("Widdybanks Bracken",10),rep("Widdybanks Heather",10), rep("Whiteside Bracken", 10), rep("Whiteside Heather", 10))

#all explanatory factors modelled

dbrda_summary <- dbrda(formula = cdf ~ `Altitude (m)` + `Pteridium aquilinium`+ `Calunna vulgaris`+ `Soil Moisture (% fresh soil mass)` + `pH` + `LatitudeN` + `LongitudeE` + `NPOC (mg C g-1)` + `TNb (mg N g-1)` + `Drift Corr C (g per kg)`+ `Drift Corr N (g per kg)` + `CN ratio` + `alpha` + `SUVA (L mg-1 cm-1)`  + `julian` , edf, distance = "euclidean", sqrt.dist = FALSE, add = FALSE, dfun = vegdist, metaMDSdist = FALSE, na.action = na.exclude, subset = NULL)

#just the environmental factors that are significant
dbrda_summary <- dbrda(formula = cdf ~ `Altitude (m)` + `Soil Moisture (% fresh soil mass)` + `LongitudeE` + `NPOC (mg C g-1)` + `Drift Corr N (g per kg)`, edf, distance = "euclidean", sqrt.dist = FALSE, add = FALSE, dfun = vegdist, metaMDSdist = FALSE, na.action = na.exclude, subset = NULL)

summary(dbrda_summary)


# Define treatment variable and convert to factor
#treatment <- as.factor(edf$treatment)

treatment <- factor(edf$treatment, levels = unique(edf$treatment))
# Define custom colors and point shapes (pch) for the 6 treatments
treatment_levels <- levels(treatment)
#colours for each treatment
colors =c("#999999","#999999", "#E69F00", "#E69F00", "#56B4E9","#56B4E9", "#009E73","#009E73", "#CC79A7", "#CC79A7", "#0072B2","#0072B2")[seq_along(treatment_levels)]
#shapes for point codes
pchs<- c(15, 0, 16, 1, 17,2, 18, 3, 19, 4, 20, 5)[seq_along(treatment_levels)]


# Get variance explained by each axis
eig_vals <- eigenvals(dbrda_summary)
var_explained <- eig_vals / sum(eig_vals) * 100
axis_labels <- paste0("dbRDA", 1:2, " (", round(var_explained[1:2], 1), "%)")



dev.new()
# Base plot: empty dbRDA ordination
plot(dbrda_summary, type = "n", scaling = 2, , 
     xlab = axis_labels[1], ylab = axis_labels[2])  # Use scaling = 2 for species-environment biplot

# Add site points, colored by treatment
for (i in seq_along(treatment_levels)) {
  sel <- treatment == treatment_levels[i]
  points(scores(dbrda_summary, display = "sites", scaling = 2)[sel, ], 
         col = colors[i], pch = pchs[i], cex = 1.2)
} 
# --- Add ellipses around treatment groups ---
ordiellipse(dbrda_summary, groups = treatment, display = "sites", kind = "se", conf = 0.95, draw = "polygon", col = colors, border = colors,lwd = 1.5,lty = 1, alpha = 60)  # transparency (0–255); needs vegan >= 2.6-4


# Add legend
legend(x = 35, y = 85, legend = treatment_levels, 
       col = colors, pch = pchs)

# Extract biplot scores of environmental variables
env_vectors <- scores(dbrda_summary, display = "bp", scaling = 2)
# Scale factor to adjust vector length visually
vec_multiplier <- ordiArrowMul(env_vectors)  # automatic scaling
# Add arrows
apply(env_vectors, 1, function(row) {
  arrows(0, 0, row[1] * vec_multiplier, row[2] * vec_multiplier, 
         length = 0.1, col = "black")
})
# Add vector labels
text(env_vectors * vec_multiplier, labels = rownames(env_vectors), 
     col = "black", pos = 4, cex = 0.8)

dev.off()

summary(dbrda_summary)
#is the model significant?
anova(dbrda_summary)
#test axes for significance
anova(dbrda_summary, by = "axis", perm.max = 500)
#test environmental variables for significance
anova(dbrda_summary, by = "terms", perm.max = 500)

