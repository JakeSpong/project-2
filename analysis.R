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
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggmap)

#load the field data
all_data <- readr::read_csv(
  here::here("data", "all-sites_field-data.csv"), show_col_types = FALSE
) 

#extract the sample coordinates from the dataframe
sample_coordinates <- data.frame(
  Site = all_data$Site,
  Vegetation = all_data$Vegetation,
  Longitude = all_data$LongitudeE,
  Latitude = all_data$LatitudeN
)
#the sample coordinates belonging to each site
bridestones <- sample_coordinates[1:20, ]
scarthwood <- sample_coordinates[21:40, ]
brimham <- sample_coordinates[41:60, ]
haweswater <- sample_coordinates[61:80,]
whiteside <- sample_coordinates[81:100, ]
widdybanks <- sample_coordinates[101:120,]

# Calculate the bounding box surrounding the 30 sample coordinates
bridestones_longitude_range_sample <- range(bridestones$Longitude)
bridestones_latitude_range_sample <- range(bridestones$Latitude)
# Calculate the bounding box surrounding the 30 sample coordinates
scarthwood_longitude_range_sample <- range(scarthwood$Longitude)
scarthwood_latitude_range_sample <- range(scarthwood$Latitude)
# Calculate the bounding box surrounding the 30 sample coordinates
brimham_longitude_range_sample <- range(brimham$Longitude)
brimham_latitude_range_sample <- range(brimham$Latitude)
# Calculate the bounding box surrounding the 30 sample coordinates
haweswater_longitude_range_sample <- range(haweswater$Longitude)
haweswater_latitude_range_sample <- range(haweswater$Latitude)
# Calculate the bounding box surrounding the 30 sample coordinates
whiteside_longitude_range_sample <- range(whiteside$Longitude)
whiteside_latitude_range_sample <- range(whiteside$Latitude)
# Calculate the bounding box surrounding the 30 sample coordinates
widdybanks_longitude_range_sample <- range(widdybanks$Longitude)
widdybanks_latitude_range_sample <- range(widdybanks$Latitude)


# Register your Google API key
register_google(key = "AIzaSyA42ZmX_RGv9dTA7PFy4UR0YIjxW3x0rUE")

# Load the UK map from the rnaturalearth package
uk_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name == "United Kingdom")

# Plot the UK map with only the bounding box
UK_Sites <- ggplot(data = uk_map) +
  geom_sf(fill = "lightblue") +  # UK map with light blue fill
  geom_rect(
    aes(xmin = bridestones_longitude_range_sample[1],
        xmax = bridestones_longitude_range_sample[2],
        ymin = bridestones_latitude_range_sample[1],
        ymax = bridestones_latitude_range_sample[2]
        ), 
    color = "red", fill = NA, size = 2) +  # Add bounding box around the points
  geom_rect(
    aes(xmin = scarthwood_longitude_range_sample[1],
        xmax = scarthwood_longitude_range_sample[2],
        ymin = scarthwood_latitude_range_sample[1],
        ymax = scarthwood_latitude_range_sample[2]
    ), 
    color = "red", fill = NA, size = 2) +
  geom_rect(
    aes(xmin = brimham_longitude_range_sample[1],
        xmax = brimham_longitude_range_sample[2],
        ymin = brimham_latitude_range_sample[1],
        ymax = brimham_latitude_range_sample[2]
    ), 
    color = "red", fill = NA, size = 2) +
  geom_rect(
    aes(xmin = widdybanks_longitude_range_sample[1],
        xmax = widdybanks_longitude_range_sample[2],
        ymin = widdybanks_latitude_range_sample[1],
        ymax = widdybanks_latitude_range_sample[2]
    ), 
    color = "red", fill = NA, size = 2) +
  geom_rect(
    aes(xmin = haweswater_longitude_range_sample[1],
        xmax = haweswater_longitude_range_sample[2],
        ymin = haweswater_latitude_range_sample[1],
        ymax = haweswater_latitude_range_sample[2]
    ), 
    color = "red", fill = NA, size = 2) +
  geom_rect(
    aes(xmin = whiteside_longitude_range_sample[1],
        xmax = whiteside_longitude_range_sample[2],
        ymin = whiteside_latitude_range_sample[1],
        ymax = whiteside_latitude_range_sample[2]
    ), 
    color = "red", fill = NA, size = 2) +
  theme_minimal()
#show the map
show(UK_Sites)
#save the UK map with Haweswater highlighted
ggsave("figures/UK_Sites_boundingbox.svg", width = 8, height = 6)


#map the sample coordinates at each site
#bridestones
# Get the Google Satellite map for the zoomed-in area based on the 30 sample coordinates
bridestones_satellite <- get_map(location = c(lon = mean(bridestones$Longitude), 
                                          lat = mean(bridestones$Latitude)),
                             zoom = 15,  # Adjust zoom level for the desired zoom
                             maptype = "satellite",  # Use the "satellite" map type
                             source = "google")
#plot the map
bridestones_map <- ggmap(bridestones_satellite) +
  geom_point(data = bridestones, aes(x = Longitude, y = Latitude, 
                                 color = Vegetation, shape = Vegetation), size = 2) +
  scale_color_manual(values = c("Bracken" = "green", "Heather" = "purple")) +  # Color by habitat
  scale_shape_manual(values = c("Bracken" = 17, "Heather" = 16)) +  # Shape by site
  theme_minimal() + 
  theme(legend.position = "right") 
#show the map
show(bridestones_map)
# Save the final map to a file
ggsave("figures/bridestones_map.svg", plot = last_plot(), width = 7.5, height = 6, dpi = 300)

#scarthwood
# Get the Google Satellite map for the zoomed-in area based on the 30 sample coordinates
scarthwood_satellite <- get_map(location = c(lon = mean(scarthwood$Longitude), 
                                              lat = mean(scarthwood$Latitude)),
                                 zoom = 15,  # Adjust zoom level for the desired zoom
                                 maptype = "satellite",  # Use the "satellite" map type
                                 source = "google")
#plot the map
scarthwood_map <- ggmap(scarthwood_satellite) +
  geom_point(data = scarthwood, aes(x = Longitude, y = Latitude, 
                                     color = Vegetation, shape = Vegetation), size = 2) +
  scale_color_manual(values = c("Bracken" = "green", "Heather" = "purple")) +  # Color by habitat
  scale_shape_manual(values = c("Bracken" = 17, "Heather" = 16)) +  # Shape by site
  theme_minimal() + 
  theme(legend.position = "right") 
#show the map
show(scarthwood_map)
# Save the final map to a file
ggsave("figures/scarthwood.svg", plot = last_plot(), width = 7.5, height = 6, dpi = 300)

#brimham 
# Get the Google Satellite map for the zoomed-in area based on the 30 sample coordinates
brimham_satellite <- get_map(location = c(lon = mean(brimham$Longitude), 
                                      lat = mean(brimham$Latitude)),
                         zoom = 15,  # Adjust zoom level for the desired zoom
                         maptype = "satellite",  # Use the "satellite" map type
                         source = "google")
  #plot the map
brimham_map <- ggmap(brimham_satellite) +
  geom_point(data = brimham, aes(x = Longitude, y = Latitude, 
                                            color = Vegetation, shape = Vegetation), size = 2) +
  scale_color_manual(values = c("Bracken" = "green", "Heather" = "purple")) +  # Color by habitat
  scale_shape_manual(values = c("Bracken" = 17, "Heather" = 16)) +  # Shape by site
  theme_minimal() + 
  theme(legend.position = "right") 
#show the map
#show(brimham_map)
# Save the final map to a file
ggsave("figures/brimham_map.svg", plot = last_plot(), width = 7.5, height = 6, dpi = 300)

#widdybanks
# Get the Google Satellite map for the zoomed-in area based on the 30 sample coordinates
widdybanks_satellite <- get_map(location = c(lon = mean(widdybanks$Longitude), 
                                          lat = mean(widdybanks$Latitude)),
                             zoom = 17,  # Adjust zoom level for the desired zoom
                             maptype = "satellite",  # Use the "satellite" map type
                             source = "google")
#plot the map
widdybanks_map <- ggmap(widdybanks_satellite) +
  geom_point(data = widdybanks, aes(x = Longitude, y = Latitude, 
                                 color = Vegetation, shape = Vegetation), size = 2) +
  scale_color_manual(values = c("Bracken" = "green", "Heather" = "purple")) +  # Color by habitat
  scale_shape_manual(values = c("Bracken" = 17, "Heather" = 16)) +  # Shape by site
  theme_minimal() + 
  theme(legend.position = "right") 
#show the map
show(widdybanks_map)
# Save the final map to a file
ggsave("figures/widdybanks_map.svg", plot = last_plot(), width = 7.5, height = 6, dpi = 300)

#Haweswater
# Get the Google Satellite map for the zoomed-in area based on the 30 sample coordinates
haweswater_satellite <- get_map(location = c(lon = mean(haweswater$Longitude), 
                                             lat = mean(haweswater$Latitude)),
                                zoom = 15,  # Adjust zoom level for the desired zoom
                                maptype = "satellite",  # Use the "satellite" map type
                                source = "google")
#plot the map
haweswater_map <- ggmap(haweswater_satellite) +
  geom_point(data = haweswater, aes(x = Longitude, y = Latitude, 
                                    color = Vegetation, shape = Vegetation), size = 2) +
  scale_color_manual(values = c("Bracken" = "green", "Heather" = "purple")) +  # Color by habitat
  scale_shape_manual(values = c("Bracken" = 17, "Heather" = 16)) +  # Shape by site
  theme_minimal() + 
  theme(legend.position = "right") 
#show the map
show(haweswater_map)
# Save the final map to a file
ggsave("figures/haweswater_map.svg", plot = last_plot(), width = 7.5, height = 6, dpi = 300)

#Whiteside
# Get the Google Satellite map for the zoomed-in area based on the 30 sample coordinates
whiteside_satellite <- get_map(location = c(lon = mean(whiteside$Longitude), 
                                             lat = mean(whiteside$Latitude)),
                                zoom = 17,  # Adjust zoom level for the desired zoom
                                maptype = "satellite",  # Use the "satellite" map type
                                source = "google")
#plot the map
whiteside_map <- ggmap(whiteside_satellite) +
  geom_point(data = whiteside, aes(x = Longitude, y = Latitude, 
                                    color = Vegetation, shape = Vegetation), size = 2) +
  scale_color_manual(values = c("Bracken" = "green", "Heather" = "purple")) +  # Color by habitat
  scale_shape_manual(values = c("Bracken" = 17, "Heather" = 16)) +  # Shape by site
  theme_minimal() + 
  theme(legend.position = "right") 
#show the map
show(whiteside_map)
# Save the final map to a file
ggsave("figures/whiteside_map.svg", plot = last_plot(), width = 7.5, height = 6, dpi = 300)


#combine the two maps into a single figure
maps_figure <- ggarrange(bridestones_map, scarthwood_map, brimham_map,                      widdybanks_map, haweswater_map, whiteside_map,
                         labels = c("A", "B", "C", "D", "E", "F"),
                         ncol = 2, nrow = 3,
                         #the width of each panel of the multifigure plot
                         widths = c(5,5),
                         common.legend = TRUE
)
#show the plot in the Plots window
show(maps_figure)
#save the figure
ggsave("figures/Maps_panel_figure.svg", plot = last_plot(), width = 7.5, height = 6, dpi = 300)



#### Plot a map of our sample locations ----

library(leaflet)
library(googleVis)

#load the field data
individual <- readr::read_csv(
  here::here("data", "all-sites_field-data.csv"), show_col_types = FALSE
) 

# Using leaflet - dynamic map
# Sites projected onto an Open Street Map background
# leaflet uses the viewer panel to display its output.
# Works when called from RStudio but no longer from the 
# R console.
background <- addTiles(leaflet())
all_field_sites <-
  addMarkers(
    background,
    lng = individual$LongitudeE,
    lat = individual$LatitudeN,
    label = as.character(individual$`SampleID`),
    labelOptions = labelOptions(noHide = TRUE, textOnly = FALSE)
  )
#map of our sampling locations
all_field_sites
library(htmlwidgets)

#a fix so that saveWidget saves the html to our working directory
saveWidgetFix <- function (widget,file,...) {
  ## A wrapper to saveWidget which compensates for arguable BUG in
  ## saveWidget which requires `file` to be in current working
  ## directory.
  wd<-getwd()
  on.exit(setwd(wd))
  outDir<-dirname(file)
  file<-basename(file)
  setwd(outDir);
  saveWidget(widget,file=file,...)
}
saveWidget(all_field_sites, file="all_field_sites.html")

#### Alpha diversity analysis of vegetation survey data ----
#### Load vegetation abundance data
d <- readr::read_csv(
  here::here("data", "all-sites_field-data.csv"), show_col_types = FALSE
) 
#add in site names
d$Site <- c(rep("Bridestones",20), rep("Scarth Wood Moor",20), rep("Brimham Moor",20), rep("Haweswater",20), rep("Whiteside", 20), rep("Widdybanks",20))
#rename "Land" column to "Vegetation"
names(d)[4] <- "Vegetation"
#order samples by bracken or heather
d <- arrange(d, d["Vegetation"])
#ensure d is a dataframe
d <- as.data.frame(d)
#reorder the sites so they show up on the plot from west (LHS) to east (RHS)
d$Site <- factor(d$Site, levels = c("Whiteside", "Haweswater", "Widdybanks", "Brimham Moor", "Scarth Wood Moor", "Bridestones"))
#replace null (empty excell cell) with "0"
d[is.na(d)] <- 0
#create a subset containing only species abundance values
#just the species counts
spe <- d[,-(1:5)]
spe <- spe[,-(16:18)]

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



#### Beta diversity analysis of vegetation survey data ----
#### Load vegetation abundance data
d <- readr::read_csv(
  here::here("data", "all-sites_field-data.csv"), show_col_types = FALSE
) 
#order samples by bracken or heather
d <- arrange(d, d["Land"])
#ensure d is a dataframe
d <- as.data.frame(d)
#replace row index with sample names
rownames(d) <- c(d$SampleID)
#replace all NAs with 0s
d[is.na(d)] <- 0
#make sure our variables are coded as factors
d$Land <- factor(d$Land, levels = c("Bracken", "Heather"), labels = c("Bracken", "Heather"))
#just the species counts
spe <- d[,-(1:5)]
spe <- spe[,-(16:17)]

#k is the number of reduced dimensions
#trymax sets the default number of iterations
example_NMDS <- metaMDS(spe, distance = "bray", k = 2, maxit = 999, trymax = 500)
#Shephard plot shows scatter around the regession between the interpoint distances in the final configuration (i.e. the distances between each pair of communities) against their original dissimilarities.  Large scatter around the line suggests the original dissimilarities are not well preserved in the reduced number of dimensions
stressplot(example_NMDS)
#set dimensions of new graphics window
dev.new(width = 719, height = 412, unit = "px")
#plot the NMDS
plot(example_NMDS, col = "white")
#change width of axes and surroundng box
axis(side = 1, lwd = 2)
axis(side = 2, lwd = 2)
box(lwd = 2)

#assign the treatments to relevant rows of the dataframe
treat=c(rep("Bracken",60),rep("Heather",60))
#set the colour for each treatment
colors=c(rep("#117733",60), rep("#AA4499", 60))
text(-1.2,2, paste("Stress = ", round(example_NMDS$stress, 3)))

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


#nested anova
anova <- aov(d$`Soil Moisture (% fresh soil mass)` ~ d$Site / factor(d$Vegetation))
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
leveneTest(d$`Soil Moisture (% fresh soil mass)` ~ d$Site / d$Vegetation)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)



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


#nested anova
anova <- aov(d$`pH` ~ d$Site / factor(d$Vegetation))
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
#compact letter display
print(cld)


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

#### Soil DOM Quality Data Analysis ----

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

#nested anova
anova <- aov(table$alpha ~ table$Site / factor(table$Vegetation))

summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)

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

#list of wavelengths of interest
wavelength_of_interest <- list(254)#list(250, 254, 260, 265, 272, 280, 285, 300, 340, 350, 365, 400, 436, 465)


#function to plot data for DOM, requires dataframe d (with columns `Sample ID`, `Wavelength (nm)`, Absorbance, Standardized Absorbance) and wavelenth of interest
DOMboxplotter <- function(d, wavelength){
  #define the wavelength of interest
  wavelength_of_interest <- wavelength
  #filter data to extract absorbance at wavelength of interest
  abs <- d %>%
    filter(`Wavelength (nm)` == wavelength_of_interest) %>% #filter for specific wavelength
    select(`Sample ID`, `Standardized Absorbance`) #select the relevant columns
  
  #add in habitat and vegetation factors
  abs$Site <- c(rep("Whiteside",20), rep("Haweswater",20), rep("Widdybanks",20), rep("Brimham Rocks",20), rep("Scarth Wood Moor",20), rep("Bridestones",20))
  
  abs$Vegetation <- c(rep("Bracken",10), rep("Heathland", 10),rep("Bracken",10), rep("Heathland", 10),rep("Bracken",10), rep("Heathland", 10),rep("Bracken",10), rep("Heathland", 10),rep("Bracken",10), rep("Heathland", 10),rep("Bracken",10), rep("Heathland", 10))
  
  abs <- as.data.frame(abs)

  #create string for y axis label
  yaxis_label <- paste("Absorbance at ", wavelength_of_interest, "nm")
  #reorder the sites so they show up on the plot from west (LHS) to east (RHS)
  table$Site <- factor(table$Site, levels = c("Whiteside", "Haweswater", "Widdybanks", "Brimham Moor", "Scarth Wood Moor", "Bridestones"))
  
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
  #nested anova
  anova <- aov(abs$`Standardized Absorbance` ~ abs$Site / factor(abs$Vegetation))
  print(wavelength_of_interest)
  summary(anova)
  #tukey's test to identify significant interactions
  tukey <- TukeyHSD(anova)
  print(tukey)
  
}
#plot the absorbance boxplot at the following given wavelengths
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
#nested anova
anova <- aov(d$`SUVA (L mg-1 cm-1)` ~ d$Site / factor(d$Vegetation))
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
#compact letter display
cld <- multcompLetters4(anova, tukey)
#compact letter display
print(cld)

#### Alpha parameter analysis ----
d <- readr::read_csv(
  here::here("data", "5) alpha paramater of DOM curve fitting.csv")
) 
#nested anova
anova <- aov(d$`alpha` ~ d$Site / factor(d$Vegetation))
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
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

show(dom_bxp)  
#save our plot
ggsave(path = "figures", paste0(Sys.Date(), "_DOM.svg"), width = 10, height= 5, dom_bxp)


#nested anova
anova <- aov(d$`NPOC (mg C g-1)` ~ d$Site / factor(d$Vegetation))
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
#compact letter display
print(cld)


#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$`NPOC (mg/l)` ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

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

show(tnb_bxp)  
#save our plot
ggsave(path = "figures", paste0(Sys.Date(), "_TNb.svg"), width = 10, height= 5, tnb_bxp)


#nested anova
anova <- aov(d$`TNb (mg N g-1)` ~ d$Site / factor(d$Vegetation))
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
#compact letter display
print(cld)


#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$`TNb (mg N g-1)` ~ d$Site / d$Vegetation)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)


#### Total C ----

d <- readr::read_csv(
  here::here("data", "8) drift corrected-C-N.csv")
) 
#trim off empty rows
d <- d[1:120,1:17]

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


#nested anova
anova <- aov(d$`Drift Corr C (g per kg)` ~ d$Site / factor(d$Vegetation))
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
leveneTest(d$`Drift Corr C (g per kg)` ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)


#### Total N ----

d <- readr::read_csv(
  here::here("data", "8) drift corrected-C-N.csv")
) 
#trim off empty rows
d <- d[1:120,1:17]

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


#nested anova
anova <- aov(d$`Drift Corr N (g per kg)` ~ d$Site / factor(d$Vegetation))
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
leveneTest(d$`Drift Corr N (g per kg)` ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)


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


#nested anova
anova <- aov(d$`CN ratio` ~ d$Site / factor(d$Vegetation))
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
leveneTest(d$`C:N ratio`~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

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
