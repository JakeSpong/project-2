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
    orditorp(example_NMDS$point[grep(i,treat),],display="sites", col=colors, cex=0.7,air=0.01)
    #plots ellipse with ellipse centered on the centroid of the samples from the same treatment (and thus encapsulating 95% of the variance)
    ordiellipse(example_NMDS$point[grep(i,treat),],draw="polygon",
                groups=treat[treat==i],col=colors[grep(i,treat)],label=F) } }
#specify legend manually
legend(1.25,-0.65, legend = c("Bracken", "Heather"), fill = c("#117733",  "#AA4499"))
