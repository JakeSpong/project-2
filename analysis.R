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

#### Soil Moisture analysis ----
#read in the data
d <- readr::read_csv(
  here::here("data-raw", "project-2-data-master", "individual", "1) Gravimetric Soil Moisture Content.csv")
) 
#boxplot the data. Use aes() with backticks (``) so avoid an error with our column name
gsmc_bxp <- ggboxplot(d, x = "Site", aes(y = `Gravimetric Soil Moisture Content (%)`), color = "Vegetation", palette = c("limegreen", "#AA4499"), lwd = 0.75)  +
  labs(x = "Site",
       y = "Gravimetric Soil \n Moisture Content (%)") + theme(
         # Remove panel border
         panel.border = element_blank(),  
         # Remove panel grid lines
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         # Remove panel background
         panel.background = element_blank(),
         # Add axis line
         axis.line = element_line(colour = "black", linewidth = 0.75),
         #change colour and thickness of axis ticks
         axis.ticks = element_line(colour = "black", linewidth = 0.5),
         #change axis labels colour
         axis.title.x = element_text(colour = "black", face = "bold"),
         axis.title.y = element_text(colour = "black", face = "bold"),
         #change tick labels colour
         axis.text.x = element_text(colour = "black", face = "bold"),
         axis.text.y = element_text(colour = "black", face = "bold"),
       ) 

show(gsmc_bxp)  
#save our plot
ggsave(path = "figures", paste0(Sys.Date(), "_gravimetric-soil-moisture-content.svg"), width = 10, height= 5, gsmc_bxp)


#nested anova
anova <- aov(d$`Gravimetric Soil Moisture Content (%)` ~ d$Site / factor(d$Vegetation))
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
leveneTest(d$GSMC ~ d$Vegetation*d$Site)
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
         axis.line = element_line(colour = "black", linewidth = 0.75),
         #change colour and thickness of axis ticks
         axis.ticks = element_line(colour = "black", linewidth = 0.5),
         #change axis labels colour
         axis.title.x = element_text(colour = "black", face = "bold"),
         axis.title.y = element_text(colour = "black", face = "bold"),
         #change tick labels colour
         axis.text.x = element_text(colour = "black", face = "bold"),
         axis.text.y = element_text(colour = "black", face = "bold"),
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
#add columns to our new table
table$Site <- c(rep("Brimham Rocks",20), rep("Bridestones",20), rep("Haweswater",20), rep("Scarth Wood",20), rep("Widdybanks",20), rep("Whiteside",20))

table$Vegetation <- c(rep("Bracken",10), rep("Heather", 10), rep("Bracken",10), rep("Heather", 10),rep("Bracken",10), rep("Heather", 10),rep("Bracken",10), rep("Heather", 10),rep("Bracken",10), rep("Heather", 10),rep("Bracken",10), rep("Heather", 10))

#save our processed data file
write.csv(table, "5) alpha paramater of DOM curve fitting.csv", row.names =FALSE)

#read in the processed absorbance data
table <- readr::read_csv(
  here::here("data", "5) e2w alpha paramater of DOM curve fitting.csv")) 

#boxplot the alpha, which describes the curve ie how quicky we go from low wavelength (high mass C compounds) to high wavelength (low mass C compounds)
alpha_bxp <- ggboxplot(table, x = "Site", y = 'alpha', color = "Vegetation", palette = c("black", "limegreen"), lwd = 0.75)  +
  labs(x = "Habitat x Vegetation",
       y = "DOM fitted curve alpha parameter") + theme(
         # Remove panel border
         panel.border = element_blank(),  
         # Remove panel grid lines
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         # Remove panel background
         panel.background = element_blank(),
         # Add axis line
         axis.line = element_line(colour = "black", linewidth = 0.75),
         #change colour and thickness of axis ticks
         axis.ticks = element_line(colour = "black", linewidth = 0.5),
         #change axis labels colour
         axis.title.x = element_text(colour = "black", face = "bold"),
         axis.title.y = element_text(colour = "black", face = "bold"),
         #change tick labels colour
         axis.text.x = element_text(colour = "black", face = "bold"),
         axis.text.y = element_text(colour = "black", face = "bold"),
       ) 
show(alpha_bxp)

#save our plot
ggsave(path = "C:/Users/jakef/Documents/York/Project 2 Analysis/project-2/figures", paste0(Sys.Date(), "_DOM-curve-alpha-paramter_black-green.svg"), alpha_bxp)



#nested anova
anova <- aov(table$alpha ~ table$Site / factor(table$Vegetation))

summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)

