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
ggsave(path = "C:/Users/jakef/Documents/York/Project 2 Analysis/project-2/figures", paste0(Sys.Date(), "_DOM-curve-alpha-paramter_green-purple.svg"), alpha_bxp)

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
# repeat at wavelengths of 250 (aromaticity, apparent molecular weight), 254 (aromaticity), 260 (hydrophobic C content), 265 (relative abundance of functional groups), 272 (aromaticity), 280 (hydrophobic C content, humification index, apparent molecular size), 285 (humification index), 300 (characterization of humic substances), 340 (colour), 350 (apparent molecular size), 365 (aromaticity, apparent molecular weight), 400 (humic substances characterization), 436 (quality indicator), 465 (relative abundance of functional groups)

#list of wavelengths of interest
wavelength_of_interest <- list(250, 254, 260, 265, 272, 280, 285, 300, 340, 350, 365, 400, 436, 465)


#function to plot data for DOM, requires dataframe d (with columns `Sample ID`, `Wavelength (nm)`, Absorbance) and wavelenth of interest
DOMboxplotter <- function(d, wavelength){
  #define the wavelength of interest
  wavelength_of_interest <- wavelength
  #filter data to extract absorbance at wavelength of interest
  abs <- d %>%
    filter(`Wavelength (nm)` == wavelength_of_interest) %>% #filter for specific wavelength
    select(`Sample ID`, Absorbance) #select the relevant columns
  
  #add in habitat and vegetation factors
  abs$Site <- c(rep("Whiteside",20), rep("Haweswater",20), rep("Widdybanks",20), rep("Brimham Rocks",20), rep("Scarth Wood",20), rep("Bridestones",20))
  
  abs$Vegetation <- c(rep("Bracken",10), rep("Heathland", 10),rep("Bracken",10), rep("Heathland", 10),rep("Bracken",10), rep("Heathland", 10),rep("Bracken",10), rep("Heathland", 10),rep("Bracken",10), rep("Heathland", 10),rep("Bracken",10), rep("Heathland", 10))
  
  abs <- as.data.frame(abs)
  
  #rename Nonbracken so that it is plotted first, and bracken second
#  abs$Vegetation[abs$Vegetation == 'Non-Bracken'] <- ''
  #create string for y axis label
  yaxis_label <- paste("Absorbance at ", wavelength_of_interest, "nm")
  
  
  #plot absorbance
  abs_bxp <- ggboxplot(abs, x = "Site", y = 'Absorbance', color = "Vegetation", palette = c("limegreen", "#AA4499"), lwd = 0.75)  +
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
  
  show(abs_bxp)
  #create file name for our plot.  Use paste0 so there are no spaces between each item in the list
  filename <- paste0(Sys.Date(), "_absorbance_at_", wavelength_of_interest, ".svg")
  #save our plot.  As this is a function, we need specify the entire file path
  ggsave(path = "C:/Users/jakef/Documents/York/Project 2 Analysis/project-2/figures", filename, abs_bxp)
  
  #statistical analysis
  #nested anova
  anova <- aov(abs$Absorbance ~ abs$Site / factor(abs$Vegetation))
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

#### DOM Quantity Data Formatting ----
d <- readr::read_csv(
  here::here("data-raw", "project-2-data-master", "individual", "4) 2025-01-16_WHI-HAWB_Jake-Vario-TOC.csv")
) 
# Group by 'Sample ID' and calculate the mean of 'concentration', putting the output in a new dataframe
averaged_df <- d %>%
  group_by(`Sample ID`) %>%
  summarise(`NPOC (mg/l)` = mean(`NPOC [mg/l]`), `TNb (mg/l)` = mean(`TNb [mg/l]`) )
#save our processed data file.  Once we have the data for all samples, we can pool into one file and add Site and Vegetation columns
write.csv(averaged_df, file =  "C:/Users/jakef/Documents/York/Project 2 Analysis/project-2/data/6) Averaged-WHI-HAWB-TOC-Data.csv", row.names =FALSE)

#### DOM Quantity Data Analysis ----
d <- readr::read_csv(
  here::here("data", "10) Averaged-TOC-Data.csv"))
  
#boxplot the data. Use aes() with backticks (``) so avoid an error with our column name
dom_bxp <- ggboxplot(d, x = "Site", aes(y = `NPOC (mg/l)`), color = "Vegetation", palette = c("limegreen", "#AA4499"), lwd = 0.75)  + 
  labs(x = "Site",
       y = "NPOC (mg/l)") + theme(
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

show(dom_bxp)  
#save our plot
ggsave(path = "figures", paste0(Sys.Date(), "_DOM.svg"), width = 10, height= 5, dom_bxp)


#nested anova
anova <- aov(d$`NPOC (mg/l)` ~ d$Site / factor(d$Vegetation))
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

#### TNb Quantity Data Analysis ----
d <- readr::read_csv(
  here::here("data", "10) Averaged-TOC-Data.csv"))

#boxplot the data. Use aes() with backticks (``) so avoid an error with our column name
tnb_bxp <- ggboxplot(d, x = "Site", aes(y = `TNb (mg/l)`), color = "Vegetation", palette = c("limegreen", "#AA4499"), lwd = 0.75)  + 
  labs(x = "Site",
       y = "TNb (mg/l)") + theme(
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

show(tnb_bxp)  
#save our plot
ggsave(path = "figures", paste0(Sys.Date(), "_TNb.svg"), width = 10, height= 5, dom_bxp)


#nested anova
anova <- aov(d$`TNb (mg/l)` ~ d$Site / factor(d$Vegetation))
summary(anova)
#tukey's test to identify significant interactions
tukey <- TukeyHSD(anova)
print(tukey)
#compact letter display
print(cld)


#check homogeneity of variance
plot(anova, 1)
#levene test.  if p value < 0.05, there is evidence to suggest that the variance across groups is statistically significantly different.
leveneTest(d$`TNb (mg/l)` ~ d$Vegetation*d$Site)
#check normality.  
plot(anova, 2)
#conduct shapiro-wilk test on ANOVA residuals to test for normality
#extract the residuals
aov_residuals <- residuals(object = anova)
#run shapiro-wilk test.  if p > 0.05 the data is normal
shapiro.test(x = aov_residuals)

