# Importing libraries
library(baselines)
library(tidyverse)
library(maps)
library(mapdata)
library(colorspace)
graphics_device_quartz()

# Importing data
df_fia <- read.csv("fire_nrt_M6_96062.csv", sep = ",")

# First look at dataset
head(df_fia)
colnames(df_fia)


# Creating a new dataset with columns of interest
columns <- c("latitude", "longitude", "confidence", "brightness", "daynight")
df2_fia <- df_fia[ , columns]

## Look at subsetted dataset before condition
head(df2_fia)
nrow(df2_fia)

# Condition: Only looking at high-confidence fires
df3_fia <- df2_fia[df2_fia$confidence >= 95, ]
nrow(df3_fia)

## Look at subsetted dataset after condition
head(df3_fia)


# Subset dataset into Day vs Night
df_fia_day <- df3_fia[df3_fia$daynight == "D", ]
df_fia_night <- df3_fia[df3_fia$daynight == "N", ]

## Check subsetted data
unique(df_fia_day$daynight)
unique(df_fia_night$daynight)

nrow(df_fia_day)
nrow(df_fia_night)


# Separating fires depending on whether it was a Daytime vs Nighttime fire

dev.new()
par(mfrow = c(2, 1), 
    mar = c(5, 4, 4, 2))

## Define the bins 
breaks <- seq(300, 550, by = 50)

## Define a colour per bin
colours <- diverge_hcl(length(breaks) - 1)



## DAYTIME
max(df_fia_day$brightness)
min(df_fia_day$brightness)

### Assign each brightness value to a bin
bins <- cut(df_fia_day$brightness, 
            breaks = breaks, 
            include.lowest = TRUE)

## Map each bin to a colour
point_colours <- colours[as.numeric(bins)]

### Plot the map
map(database = "worldHires",
    regions = "Australia",
    xlim = c(112, 154),
    ylim = c(-44, -10),
    col = "white",
    fill = TRUE)

### Plot the points 
points(df_fia_day$longitude, df_fia_day$latitude,
       pch = 4, cex = 0.1, col = point_colours)

### Add a legend
legend(x = 65, y = -20, 
       legend = paste(breaks[-length(breaks)], "-", breaks[-1]), 
       fill = colours, 
       title = "Brightness (in Kelvin)",
       cex = 0.7,
       pt.cex = 0.5,
       title.cex = 0.9,
       xpd = TRUE,
       bty = "n")

title(main = "Australia Wildfires in the Day (95% confidence)",
      cex.main = 1.5)

mtext("Figure 1a. Distribution of Daytime Wildfires (N = 16204).",
      side = 1, line = 1, cex = 1, adj = 0.5)
mtext("Each marker on the map indicates a high-confidence fire (i.e., having at least a 95% probability of being a daytime fire hotspot).",
      side = 1, line = 2, cex = 0.7, adj = 0.5)
mtext("The colour of each marker represents the Brightness temperature (measured in Kelvin).",
      side = 1, line = 3, cex = 0.7, adj = 0.5)


## NIGHTTIME 
max(df_fia_night$brightness)
min(df_fia_night$brightness)

### Assign each brightness value to a bin
bins <- cut(df_fia_night$brightness, 
            breaks = breaks, 
            include.lowest = TRUE)

## Map each bin to a colour
point_colours <- colours[as.numeric(bins)]

### Plot the map
map(database = "worldHires",
    regions = "Australia",
    xlim = c(112, 154),
    ylim = c(-44, -10),
    col = "white",
    fill = TRUE)

### Plot the points 
points(df_fia_night$longitude, df_fia_night$latitude,
       pch = 4, cex = 0.1, col = point_colours)

### Add a legend
legend(x = 65, y = -20, 
       legend = paste(breaks[-length(breaks)], "-", breaks[-1]), 
       fill = colours, 
       title = "Brightness (in Kelvin)",
       cex = 0.7,
       pt.cex = 0.5,
       title.cex = 0.9, 
       xpd = TRUE,
       bty = "n")

title(main = "Australia Wildfires in the Night (95% confidence)",
      cex.main = 1.5)

mtext("Figure 1b. Distribution of Nighttime Wildfires (N = 16391).",
      side = 1, line = 1, cex = 1, adj = 0.5)
mtext("Each marker on the map indicates a high-confidence fire (i.e., having at least a 95% probability of being a nighttime fire hotspot).",
      side = 1, line = 2, cex = 0.7, adj = 0.5)
mtext("The colour of each marker represents the Brightness temperature (measured in Kelvin).",
      side = 1, line = 3, cex = 0.7, adj = 0.5)








# ARCHIVE: CODE FOR FIGURE 5
# # Separating fires depending on whether it was a Daytime vs Nighttime fire
# ## Set up plotting layout
# dev.new()
# par(mfrow = c(2, 2),
#     mar = c(5, 3, 5, 3))
# 
# ## Daytime fires
# ### Map
# map(database = "worldHires",
#     xlim = c(112, 154),
#     ylim = c(-44, -10),
#     regions = "Australia", asp = 1)
# 
# points(df_fia_day$longitude, df_fia_day$latitude,
#        pch = 4, cex = 0.1, col = "orange")
# 
# #### Annotations
# title(main = "Australia Wildfires in the Day (95% confidence)",
#       cex.main = 1.5)
# 
# mtext("Figure 1a. Distribution of daytime wildfires (N = 16204).",
#       side = 1, line = 3, cex = 1, adj = 0.5)
# mtext("Each orange marker on the map indicates a high-confidence fire (i.e.,",
#       side = 1, line = 4, cex = 1, adj = 0.5)
# mtext("having at least a 95% probability of being a daytime fire hotspot)",
#       side = 1, line = 5, cex = 1, adj = 0.5)
# 
# ### Histogram
# hist(df_fia_day$brightness,
#      main = "Histogram of Brightness Temperature in Daytime Fires",
#      xlab = "Brightness Temperature (Kelvin)",
#      ylab = "Number of fires",
#      cex.main = 1.5,  
#      cex.lab = 1.2,  
#      cex.axis = 1.0)
# 
# #### Annotations
# mtext("Figure 1b. Distribution of brightness temperature in daytime wildfires.",
#       side = 1, line = 5, cex = 1, adj = 0.5)
# 
# ## Nighttime fires
# ### Map
# map(database = "worldHires",
#     xlim = c(112, 154),
#     ylim = c(-44, -10),
#     regions = "Australia")
# 
# points(df_fia_night$longitude, df_fia_night$latitude,
#        pch = 4, cex = 0.1, col = "orange")
# 
# #### Annotations
# title(main = "Australia Wild Fires in the Night (95% confidence)",
#       cex.main = 1.5)
# 
# mtext("Figure 2a. Distribution of nighttime wildfires (N = 16391).",
#       side = 1, line = 3, cex = 1, adj = 0.5)
# mtext("Each orange marker on the map indicates a high-confidence fire (i.e.,",
#       side = 1, line = 4, cex = 1, adj = 0.5)
# mtext("having at least a 95% probability of being a nighttime fire hotspot)",
#       side = 1, line = 5, cex = 1, adj = 0.5)
# 
# ### Histogram
# hist(df_fia_night$brightness,
#      main = "Histogram of Brightness Temperature in Nighttime Fires",
#      xlab = "Brightness Temperature (Kelvin)",
#      ylab = "Number of fires",
#      xlim = c(300, 510),
#      ylim = c(0, 5500),
#      axes = FALSE,
#      cex.main = 1.5, 
#      cex.lab = 1.2, 
#      cex.axis = 1.0)
# 
# #### Custom x-axis
# axis(1, at = seq(300, 550, by = 50))
# 
# #### Default y-axis
# axis(2, at = seq(0, 5500, by = 500))
# 
# #### Annotations
# mtext("Figure 2b. Distribution of brightness temperature in nighttime wildfires.",
#       side = 1, line = 5, cex = 1, adj = 0.5)

