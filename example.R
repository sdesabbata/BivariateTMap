# Import function
source("bivariate_tmap.R")

# Import example data
library(tmap)
data("World")
# Transform example data to SpatialPolygonDataFrame
library(sf)
world_spdf <- as(World, "Spatial")

# Plot bivariate choroplet map
bivariate_choropleth(world_spdf, c("gdp_cap_est", "inequality"))