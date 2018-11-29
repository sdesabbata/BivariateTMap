# Import function
source("bivariate_tmap.R")

# Import example data
library(tmap)
data("World")
# Transform example data to SpatialPolygonDataFrame
library(sf)
world_spdf <- as(World, "Spatial")

# Option 1
# Plot bivariate choroplet map (including legend)
bivariate_choropleth(world_spdf, c("gdp_cap_est", "inequality"))

# Option 2
# Simply get the map (without legend)
# for further manipulation
biv_map <- get_bivariate_choropleth(world_spdf, c("gdp_cap_est", "inequality"))