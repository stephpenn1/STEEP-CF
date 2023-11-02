
library(terra)


# SDS Name: Gpp_500m
# Description: Gross Primary Productivity
# Units: kg C/m²
# 16-bit signed integer
# Fill Value: 32761 to 32767
# No Data Value: N/A
# Valid Range: 0 to 30000
# Scale Factor: 0.0001


# https://rspatial.org/modis/4-quality.html

# install.packages('luna', repos='https://rspatial.r-universe.dev')
library(terra)
library(luna)

# I selected a single point, the coordinates of the NEON tower
# 38.890131, -76.560014
product <- "MOD17A2H" # MOD17A3HGF
start <- "2022-01-01"
end <- "2022-12-31"
aoi <- ext(-76.560014, -76.560014, 38.890131, 38.890131)

tiles <- luna::getModis(product,
                        start, end, aoi = aoi,
                        download = FALSE)

# Save a credentials file by
# earthdata <- list(username = "...", password = "...")
# saveRDS(earthdata, file = "modis_data/earthdata_credentials")

# Read in the credentials file so we can log in and download
earthdata <- readRDS("modis_data/earthdata_credentials")

mf <- luna::getModis(product,
                     start, end, aoi = aoi,
                     download = TRUE,
                     path = "./modis_data/",
                     username = earthdata$username,
                     password = earthdata$password)

# Quality screening, following https://rspatial.org/modis/4-quality.html

# TODO

# Processing

files <- list.files("modis_data/",
                    pattern = "^MOD17A2H.*hdf$",
                    full.names = TRUE)

# Read in data files and compute spatial average
res <- list()
for(f in sort(files)) {
  message("Processing ", f)
  r <- rast(f)
  # Isolate GPP data
  gpp <- r[["Gpp_500m"]]
  # Rescale and 'clamp' the values, based on documentation:
  # https://lpdaac.usgs.gov/products/myd17a2hv061/
  gpp_clamp <- clamp(gpp / 0.0001, lower = 0, upper = 30000, values = FALSE) * 0.0001
  # Compute mean in gC/m2
  gpp_mean <- mean(values(gpp_clamp), na.rm = TRUE) * 1000 # gC/m2
  res[[f]] <- data.frame(filename = basename(f),
                         GPP = gpp_mean)
}

results <- do.call(rbind, res)
rownames(results) <- NULL

# Separate filename into more useful data. For the help page:

# In this example of a tiled product, the filename
# MCD19A2.A2023081.h22v02.061.2023082164002.hdf indicates:
#
# MCD19A2 - Product Short Name
# A2023081 - Julian Date of Acquisition (AYYYYDDD)
# h22v02 - Tile Identifier (horizontalXXverticalYY)
# 061 - Collection Version
# 2023082164002 - Julian Date of Production (YYYYDDDHHMMSS)
# .hdf - Data Format (HDF-EOS)


library(tidyr)
results %>%
  separate(filename,
           into = c("product", "jdoa", "ti", "cv", "jdop", "df"),
           sep = "\\.") ->
  gpp

gpp$year <- as.integer(substr(gpp$jdoa, 2, 5))
gpp$day <- as.integer(substr(gpp$jdoa, 6, 8))
gpp <- gpp[gpp$year == 2022,]

# MODIS GPP is an 8-day product. Convert to daily GPP
gpp$daily_GPP <- gpp$GPP / 8

# Interpolate to every day for the entire year
gpp_interp <- data.frame(day = 1:365,
                         GPP = approx(gpp$day,
                                      gpp$daily_GPP,
                                      xout = 1:365, rule = 2)$y)

