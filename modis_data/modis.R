
library(terra)

# Files downloaded 2023-11-01 from NASA EarthData (see download.sh)
# I selected a single point, the coordinates of the NEON tower
files <- list.files("modis_data/", pattern = "hdf$", full.names = TRUE)

# Read in data files and compute spatial average
res <- list()
for(f in sort(files)) {
  message("Processing ", f)
  hdf_dat <- rast(f)
  dat_means <- colMeans(values(hdf_dat), na.rm = TRUE)
  res[[f]] <- as.data.frame(t(dat_means))
  res[[f]]$filename <- basename(f)
}

results <- do.call(rbind, res)
rownames(results) <- NULL

# SDS Name: Gpp_500m
# Description: Gross Primary Productivity
# Units: kg C/m²
# 16-bit signed integer
# Fill Value: 32761 to 32767
# No Data Value: N/A
# Valid Range: 0 to 30000
# Scale Factor: 0.0001

y <- clamp(x[,"Gpp_500m"] /  0.0001, lower = 0, upper = 30000)


# https://rspatial.org/modis/4-quality.html

# install.packages('luna', repos='https://rspatial.r-universe.dev')
library(terra)
library(luna)

product <- "MOD17A2H" # MOD17A3HGF
start <- "2022-01-01"
end <- "2022-12-31"
aoi <- ext(-76.57, -76.55, 38.88, 38.90)

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
