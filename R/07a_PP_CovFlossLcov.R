## SCRIPT TO PRE-PROCESS FOREST LOSS AND LAND COVER INPUTS ##

# Preliminaries
pacman::p_load (raster, rgdal)
mydir <- setwd('D:/THESIS_PP')

# Function to open files from subfolders: general, weather, soil, lcover, dem
FolderFiles <- function(folder, ext) {
  allfiles <- list.files(paste0(mydir,'/data/', folder),  pattern = paste0('*.', ext, '$'))
  print (allfiles)
}

# Reads land cover and loss-gain pixels then set extent, mask and write
lcovtifs <- FolderFiles ('lcover', 'tif')
floss <- raster(paste0(mydir,'/data/lcover/', as.character(lcovtifs)[2]))
fgain <- raster(paste0(mydir,'/data/lcover/', as.character(lcovtifs)[1]))
lcov1 <- raster(paste0(mydir,'/data/lcover/', as.character(lcovtifs)[5]))
lcov2 <- raster(paste0(mydir,'/data/lcover/', as.character(lcovtifs)[6]))
lcov1 <- setExtent(lcov1, lcov2)
lcov1 [lcov1==0] <- NA
lcov2 [lcov2==0] <- NA
lcov1 [lcov1==255] <- NA
lcov2 [lcov2==255] <- NA
NetLoss <- mask(floss, fgain)

lshape <- readOGR(dsn = 'D:/THESIS_PP/data/general', layer = "subs1")
lcrop1 <- crop(lcov1, extent(lshape))
lcrop2 <- crop(lcov2, extent(lshape))
lmask1 <- mask(lcrop1, lshape)
lmask2 <- mask(lcrop2, lshape)
lmask1 <- setExtent(lmask1, lmask2)
lmask1 [lmask1 == 0] <- NA
lmask2 [lmask2 == 0] <- NA
lmask2 [lmask2 == 255] <- NA
lcovs <- list(lcov1, lcov2)
lcovs$fun <- mean
lcov2000 <- do.call(mosaic, lcovs)

setwd('D:/THESIS_PP/mid-results')
writeRaster(lmask, filename="Lcov00.tif", overwrite=TRUE) # input to reclassification after, too slow in R, had to switch to ArcMap
hist(lmask)
