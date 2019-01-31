## SCRIPT TO PRE-PROCESS DEM AND SLOPE INPUTS ##

# Preliminaries
pacman::p_load (raster, rgdal)
mydir <- setwd('D:/THESIS_PP')

# Function to open files from subfolders
  # argument folder = 'general', 'weather', 'soil', 'lcover', 'dem' for folders #argument ext = 'tif'
  FolderFiles <- function(folder, ext) {
    allfiles <- list.files(paste0(mydir,'/data/', folder),  pattern = paste0('*.', ext, '$'))
    print (paste('loaded:', allfiles))
    return (allfiles)
  }

  # Read loss-gain pixelS, mask, and write
  DEMfiles <- FolderFiles ('dem', 'tif')

DEM <- raster(paste0(mydir,'/data/dem/', DEMfiles[2]))
slope <- raster(paste0(mydir,'/data/dem/', DEMfiles[3]))
DEM.slope <- stack(DEM, slope)

dshape <- readOGR(dsn = 'D:/THESIS_PP/data/general', layer = "subs1")
dcrop <- crop(DEM.slope, extent(shape))
dmask <- mask(dcrop, shape)

setwd('D:/THESIS_PP/mid-results')
writeRaster(dmask, filename="DEM.sl.tif", overwrite=TRUE)
