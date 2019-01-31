## SCRIPT TO PRE-PROCESS SOIL INPUTS ##

# Preliminaries
pacman::p_load (raster, rgdal)
mydir <- setwd('D:/THESIS_PP')

# Function to open files from subfolders: general, weather, soil, lcover
FolderFiles <- function(folder, ext) {
  allfiles <- list.files(paste0(mydir,'/data/', folder),  pattern = paste0('*.', ext, '$'))
  print (allfiles)
}

soiltifs <- FolderFiles ('soil', 'tif')
print (soiltifs)

# Function to read tifs, mosaic and mask soil inputs
ReadSoil <- function (soilcode){
  index <- grep(soiltifs,  pattern = as.character(paste0('^', soilcode)))
  input <- lapply(index, function(x) raster(paste0('M:/THESIS_PP/data/soil/', as.character(soiltifs[x]))))
  input$fun <- mean
  smosaic <- do.call(mosaic, input)
  shape <- readOGR(dsn = 'D:/THESIS_PP/data/general', layer = "subs1") #inserts watershed shapefile (no R function yet so must come from ArcMap hydrotools i.e. SWAT)
  sproj <- projectRaster(smosaic, crs = shape@proj4string)
  scrop <- crop(sproj, extent(shape))
  smask <- mask(scrop, shape)
  return (smask)
}

SoilClass <- ReadSoil ('TAXNWRB')
Bedrock <- ReadSoil ('BDTICM')
BulkDensity <- ReadSoil ('BLDFIE')
Sand <- ReadSoil ('SNDPPT')
Silt <- ReadSoil ('SLTPPT')
Clay <- ReadSoil ('CLYPPT')


setwd('D:/THESIS_PP/mid-results')
writeRaster(SoilClass, filename="SoilClass.tif", overwrite=TRUE)
writeRaster(Bedrock, filename="Bedrock.tif", overwrite=TRUE)
writeRaster(BulkDensity, filename="BulkDensity.tif", overwrite=TRUE)
writeRaster(Sand, filename="Sand.tif", overwrite=TRUE)
writeRaster(Clay, filename="Clay.tif", overwrite=TRUE)
writeRaster(Silt, filename="Silt.tif", overwrite=TRUE)
setwd('D:/THESIS_PP')





