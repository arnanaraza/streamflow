## Script to organize streamflow data ##

# Preliminaries - packages, directory
if (!require("pacman")) install.packages("pacman")
pacman::p_load(imputeTS, Hmisc, rio, plyr, dplyr, xlsx)

mydir <- setwd('D:/THESIS_PP')

# Load streamflow data in xls and converts to csv 
loadcsv <- function(datadir){
  setwd (datadir)
  files.to.read = list.files(datadir, pattern="xls")
  lapply(files.to.read, function(f) {
    df = read.xlsx(f, sheetIndex=1)
    write.csv(df, gsub("xls", "csv", f), row.names=FALSE)
  })
  alltables <- list.files (datadir, pattern='*.csv')
  alltables1 <- lapply(alltables, read.csv)
  alltables2 <- lapply(alltables1, function(x) x[-c(6941:10000),])# delete excess rows
  alltables3 <- bind_cols(alltables2) 
  setwd('D:/THESIS_PP/mid-results')
  write.csv (alltables3, file='obs_data1.csv')
  setwd(datadir)
}



loadcsv('D:/THESIS_PP/data/streamflow/updated1')
