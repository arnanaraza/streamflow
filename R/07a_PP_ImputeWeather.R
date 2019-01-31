### Script to impute missing weather data ##

# Preliminaries - packages, directory
pacman::p_load(imputeTS, Hmisc, rio, plyr, dplyr, xlsx)
mydir <- setwd ('D:/THESIS_PP')

# Load weather data in xls and converts to csv 
loadcsv <- function(datadir){
  setwd (datadir)
  files.to.read = list.files(datadir, pattern="xls")
  lapply(files.to.read, function(f) {
    df = read.xlsx(f, sheetIndex=1)
    write.csv(df, gsub("xls", "csv", f), row.names=FALSE)
    setwd(mydir)
  })
}


allcsv <- loadcsv('D:/THESIS_PP/data/weather/updated/')


# Function to fill small gaps via interpolation; if NAs are smaller than given 'gap', the funciton will interpolate
FillSmall <- function (datadir, gap) {
  setwd (datadir)
  alltables <- list.files (datadir, pattern='*.csv')
  alltables0 <- lapply(alltables, read.csv)
  alltables1 <- lapply(alltables0, "[", TRUE, -1) # delete first column, not needed for regression
  alltables2 <- lapply(alltables1, function(x) x[-c(6941:10000),])# delete excess rows
  alltables3 <- bind_cols(alltables2) # won't bind if different rowsums
  alltables3 <- alltables3[, colSums(is.na(alltables3)) != nrow(alltables3)]
  
  
  for (col in names(alltables3)) {
    missing <- sum(is.na(alltables3[,col]))
    if (missing <= gap & missing != 0) {
      fill <- na.interpolation(alltables3[,col], option='spline')
      alltables3[,col] <- fill
      setwd('D:/THESIS_PP/mid-results/')
      write.csv (alltables3, file='int_weather1.csv')
      }
  }
    if (missing >= length(col)){
      col_pos <- which(colnames(alltables3)==col)
      alltables3 <- alltables3[,-col_pos]
  }
  setwd('D:/THESIS_PP/mid-results/')
 # alltables3 <-  alltables3[, -which(colMeans(is.na(alltables3)) > lo)]
  write.csv (alltables3, file='int_weather1.csv')
  return(alltables3)
}

df <- FillSmall('D:/THESIS_PP/data/weather/updated/',50)
df.x <- df[,colSums(is.na(df))<nrow(df)]

## double check stations without RL


# Function to use regression in imputation 
NoMissing <- function (gap){
  w.table <- read.csv('D:/THESIS_PP/mid-results/int_weather1.csv')
  w.table <- w.table[,-1]
  w.table <- w.table[,colSums(is.na(w.table))<nrow(w.table)]
  for (col in names(w.table)){
    missing <- sum(is.na(w.table[,col]))
    #if (sum(!is.na(col)) < 2 ) {return(NA)} 
    if (missing <= gap) {
      col_pos <- which(colnames(w.table)==col)
      ind <- w.table[,-(as.numeric(col_pos))]
      dep <- w.table[,col]
      coi <- w.table[[col_pos]]
      
      # first batch regression removing column of interest
      reg <- lapply(ind, function(x) (summary(lm(dep ~ x))))
      
      # get highest R2
      r2 <- ldply(reg, function(x) {r.sq <- x[1:length(x)]$r.squared
      data.frame(r.sq)})
      r2high <- order(-r2$r.sq)
      r2high <- r2high[1]
      predname <- noquote(r2$.id[r2high])
      pred <- which (colnames(w.table)==predname)
      
      # regress again and predict using highest R2
      result <- lm((dep) ~ (w.table[[pred]]))
      newpred <- predict(result, newdata= data.frame(dep))
      
      # retain original values, replace with predicted column, and write new table
      is.na(w.table[[col_pos]][]) <- setNames(w.table[[col_pos]], newpred)[as.character(unlist(is.na(w.table[[col_pos]])))]
      w.table[[col_pos]] <- newpred
      indx <- !is.na(coi)
      w.table[[col_pos]][indx] <- coi[indx]
      setwd('D:/THESIS_PP/mid-results/')
      write.csv(w.table, 'reg_weather1.csv')
      setwd(mydir)
    }
  }
  for (i in names (w.table)) {print(sum(is.na((w.table[,i]))))} #checker
  return(w.table)
  
}

# some tests
df1 <- NoMissing (1000)
df2 <- NoMissing (5000)

