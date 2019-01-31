### FUNCTION TO COMPUTE ANOMALIES BASED ON HISTORICAL MEAN STREAMFLOW VALUES PER SEASON FOR 6 VALIDATION SITES### 

# Preliminearies
pacman::p_load(dplyr, lubridate)
mydir <- setwd('D:/THESIS_PP')

# Function proper
getHist1 <- function (pred.table, SW, floss) {
  
  # get wet and dry season months
  pred.table$date <- seq(from = lubridate::date("2000-01-01"), to = lubridate::date("2016-12-31"), by = 'day')
  by.month <- aggregate(pred.table$predicted ~month(date),
                        data=pred.table,FUN=mean)
  colnames(by.month) <- c('mo', 'pred')
  by.month.d <- data.table(by.month, key='pred')
  print (by.month.d) #checker if sorted
  by.month.d[, tail(.SD, 3), by=by.month.d$pred]
  
  # pivot per year
  by.year <- aggregate(pred.table$predicted ~ year(date) + month(date),
                       data=pred.table,FUN=mean)
  colnames(by.year) <- c('year','mo', 'pred')
  by.year.pivot <- dcast(by.year, mo ~ year, value.var="pred", fun.aggregate=mean)
  
  
  # get driest and wettest per year
  mo <- by.month.d[,1]
  dry.mo <- as.vector(t(mo[1]))
  wet.mo <- as.vector(t(mo[11:12]))
  
  driest <- filter(by.year.pivot, mo %in% dry.mo)
  wettest <- filter(by.year.pivot, mo %in% wet.mo)
  
  dry.list <- t(as.data.frame(lapply(driest[1,2:length(driest)], mean)))
  wet.list <- t(as.data.frame(lapply(wettest[1:2,2:length(wettest)], mean)))
  
  # attach pcp and fl
  by.year.fl <- aggregate(pred.table$forest ~year(date),data=pred.table,FUN=mean)
  driest.fl <- cbind(dry.list, by.year.fl)
  driest.fl <- driest.fl[,-4]
  colnames (driest.fl) <- c('dry.ts', 'year', 'fl')
  wettest.fl <- cbind(wet.list, by.year.fl)
  wettest.fl <- wettest.fl[,-4]
  colnames (wettest.fl) <- c('wet.ts', 'year', 'fl')
   
  return (wettest.fl) #inter-change wettest and driest
}


