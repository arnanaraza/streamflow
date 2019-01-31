### FUNCTION TO INTEGRATE RAINFALL IN ANALYSIS, 6 VALIDATION SITES ### 

# Preliminearies
pacman::p_load(dplyr, lubridate)
mydir <- setwd('D:/THESIS_PP')

# Function proper
plotPcp <- function (pred.table, SW) {
  pred.table$date <- seq(from = lubridate::date("2000-01-01"), to = lubridate::date("2016-12-31"), by = 'day')
  by.month <- aggregate(pred.table$pcp ~month(date),
                        data=pred.table,FUN=mean)
  colnames(by.month) <- c('mo', 'pcp')
  by.month.d <- data.table(by.month, key='pcp')
  by.month.d[, tail(.SD, 3), by=by.month.d$pcp]
  
  
  by.year <- aggregate(pred.table$pcp ~ year(date) + month(date),
                       data=pred.table,FUN=mean)
  colnames(by.year) <- c('year','mo', 'pcp')
  by.year.pivot <- dcast(by.year, mo ~ year, value.var="pcp", fun.aggregate=mean)
  
  mo <- by.month.d[,1]
  dry.mo <- as.vector(t(mo[1:2]))
  wet.mo <- as.vector(t(mo[11:12]))
  
  driest <- filter(by.year.pivot, mo %in% dry.mo)
  driest$id <- 1
  driest.mo <- aggregate(driest[1:2,], list(driest$id), mean)
  driest.mo <- driest.mo [,-c(1,2,length(driest.mo))]
  driest.mo <- t(as.data.frame(driest.mo))
  driest.mo <- as.data.frame(driest.mo)
  driest.mo$year <- unique(by.year$year)
  colnames(driest.mo) <- c('avg.pcp.mm', 'year')
  
  plot <- ggplot(driest.mo, aes( year ) ) + 
    geom_line(aes(y=avg.pcp.mm)) + 
    labs(title=paste0('average rainfall, dry season', ' ',SW))
  plot(plot)
  ggsave(plot=plot, filename=paste0('D:/THESIS_PP/final results/',  Sys.Date(),'_', SW, '_pcp_dry.png'), device='png', dpi=100, width = 11, height = 8, units='in')
  
  wettest <- filter(by.year.pivot, mo %in% wet.mo)
  wettest$id <- 1
  wettest.mo <- aggregate(wettest[1:2,], list(wettest$id), mean)
  wettest.mo <- wettest.mo [,-c(1,2,length(wettest.mo))]
  wettest.mo <- t(as.data.frame(wettest.mo))
  wettest.mo <- as.data.frame(wettest.mo)
  wettest.mo$year <- unique(by.year$year)
  colnames(wettest.mo) <- c('avg.pcp.mm', 'year')
  
  plot1 <- ggplot(wettest.mo, aes( year ) ) + 
    geom_line(aes(y=avg.pcp.mm))+
    labs(title=paste0('average rainfall, wet season', ' ',SW))
  plot(plot1)
  ggsave(plot=plot1, filename=paste0('D:/THESIS_PP/final results/', Sys.Date(),'_', SW, '_pcp_wet.png'), device='png', dpi=100, width = 11, height = 8, units='in')
  
  #write results
  setwd('D:/THESIS_PP/final results/')
  subs <- as.name(SW)
  fname <- sprintf('%s.csv',deparse(substitute(subs)))
  write.csv(driest.mo, file=fname)
  setwd(mydir)
  
return(driest.mo)
}


