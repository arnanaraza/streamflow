### FUNCTION TO COMPUTE ANOMALIES BASED ON HISTORICAL MEAN STREAMFLOW VALUES PER SEASON ### 

# Preliminearies
pacman::p_load(dplyr, lubridate, data.table, ggplot2)
mydir <- setwd('D:/THESIS_PP')

# Function proper
getHist <- function (pred.table, SW, floss) {
  
  # get wet and dry season months
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
  dry.mo <- as.vector(t(mo[1:2]))
  wet.mo <- as.vector(t(mo[11:12]))
  
  driest <- filter(by.year.pivot, mo %in% dry.mo)
  wettest <- filter(by.year.pivot, mo %in% wet.mo)
  
  dry.list <- t(as.data.frame(lapply(driest[1:2,2:length(driest)], mean)))
  wet.list <- t(as.data.frame(lapply(wettest[1:2,2:length(wettest)], mean)))
  
  # attach pcp and fl
  by.year.fl <- aggregate(pred.table$forest ~year(date),data=pred.table,FUN=mean)
  driest.fl <- cbind(dry.list, by.year.fl)
  driest.fl <- driest.fl[,-4]
  colnames (driest.fl) <- c('dry.flow', 'year', 'forest.loss')
  wettest.fl <- cbind(wet.list, by.year.fl)
  wettest.fl <- wettest.fl[,-4]
  colnames (wettest.fl) <- c('wet.flow', 'year', 'forest.loss')
  
  # open historical pivot table and get SW 
  h.table <- read.csv('D:/THESIS_PP/mid-results/hist3.csv')
  h.table <- h.table[-c(13:15),]
  
  subs <- subset(h.table, select = grep(SW, names(h.table))) #given that h.table has SW as headings
  #colnames(subs) <- c('mo', SW)
  
  driest.h <- filter(subs, h.table$mo %in% dry.mo)
  wettest.h <- filter(subs, h.table$mo %in% wet.mo)
  driest.mean <- mean(driest.h[[1]])
  wettest.mean <- mean(wettest.h[[1]])
  
  # plot the graph
  plot <- ggplot(driest.fl, aes( year ) ) + 
    geom_line(aes(y=dry.flow, colour ='dry.flow'))+
    geom_line(aes(y=forest.loss, colour="forest.loss")) +  
    geom_hline(yintercept = as.numeric(driest.mean), linetype="dotted",  size=2) +
    labs(title=paste0('predicted dry season daily mean streamflow,', ' ', SW)) +
    scale_colour_manual('',breaks = c('dry.flow', 'forest.loss'),
                      values = c('dry.flow'="blue", 'forest.loss'="red")) +
    xlab("year") +
    scale_y_continuous("average streamflow (l/sec)") +
    annotate("text", min(driest.fl$year),  as.numeric(driest.mean), hjust = 0, vjust = -1, label = "historical dry season daily mean streamflow") + 
    theme(text = element_text(size = 16))
  #plot(plot)
  ggsave(plot=plot, filename=paste0('D:/THESIS_PP/finalresults/',  Sys.Date(),'_', SW, '_', floss,'_','_dry.png'), device='png', dpi=100, width = 11, height = 8, units='in')

  
  plot1 <- ggplot(wettest.fl, aes( year ) ) + 
    geom_line(aes(y=wet.flow, colour ='wet.flow'))+
    geom_line(aes(y=forest.loss, colour="forest.loss")) +  
    geom_hline(yintercept = as.numeric(wettest.mean), linetype="dotted", size=2) +
    labs(title=paste0('predicted wet season daily mean streamflow', ' ', SW)) +
    scale_colour_manual('',breaks = c('wet.flow', 'forest.loss'),
                        values = c('wet.flow'="blue", 'forest.loss'="red")) +
  xlab("year") +
  scale_y_continuous("average streamflow (l/sec)") +
    annotate("text", min(wettest.fl$year),  as.numeric(wettest.mean), hjust = 0, vjust = -1, label = "historical wet season daily mean streamflow") + 
    theme(text = element_text(size = 16))
 # plot(plot)
  ggsave(plot=plot1, filename=paste0('D:/THESIS_PP/finalresults/',  Sys.Date(),'_', SW, '_',floss, '_','_wet.png'), device='png', dpi=100, width = 11, height = 8, units='in')

  
  return (driest.fl) #inter-change to driest and wettest
}


