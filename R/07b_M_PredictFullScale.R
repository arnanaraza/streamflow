### FUNCTION TO PREDICT USING RF MODEL, REGRESS, THEN PLOT ### 

# Preliminearies
pacman::p_load(ggplot2, ggpmisc, devtools, lubridate)
mydir <- setwd('D:/THESIS_PP')

# Function to predict using RF model then 
predAll <- function (VT, SW, RF, floss) {

  #prepare training-test data
  #VT$date <-  lubridate::ymd(VT$date)
  VT1 <- VT[,-53] #remove date from covariates
  VT1$mo <- month(VT$date)
  vt.names <- c('LC1.agr', 'LC2.for', 'LC3.gr', 'LC4.wgr', 'LC5.wetl', 
                'LC6.riv', 'LC7.bu', 'LC8.barr', 'LC9.CN', 'LC10.man',
                'LC11.fl', 'W1.ws', 'W2.tmin', 'W3.tmax', 'W4.pcp', 'W5.rh', 'O.obs',
                'P.bd1', 'P.br1', 'P.cl1', 'P.sa1', 'P.si1', 'P.dem1', 'P.sl1', 
                'P.bd2', 'P.br2', 'P.cl2', 'P.sa2', 'P.si2', 'P.dem2', 'P.sl2',
                'P.bd3', 'P.br3', 'P.cl3', 'P.sa3', 'P.si3', 'P.dem3', 'P.sl3',
                'P.bd4', 'P.br4', 'P.cl4', 'P.sa4', 'P.si4', 'P.dem4', 'P.sl4', 
                'P.bd5', 'P.br5', 'P.cl5', 'P.sa5', 'P.si5', 'P.dem5', 'P.sl5', 
                'W6.pcpw', 'W7.pcpm', 'W8.pcpmx', 'SW', 'mo')
  colnames(VT1) <- vt.names
  if (floss == 'yes') {
    VT1 <- VT1
    col.pos <- 17}
  if (floss == 'no') {
    VT1 <- VT1[,-c(2,9:11)]
    col.pos <- 13}
  
  #predict using the model
  pred <- predict(RF, VT1)
  join <- as.data.frame(cbind(VT1[[col.pos]], pred[[1]]))
  date <- VT$date 
  join <- cbind(join, as.Date(date))
  
  
  #plot regression graphs
  colnames (join) <- c('observed','predicted', 'date')
  
  plot1 <- (ggplot(join, aes(date)) +                    
    geom_line(aes(y=observed, colour="observed")) +  
    geom_line(aes(y=predicted, colour="predicted")) + 
    scale_colour_manual('',breaks = c('observed', 'predicted'),
                        values = c('observed'="black", 'predicted'="red")) +
    xlab("date") +
    scale_y_continuous("streamflow (l/sec") + 
    labs(title="observed vs. predicted - all data"))
  plot(plot1)
  ggsave(plot=plot1, filename=paste0('D:/THESIS_PP/finalresults/',  Sys.Date(), SW, '_', floss,'_ts.png'), device='png', dpi=100, width = 11, height = 8, units='in')
  
  #write results
  setwd('D:/THESIS_PP/finalresults/')
  subs <- as.name(SW)
  fname <- sprintf('%s.csv',deparse(substitute(subs)))
  write.csv(join, file=fname)
  setwd(mydir)

  return(join)
  
}


