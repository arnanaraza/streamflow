### FUNCTION TO PREDICT USING RF MODEL, REGRESS, THEN PLOT FOR 6 VALIDATION SITES ### 

# Preliminearies
pacman::p_load(ggplot2, ggpmisc, devtools, lubridate)
mydir <- setwd('D:/THESIS_PP')

# Function to predict using RF model then 
predValid <- function (VT, SW, RF, floss) {

  #prepare training-test data
  #VT$date <-  base::date(VT$date)
  VT1 <- VT[,-52]
  VT1$mo <- month(lubridate::date(VT$date))
  vt.names <- c('LC1.agr', 'LC2.for', 'LC3.gr', 'LC4.wgr', 'LC5.wetl', 
                'LC6.riv', 'LC7.bu', 'LC8.barr', 'LC9.CN', 'LC10.man',
                'LC11.fl', 'W1.ws', 'W2.tmin', 'W3.tmax', 'W4.pcp', 'W5.rh',
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
  det <- seq(from = lubridate::date("2000-01-01"), to = lubridate::date("2016-12-31"), by = 'day')
  join <- cbind(pred$predictions, as.Date(det))
  join <- as.data.frame(join)
  colnames(join) <-c('pred', 'date')
  
  plot1 <- (ggplot(join, aes(x=as.Date(date), y=pred)) +                    
              geom_line() +  
              xlab("date") +
              scale_y_continuous("streamflow (l/sec)") + 
              labs(title=paste0("predicted streamflow, validation, ",' ', SW)))
 # plot(plot1)
  ggsave(plot=plot1, filename=paste0('D:/THESIS_PP/final results valid/',  Sys.Date(), ' ',SW, '_', floss,'_ts.png'), device='png', dpi=100, width = 11, height = 8, units='in')
  

  return(join)
  
}


