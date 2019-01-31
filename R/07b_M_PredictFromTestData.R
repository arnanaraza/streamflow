### FUNCTION TO PREDICT USING RF MODEL, REGRESS, THEN PLOT FROM TEST DATA ### 

# Preliminearies
pacman::p_load(ggplot2, ggpmisc, devtools, caret)
mydir <- setwd('D:/THESIS_PP')

# Function to predict using RF model then 
pred <- function (td, basin, RF, floss) {

  all.VT.fin <- read.csv(paste0('D:/THESIS_PP/finalresults/', basin,'.csv')) 
  all.VT.fin <- all.VT.fin[,-1]
  
  #prepare training-test data
  #sample <-  sample(1:nrow(all.VT.fin), size=(1-td)*nrow(all.VT.fin))
  set.seed(123)
  samp <- floor((1-td)*nrow(all.VT.fin))
  train_ind <- sample(seq_len(nrow(all.VT.fin)), size = samp)
  
  
  train <- all.VT.fin[-train_ind,]
  #train <- all.VT.fin[-sample,]
  test <- all.VT.fin[ !(rownames(all.VT.fin) %in% rownames(train)), ]

  #predict using the model
  val.rf <- predict(RF, test)
  val.join <- as.data.frame(cbind(test[[17]], val.rf[[1]]))
  date <- test$date
  val.join <- cbind(val.join, as.Date(date))
  
  #use VT name for outputs export
  df.name <- basin
  
  #plot regression graphs
  colnames (val.join) <- c('observed','predicted', 'date')
  
  plot1 <- (ggplot(val.join, aes(as.Date(date))) +                    
    geom_line(aes(y=observed, colour="observed")) +  
    geom_line(aes(y=predicted, colour="predicted")) + 
    scale_colour_manual('',breaks = c('observed', 'predicted'),
                        values = c('observed'="black", 'predicted'="red")) +
    xlab("date") +
    scale_y_continuous("streamflow (l/sec") + 
    labs(title=paste0("observed vs. predicted for ", basin)) + 
    scale_x_date(date_breaks = waiver(), date_labels =  waiver()))
  plot(plot1)
  ggsave(plot=plot1, filename=paste0('D:/THESIS_PP/finalresults/',  Sys.Date(),basin, '_', floss,'_ts_val.png'), device='png', dpi=100, width = 11, height = 8, units='in')
  
  my.formula  <-val.join$predicted ~ val.join$observed
  plot2 <- ggplot(val.join,aes(x=observed,y=predicted))+geom_point()+
    geom_smooth(method = "lm", se = FALSE) +
    stat_poly_eq(formula = my.formula, 
                 eq.with.lhs = "italic(hat(y))~`=`~",
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE) 
  plot(plot2)
  ggsave(plot=plot2, filename=paste0('D:/THESIS_PP/finalresults/',  Sys.Date(), basin, '_', floss,'_reg_val.png'), device='png', dpi=100, width = 11, height = 8, units='in')
  
  
  #write results
  setwd('D:/THESIS_PP/finalresults/')
  write.csv(val.join, file=paste0(basin, '_', floss, ' ','.csv'))
  setwd(mydir)

  return(val.join)
  
}




