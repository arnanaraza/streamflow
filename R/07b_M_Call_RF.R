### FUNCTION TO RUN RANDOM FOREST OF WATERSHED-OF-INTEREST ### 

# Preliminearies
pacman::p_load(randomForest, caTools, lubridate, rfUtilities, ranger, pdp)
mydir <- setwd('D:/THESIS_PP')

# Function that takes valuetable, training data %, and subwatershed codes as arguments to run randomForest
runRF <- function (VT, td, SW, floss) {
  #prepare training data
  #subs <- subset(VT, select = grep(SW, names(VT)))
  #col.pos <- which(colnames(subs)==colnames(VT))
  vt.nonNA <- na.omit(VT)
  set.seed(123)
  sample <-  sample(1:nrow(vt.nonNA), size=(1-td)*nrow(vt.nonNA))
  train <- vt.nonNA[-sample,]
  train$date <-  lubridate::ymd(train$date)
  train$mo <- NA
  train$mo <- month(train$date)


  if (floss == 'yes'){
    train <- train [,-53]
    col.pos <- 17}
  if (floss == 'no'){
    train <- train [,-53]
    train <- train [,-c(1:11)]
    col.pos <- 6}
  
  #tune for best mtry value
  #tune <- tuneRF(x=train[ ,c(1:length(train))], y=train[[col.pos]])
  
  #run the model, plot and save VIm and PDP
  df.name <- deparse(substitute(VT))
  #modelRF <- ranger(train[[col.pos]] ~ ., data=train[,-col.pos], importance='impurity', num.trees=1000, mtry=36)
  modelRF <- randomForest(train[[col.pos]] ~ ., data=train[,-col.pos], importance=T, ntree=2000, mtry=36)
  
  #save impt graphs
  png(filename=paste0('D:/THESIS_PP/finalresults/', SW, '_', floss, '_MSE', '.png'))
  varImpPlot(modelRF, type = 1)
  dev.off()
  
  png(filename=paste0('D:/THESIS_PP/finalresults/', SW, '_',floss, '_gi', '.png'))
  varImpPlot(modelRF, type = 2)
  dev.off()
  
  if (floss == 'yes'){
    png(filename=paste0('D:/THESIS_PP/finalresults/',  Sys.Date(), '_',SW, '_',floss,'_fl_pdp', '.png'))
    partialPlot(modelRF, train[,-col.pos], x.var = 'LC11.fl' ,plot = TRUE)
    dev.off()
    
    png(filename=paste0('D:/THESIS_PP/finalresults/',  Sys.Date(), '_',SW, '_',floss,'_CN_pdp', '.png'))
    partialPlot(modelRF, train[,-col.pos], x.var = 'LC9.CN' ,plot = TRUE)
    dev.off()
    
    png(filename=paste0('D:/THESIS_PP/finalresults/',  Sys.Date(), '_',SW, '_',floss,'_X2_pdp', '.png'))
    partialPlot(modelRF, train[,-col.pos], x.var = 'LC2.for' ,plot = TRUE)
    dev.off()
    
    png(filename=paste0('D:/THESIS_PP/finalresults/',  Sys.Date(), '_',SW, '_',floss,'_man_pdp', '.png'))
    partialPlot(modelRF, train[,-col.pos], x.var = 'LC10.man' ,plot = TRUE)
    dev.off()}
  
  return(modelRF)


}

