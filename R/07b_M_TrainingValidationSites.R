### FUNCTION TO TRAIN DATA, PREDICT FROM TEST DATA, AND PREDICT AT FULL-SCALE FOR 6 VALIDATION SITES ### 

# Preliminearies
if (!require("pacman")) install.packages("pacman")
mydir <- setwd('D:/THESIS_PP')


# Calling function to assemble VT per basin, returns training data
source('scripts/07b_M_CreateTrainingDataLearnallSize.r') # merging VTs, %training data, and SW name as arguments 
all.VT.yes <- mergeVT('all', .90, 'yes')
all.VT.no <- mergeVT('all', 0.90, 'no')


# Running RF / ranger per basin, 
all.RF.yes <- ranger(all.VT.yes[[17]] ~ ., data=all.VT.yes[,-17], 
                   importance='impurity', splitrule='variance',mtry=25, num.trees=2000)
all.RF.no <- ranger(all.VT.no[[6]] ~ ., data=all.VT.no[,-6], 
                 importance='impurity', splitrule='variance',mtry=21, num.trees=2000)


# Full-scale prediction per SW
source('07_Scripts/07b_M_FullScaleValuetableValidationSites.R')
valid.list <- c('crb_vs', 'abrb_vl','prb_vl', 'abrb_vs', 'aarb_vs', 'arb_vs')
valid.VT1.yes <- getVT1(all.list, 'yes')
valid.VT1.no <- getVT1(all.list, 'no')


source('07_Scripts/07b_M_PredictFullScaleValidationSites.R')
valid.predall.yes <- lapply((1:length(valid.VT1.yes)), function(x) predValid(valid.VT1.yes[[x]], valid.list[[x]], all.RF, 'yes'))
view.yes <- multiplot(plotlist = valid.predall.yes, cols = 2)
valid.predall.no <- lapply((1:length(valid.VT1.no)), function(x) predValid(valid.VT1.no[[x]], valid.list[[x]], all.RF, 'no'))
view.no <- multiplot(plotlist = valid.predall.no, cols = 2)

# Plot fancy graphs
source('07_Scripts/07c_V_CreateHydrographsValidationSites.R')
valid.graphs.yes <- lapply((1:length(valid.VT1.yes)), function(x) plotGraphs1(valid.VT1.yes[[x]], valid.predall.yes[[x]], valid.list[[x]]))
valid.graphs.no <- lapply((1:length(valid.VT1.no)), function(x) plotGraphs1(valid.VT1.no[[x]], valid.predall.no[[x]], valid.list[[x]]))

source('07_Scripts/07c_V_HistoricalComparisonValidationSites.R')
valid.hist.yes <- lapply((1:length(valid.list)), function(x) getHist1(valid.graphs.yes[[x]], valid.list[[x]], 'yes'))
valid.hist.yes <- Map(cbind, valid.hist.yes, SW = valid.list)


# Plot rainfall
source('07_Scripts/07c_V_WithRainfall.R')
valid.pcp.yes <- lapply((1:length(valid.VT1.yes)), function(x) plotPcp(valid.graphs.yes[[x]], valid.list[[x]]))
valid.yes <- do.call(rbind, Map(data.frame, A=valid.hist.yes, B=valid.pcp.yes))
colnames(valid.yes) <- c('wet.flow', 'year', 'f.loss', 'SW', 'pcp','year1' )
write.csv(valid.yes, 'valid.reg.wet.csv')
#valid.reg.pivot <- dcast(setDT(valid.yes), year~SW, value.var=c('dry.flow','f.loss', 'pcp'), fun.aggregate=sum)


