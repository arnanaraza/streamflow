### MAIN MODELING FUNCTION FOR MODEL TRAINING AND TESTING FOR SIZE-BASED AND LEARN-ALL METHODS ### 

# Preliminearies
if (!require("pacman")) install.packages("pacman")
mydir <- setwd('D:/ThesisArazaArchive/')

# Calling function to assemble VT per basin, returns training data
source('07_Scripts/07b_M_CreateTrainingDataLearnallSize.R') # VT size, %training data, and FLAC (yes or no) as arguments
large.VT.yes <- mergeVT('l', 0.90, 'yes')
small.VT.yes <- mergeVT('s', 0.90, 'yes')
all.VT.yes <- mergeVT('all', 0.90, 'yes')

large.VT.no <- mergeVT('l', 0.90, 'no')
small.VT.no <- mergeVT('s', 0.90, 'no')
all.VT.no <- mergeVT('all', 0.90, 'no')



# Running RF / ranger per category 
  #with FLAC
  large.RF <- ranger(large.VT.yes[[17]] ~ ., data=large.VT.yes[,-17], 
                    importance='impurity', splitrule='variance', mtry=25, num.trees=2000)
  small.RF <- ranger(small.VT.yes[[17]] ~ ., data=small.VT.yes[,-17], 
                    importance='impurity', splitrule='variance', mtry=25, num.trees=2000)
  all.RF <- ranger(all.VT.yes[[17]] ~ ., data=all.VT.yes[,-17], 
                     importance='impurity', splitrule='variance',mtry=25, num.trees=2000)
  
  #without FLAC
  large.RF.no <- ranger(large.VT.no[[6]] ~ ., data=large.VT.no[,-6], 
                     importance='impurity', splitrule='variance', mtry=21, num.trees=2000)
  small.RF.no <- ranger(small.VT.no[[6]] ~ ., data=small.VT.no[,-6], 
                     importance='impurity', splitrule='variance', mtry=21, num.trees=2000)
  all.RF.no <- ranger(all.VT.no[[6]] ~ ., data=all.VT.no[,-6], 
                   importance='impurity', splitrule='variance',mtry=21, num.trees=2000)

  
  
# Test fitted model using test data, plot regression results 
source('07_Scripts/07b_M_PredictFromTestData.R')
  #with FLAC
  large.pred.yes <- pred(0.90, 'l', large.RF, 'yes')
  small.pred.yes <- pred(0.90, 's', small.RF, 'yes')
  all.pred.yes <- pred(0.90, 'all', all.RF, 'yes')
  
  #without FLAC
  large.pred.no <- pred(0.90, 'l', large.RF.no, 'no')
  small.pred.no <- pred(0.90, 's', small.RF.no, 'no')
  all.pred.no <- pred(0.90, 'all', all.RF.no, 'no')


# Full-scale prediction of Learn-all and Size-based
source('scripts/07b_M_FullScaleValuetableGroupedLearnallAndSize.R')
  #generate grouped VT
  large.VT1.yes <- getVT('l')
  small.VT1.yes <- getVT('s')
  all.VT1.yes <- getVT('all')
  
  #just a list per of basins as string inputs to next function
  large.list <-  c('aarb_a',	'crb_a',	'crb_bu',	'crb_d',	'crb_j',	'crb_m',	'crb_p', 	'crb_u',	'prb_a',	'prb_r')
  small.list <-  c('aarb_n',	'abrb_s',	'arb_b',	'arb_c',	'crb_be',	'crb_s',	'crb_t',	'mrb_s',	'prb_b',	'prb_c',	'prb_p')
  all.list <- c('aarb_a', 'aarb_n', 'abrb_s','arb_b', 'arb_c', 'crb_a', 'crb_be',  'crb_bu', 'crb_d', 'crb_j','crb_m', 'crb_p', 'crb_s',  'crb_t', 'crb_u', 'mrb_s', 'prb_b', 'prb_c', 'prb_a', 'prb_p', 'prb_r')

  #predict at full-scale
  source('scripts/07b_M_PredictFullScale.R')
  large.predall.yes <- lapply((1:length(large.VT.yes)), function(x) predAll(large.VT.yes[[x]], large.list[[x]], large.RF, 'yes'))
  small.predall.yes <- lapply((1:length(small.VT.yes)), function(x) predAll(small.VT.yes[[x]], small.list[[x]], small.RF, 'yes'))
  all.predall.yes <- lapply((1:length(all.VT1.yes)), function(x) predAll(all.VT1.yes[[x]], all.list[[x]], all.RF, 'yes'))

  
# Create hydrographs and return a time series of streamflow and rainfall or forest loss
source('07_Scripts/07c_V_CreateHydrographs.R')
large.graphs.yes <- lapply((1:length(large.VT1.yes)), function(x) plotGraphs(large.VT1.yes[[x]], large.predall.yes[[x]], large.list[[x]]))
small.graphs.yes <- lapply((1:length(small.VT1.yes)), function(x) plotGraphs(small.VT1.yes[[x]], small.predall.yes[[x]], small.list[[x]]))
all.graphs.yes <- lapply((1:length(all.VT1.yes)), function(x) plotGraphs(all.VT1.yes[[x]], all.predall.yes[[x]], all.list[[x]]))


# Graphs for historical flow comparison
source('scripts/07c_V_HistoricalComparison.R')
all.hist.yes <- lapply((1:length(all.list)), function(x) getHist(all.graphs.yes[[x]], all.list[[x]], 'yes'))
all.hist.view <- multiplot(plotlist = all.hist.yes, cols = 3)
all.hist.yes <- Map(cbind, all.hist.yes, SW = all.list)


# Plot rainfall
source('07_Scripts/07c_V_ForestlossAndRainfall.R')
  #generate df with rainfall
  all.pcp.yes <- lapply((1:length(all.VT1.yes)), function(x) plotPcp(all.graphs.yes[[x]], all.list[[x]]))
  
  #combine rainfall to key covariates
  all.yes <- do.call(rbind, Map(data.frame, A=all.hist.yes, B=all.pcp.yes))
  colnames(all.yes) <- c('dry.flow', 'year', 'f.loss', 'SW', 'pcp')
  #reg.pivot <- dcast(setDT(all.yes), year~SW, value.var=c('dry.flow','f.loss', 'pcp'), fun.aggregate=sum)
  
  #write a one full table for analysis
  write.csv(all.yes, 'wet_pcp_floss_REG_3moPCP.csv')

  #graph for all years
  my.formula  <-all.yes$dry.flow ~ all.yes$f.loss
  plot2 <- ggplot(all.yes,aes(x=dry.flow,y=f.loss))+geom_point()+
    geom_smooth(method = "lm", se = FALSE) +
    stat_poly_eq(formula = my.formula, 
                 eq.with.lhs = "italic(hat(y))~`=`~",
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE) 
  plot(plot2)
  ggsave(plot=plot2, filename=paste0('D:/THESIS_PP/final results pcp/',  Sys.Date(), '_', 'floss_dryflow.png'), device='png', dpi=100, width = 11, height = 8, units='in')



# Plot boxplots from time series prediction for seasonality overview
all.box.yes1 <- lapply(1:21, function(x) as.vector(all.hist.yes[[x]]$wet.flow))
all.box.yes1 <- na.omit(all.box.yes1)
all.box <- boxplot(all.box.yes1, las=2,names=all.list,outline=F)

small.box.yes <- lapply(1:11, function(x) as.vector(small.graphs.yes[[x]]$predicted))
small.box.yes1 <- na.omit(small.box.yes)
small.box <- boxplot(small.box.yes1, las=2,names=small.list,outline=F)

large.box.yes <- lapply(1:10, function(x) as.vector(large.graphs.yes[[x]]$predicted))
large.box.yes1 <- na.omit(large.box.yes)
large.box <- boxplot(large.box.yes1, las=2,names=large.list,outline=F)


# Integrate Peakflow and rainfall ratio
  # get starting and end flows
  start.end <- lapply (1:21, function(x) subset(all.hist.yes[[x]]))

  # per yearly avg
  all.list <- c('aarb_a', 'aarb_n', 'abrb_s','arb_b', 'arb_c', 'crb_a', 'crb_be',  'crb_bu', 'crb_d', 'crb_j','crb_m', 'crb_p', 'crb_s',  'crb_t', 'crb_u', 'mrb_s', 'prb_b', 'prb_c', 'prb_a', 'prb_p', 'prb_r')
  all.pp.table <- lapply(1:21, function (x) data.table(all.graphs.yes[[x]], key='pcp'))
  all.pp.table <- lapply(1:21, function(x) cbind(all.pp.table[[x]], pp.ratio=all.pp.table[[x]]$pcp/all.pp.table[[x]]$predicted))
  all.pp.year <- lapply(1:21, function(x) aggregate(all.pp.table[[x]]$pp.ratio ~ year(date), data=all.pp.table[[x]],FUN=mean))
  colnames <- c('date', 'pp.ratio')
  all.pp.year <- lapply(all.pp.year, setNames, colnames)
  sel.pp.year <- lapply(pp.list, function(x) all.pp.year[[x]])
  ggplot(bind_rows(sel.pp.year, .id="df"), aes(date, pp.ratio, colour=df)) +
    geom_line()
  
  # per 100 days
  all.pp.table1 <- lapply(1:21, function(x) all.pp.table[[x]] = all.pp.table[[x]][5000:6210,])
  all.pp.year1 <- lapply(1:21, function(x) aggregate(all.pp.table1[[x]]$pp.ratio ~ year(date) + forest, data=all.pp.table1[[x]],FUN=mean))
  colnames <- c('date', 'forest', 'pp.ratio')
  all.pp.year1 <- lapply(all.pp.year1, setNames, colnames)
  
  pp.list <- c(1:21)
  all.pp.year2 <- lapply(pp.list, function(x) all.pp.year1[[x]])
  ggplot(bind_rows(all.pp.year2, .id="df"), aes(date, pp.ratio, colour=df)) +
    geom_line()
  
  
  my.formula.pcp  <-a$pp.ratio ~ a$forest
  plot <- ggplot(a,aes(x=pp.ratio,y=forest))+geom_point()+
    xlab("rainfall-peak flow ratio") +
    scale_y_continuous("forest loss (ha)") + 
    geom_smooth(method = "lm", se = FALSE) +
    labs(title='relation of rainfall-peakflow ratio to forest loss') +
    stat_poly_eq(formula = my.formula.pcp, 
                 eq.with.lhs = "italic(hat(y))~`=`~",
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE) 
  plot(plot)

