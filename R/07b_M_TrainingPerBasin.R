### MAIN MODELING FUNCTION FOR MODEL TRAINING AND TESTING FOR BASIN LEVEL METHOD ### 

# Preliminearies
if (!require("pacman")) install.packages("pacman")
mydir <- setwd('D:/THESIS_PP')

# Calling function to assemble VT per basin, returns training data
source('07_Scripts/07b_M_CreateTrainingDataLearnallSize.R') # merging VTs, %training data, and SW name as arguments 
aarb.VT.yes <- mergeVT('aarb', 0.90, 'yes')
abrb.VT.yes <- mergeVT('abrb', 0.90, 'yes')
arb.VT.yes <- mergeVT('arb', 0.90, 'yes')
crb.VT.yes <- mergeVT('crb', 0.90, 'yes')
mrb.VT.yes <- mergeVT('mrb', 0.90, 'yes')
prb.VT.yes <- mergeVT('prb', 0.90, 'yes')

aarb.VT.no <- mergeVT('aarb', .90, 'no')
abrb.VT.no <- mergeVT('abrb', .90, 'no')
arb.VT.no <- mergeVT('arb', .90, 'no')
crb.VT.no <- mergeVT('crb', .90, 'no')
mrb.VT.no <- mergeVT('mrb', .90, 'no')
prb.VT.no <- mergeVT('prb', .90, 'no')

# Running RF / ranger per basin
  #with FLAC
  aarb.RF <- ranger(aarb.VT.yes[[17]] ~ ., data=aarb.VT.yes[,-17], 
                    importance='impurity', splitrule='variance', mtry=25, num.trees=2500)
  abrb.RF <- ranger(abrb.VT.yes[[17]] ~ ., data=abrb.VT.yes[,-17], 
                    importance='impurity', splitrule='variance', mtry=25, num.trees=2500)
  arb.RF <- ranger(arb.VT.yes[[17]] ~ ., data=arb.VT.yes[,-17], 
                    importance='impurity', splitrule='variance', mtry=25, num.trees=2500)
  crb.RF <- ranger(crb.VT.yes[[17]] ~ ., data=crb.VT.yes[,-17], 
                   importance='impurity', splitrule='variance', mtry=25, num.trees=2500)
  mrb.RF <- ranger(mrb.VT.yes[[17]] ~ ., data=mrb.VT.yes[,-17], 
                    importance='permutation', splitrule='variance', mtry=25, num.trees=2500)
  prb.RF <- ranger(prb.VT.yes[[17]] ~ ., data=prb.VT.yes[,-17], 
                   importance='impurity',splitrule='variance', mtry=25, num.trees=2500)
  #without FLAC
  aarb.RF.no <- ranger(aarb.VT.no[[6]] ~ ., data=aarb.VT.no[,-6], 
                    importance='impurity', splitrule='variance', mtry=21, num.trees=2000)
  abrb.RF.no <- ranger(abrb.VT.no[[6]] ~ ., data=abrb.VT.no[,-6], 
                    importance='impurity', splitrule='variance', mtry=21, num.trees=2000)
  arb.RF.no <- ranger(arb.VT.no[[6]] ~ ., data=arb.VT.no[,-6], 
                   importance='impurity', splitrule='variance', mtry=21, num.trees=2000)
  crb.RF.no <- ranger(crb.VT.no[[6]] ~ ., data=crb.VT.no[,-6], 
                   importance='impurity', splitrule='variance', mtry=21, num.trees=2000)
  mrb.RF.no <- ranger(mrb.VT.no[[6]] ~ ., data=mrb.VT.no[,-6], 
                   importance='impurity', splitrule='variance', mtry=21, num.trees=2000)
  prb.RF.no <- ranger(prb.VT.no[[6]] ~ ., data=prb.VT.no[,-6], 
                   importance='impurity', splitrule='variance', mtry=21, num.trees=2000)

# Plot PDP
learner = makeLearner("regr.ranger")
task = makeRegrTask(data = prb.VT.yes, target = "O.obs")
ps <- makeParamSet(makeIntegerParam("mtry", 23, 34),
                   makeDiscreteParam("num.trees", 2000))
rdesc <- makeResampleDesc("CV", iters = 5)
res = tuneParams(learner, task, rdesc, par.set = ps,
                 control = makeTuneControlGrid())
lrn = setHyperPars(makeLearner("regr.ranger"), par.vals = res$x)
fit.reg = train(lrn, task)
pd = generatePartialDependenceData(fit.reg, task)
plotPartialDependence(pd) #facet.wrap.nrow=7)


# Test fitted model using test data, plot regression results 
source('07_Scripts/07b_M_PredictFromTestData.R')
  #using regression with FLAC
  aarb.pred.yes <- pred(0.90, 'aarb', aarb.RF, 'yes')
  abrb.pred.yes <- pred(0.90, 'abrb', abrb.RF, 'yes')
  arb.pred.yes <- pred(0.90, 'arb', arb.RF, 'yes')
  crb.pred.yes <- pred(0.90, 'crb', crb.RF, 'yes')
  mrb.pred.yes <- pred(0.90, 'mrb', mrb.RF, 'yes')
  prb.pred.yes <- pred(0.90, 'prb', prb.RF, 'yes')

  #using NSE with FLAC
  aarb.nse.yes <- NSE(obs=aarb.pred.yes$observed, sim=aarb.pred.yes$predicted, na.rm=T)
  abrb.nse.yes <- NSE(obs=abrb.pred.yes$observed, sim=abrb.pred.yes$predicted, na.rm=T)
  arb.nse.yes <- NSE(obs=arb.pred.yes$observed, sim=arb.pred.yes$predicted, na.rm=T)
  crb.nse.yes <- NSE(obs=crb.pred.yes$observed, sim=crb.pred.yes$predicted, na.rm=T)
  mrb.nse.yes <- NSE(obs=mrb.pred.yes$observed, sim=mrb.pred.yes$predicted, na.rm=T)
  prb.nse.yes <- NSE(obs=prb.pred.yes$observed, sim=prb.pred.yes$predicted, na.rm=T)

  #using regression without FLAC
  aarb.pred.no <- pred(0.90, 'aarb', aarb.RF.no, 'no')
  abrb.pred.no <- pred(0.90, 'abrb', abrb.RF.no, 'no')
  arb.pred.no <- pred(0.90, 'arb', arb.RF.no, 'no')
  crb.pred.no <- pred(0.90, 'crb', crb.RF.no, 'no')
  mrb.pred.no <- pred(0.90, 'mrb', mrb.RF.no, 'no')
  prb.pred.no <- pred(0.90, 'prb', prb.RF.no, 'no')
  
  #using NSE without FLAC
  aarb.nse.no <- NSE(obs=aarb.pred.no$observed, sim=aarb.pred.no$predicted, na.rm=T)
  abrb.nse.no <- NSE(obs=abrb.pred.no$observed, sim=abrb.pred.no$predicted, na.rm=T)
  arb.nse.no <- NSE(obs=arb.pred.no$observed, sim=arb.pred.no$predicted, na.rm=T)
  crb.nse.no <- NSE(obs=crb.pred.no$observed, sim=crb.pred.no$predicted, na.rm=T)
  mrb.nse.no <- NSE(obs=mrb.pred.no$observed, sim=mrb.pred.no$predicted, na.rm=T)
  prb.nse.no <- NSE(obs=prb.pred.no$observed, sim=prb.pred.no$predicted, na.rm=T)

# Full-scale prediction per Basin
source('07_Scripts/07b_M_FullScaleValuetableGroupedBasin.R')
  #generate grouped VT
  aarb.SW.VT.yes <- getVT('aarb')
  abrb.SW.VT.yes <- getVT('abrb')
  arb.SW.VT.yes <- getVT('arb')
  crb.SW.VT.yes <- getVT('crb')
  mrb.SW.VT.yes <- getVT('mrb')
  prb.SW.VT.yes <- getVT('prb')

  #just a list per of basins as string inputs to next function
  aarb.list <-  c('aarb_a', 'aarb_n')
  abrb.list <-  'abrb_s'
  arb.list <-  c('arb_b', 'arb_c')
  crb.list <- c('crb_a', 'crb_be', 'crb_bu', 'crb_d', 'crb_j','crb_m', 'crb_p', 'crb_s', 'crb_t', 'crb_u')
  mrb.list <-  'mrb_s'
  prb.list <-  c('prb_b', 'prb_c', 'prb_a', 'prb_p', 'prb_r')

  #predict at full-scale
  source('07_Scripts/07b_M_PredictFullScale.R')
  #with FLAC
  all.predall.yes.aarb <- lapply((1:length(aarb.SW.VT.yes)), function(x) predAll(aarb.SW.VT.yes[[x]], aarb.list[[x]], aarb.RF, 'yes'))
  all.predall.yes.abrb <- lapply((1:length(abrb.SW.VT.yes)), function(x) predAll(abrb.SW.VT.yes[[x]], abrb.list[[x]], abrb.RF, 'yes'))
  all.predall.yes.arb <- lapply((1:length(arb.SW.VT.yes)), function(x) predAll(arb.SW.VT.yes[[x]], arb.list[[x]], arb.RF, 'yes'))
  all.predall.yes.crb <- lapply((1:length(crb.SW.VT.yes)), function(x) predAll(crb.SW.VT.yes[[x]], crb.list[[x]], crb.RF, 'yes'))
  all.predall.yes.mrb <- lapply((1:length(mrb.SW.VT.yes)), function(x) predAll(mrb.SW.VT.yes[[x]], mrb.list[[x]], mrb.RF, 'yes'))
  all.predall.yes.prb <- lapply((1:length(prb.SW.VT.yes)), function(x) predAll(prb.SW.VT.yes[[x]], prb.list[[x]], prb.RF, 'yes'))
  
  #without FLAC
  all.predall.no.aarb <- lapply((1:length(aarb.SW.VT.yes)), function(x) predAll(aarb.SW.VT.yes[[x]], aarb.list[[x]], aarb.RF.no, 'no'))
  all.predall.no.abrb <- lapply((1:length(abrb.SW.VT.yes)), function(x) predAll(abrb.SW.VT.yes[[x]], abrb.list[[x]], abrb.RF.no, 'no'))
  all.predall.no.arb <- lapply((1:length(arb.SW.VT.yes)), function(x) predAll(arb.SW.VT.yes[[x]], arb.list[[x]], arb.RF.no, 'no'))
  all.predall.no.crb <- lapply((1:length(crb.SW.VT.yes)), function(x) predAll(crb.SW.VT.yes[[x]], crb.list[[x]], crb.RF.no, 'no'))
  all.predall.no.mrb <- lapply((1:length(mrb.SW.VT.yes)), function(x) predAll(mrb.SW.VT.yes[[x]], mrb.list[[x]], mrb.RF.no, 'no'))
  all.predall.no.prb <- lapply((1:length(prb.SW.VT.yes)), function(x) predAll(prb.SW.VT.yes[[x]], prb.list[[x]], prb.RF.no, 'no'))


# Create hydrographs and return a time series of streamflow and rainfall or forest loss
source('07_Scripts/07c_V_CreateHydrographs.R')
aarb.graphs.yes <- lapply((1:length(aarb.SW.VT.yes)), function(x) plotGraphs(aarb.SW.VT.yes[[x]], all.predall.yes.aarb[[x]], aarb.list[[x]]))
abrb.graphs.yes <- lapply((1:length(prb.SW.VT.yes)), function(x) plotGraphs(prb.SW.VT.yes[[x]], all.predall.yes.prb[[x]], prb.list[[x]]))
arb.graphs.yes <- lapply((1:length(prb.SW.VT.yes)), function(x) plotGraphs(prb.SW.VT.yes[[x]], all.predall.yes.prb[[x]], prb.list[[x]]))
crb.graphs.yes <- lapply((1:length(prb.SW.VT.yes)), function(x) plotGraphs(prb.SW.VT.yes[[x]], all.predall.yes.prb[[x]], prb.list[[x]]))
mrb.graphs.yes <- lapply((1:length(prb.SW.VT.yes)), function(x) plotGraphs(prb.SW.VT.yes[[x]], all.predall.yes.prb[[x]], prb.list[[x]]))
prb.graphs.yes <- lapply((1:length(prb.SW.VT.yes)), function(x) plotGraphs(prb.SW.VT.yes[[x]], all.predall.yes.prb[[x]], prb.list[[x]]))



