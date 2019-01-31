### MAIN MODELING FUNCTION FOR MODEL TRAINING AND TESTING FOR BASIN LEVEL METHOD ### 

# Preliminearies
if (!require("pacman")) install.packages("pacman")
mydir <- setwd('D:/THESIS_PP')


# Calling create valuetable (VT) function run per SW

# arguments - SW code and yes-no / with or without conversion
source('07_Scripts/07a_PP_CreateValuetables.R')
SW.list <- c('aarb_a', 'aarb_n', 'abrb_s','arb_b', 'arb_c', 'crb_a', 'crb_be', 
             'crb_bu', 'crb_d', 'crb_j','crb_m', 'crb_p', 'crb_s', 
             'crb_t', 'crb_u', 'mrb_s', 'prb_b', 'prb_c', 'prb_a', 
             'prb_p', 'prb_r')
SW.list <- as.list(SW.list)
  # attempt to integrate known land use conversion for aarb_a, crb_a, prb_a
  SW.conv <- c ('yes', 'no', 'no', 'no', 'no','yes', 'no', 'yes', 'no','no','no','yes','no','no','no','no','no','no','yes', 'no', 'yes')


# Format VT
all.VT <- lapply(SW.list, function(x) GetVT(x, SW.conv))
all.VT1 <- lapply(all.VT, function(x) x[-c(1:730),]) #remove 1998-1999 years
vt.names <- c('LC1.agr', 'LC2.for', 'LC3.gr', 'LC4.wgr', 'LC5.wetl', 
              'LC6.riv', 'LC7.bu', 'LC8.barr', 'LC9.CN', 'LC10.man',
              'LC11.fl', 'W1.ws', 'W2.tmin', 'W3.tmax', 'W4.pcp', 'W5.rh', 'O.obs',
              'P.bd1', 'P.br1', 'P.cl1', 'P.sa1', 'P.si1', 'P.dem1', 'P.sl1', 
              'P.bd2', 'P.br2', 'P.cl2', 'P.sa2', 'P.si2', 'P.dem2', 'P.sl2',
              'P.bd3', 'P.br3', 'P.cl3', 'P.sa3', 'P.si3', 'P.dem3', 'P.sl3',
              'P.bd4', 'P.br4', 'P.cl4', 'P.sa4', 'P.si4', 'P.dem4', 'P.sl4', 
              'P.bd5', 'P.br5', 'P.cl5', 'P.sa5', 'P.si5', 'P.dem5', 'P.sl5', 
              'date', 'W6.pcpw', 'W7.pcpm', 'W8.pcpmx')


# Calling runRF function and providing valuetable, %training data, and SW name, and FLAC string as arguments 
source('07_Scripts/07b_M_Call_RF.R')
all.RF.yes <- lapply((1:length(all.VT1)), function(x) runRF(all.VT1[[x]], 0.90, SW.list[[x]], 'yes'))
all.RF.no <- lapply((1:length(all.VT1)), function(x) runRF(all.VT1[[x]], 0.90, SW.list[[x]], 'no'))


# Predict from test data and graph accuracy
source('07_Scripts/07b_M_PredictFromTestData.R')
#for R2
all.pred.yes <- lapply((1:length(all.VT1)), function(x) pred(all.VT1[[x]], 0.95, SW.list[[x]], all.RF.yes[[x]], 'yes'))
all.pred.no <- lapply((1:length(all.VT1)), function(x) pred(all.VT1[[x]], 0.95, SW.list[[x]], all.RF.no[[x]], 'no'))
#for NSE
all.NSE.yes <- lapply(1:length(all.pred.yes), function(x) NSE(obs=as.numeric(all.pred.yes[[x]][[1]]), sim=as.numeric(all.pred.yes[[x]][[2]]), na.rm = T))
all.NSE.yes <- t(as.data.frame(all.NSE.yes))
all.NSE.no <- lapply(1:length(all.pred.no), function(x) NSE(obs=as.numeric(all.pred.no[[x]][[1]]), sim=as.numeric(all.pred.no[[x]][[2]]), na.rm = T))
all.NSE.no <- t(as.data.frame(all.NSE.no))


# Predict at full-scale
source('07_Scripts/07b_M_PredictFullScale.R')
all.predall.yes <- lapply((1:length(all.VT1)), function(x) predAll(all.VT1[[x]], SW.list[[x]], modelRF, 'yes'))
all.predall.no <- lapply((1:length(all.VT1)), function(x) predAll(all.VT1[[x]], SW.list[[x]], modelRF.no, 'no'))

