### FUNCTION TO CREATE GROUPED LEARN-ALL AND SIZE-BASED VALUETABLES ### 

# Preliminearies
pacman::p_load(randomForest, caTools, lubridate, rfUtilities, ranger, hydroGOF)
mydir <- setwd('D:/THESIS_PP')

mergeVT <- function(basin,td, floss){
  
  # arrange subwatersheds accordingly per size and learn-all
  if (basin == 'l'){
    SW.list <-  c('aarb_a',	'crb_a',	'crb_bu',	'crb_d',	'crb_j',	'crb_m',	'crb_p', 	'crb_u',	'prb_a',	'prb_r')
    SW.conv <- c ('yes', 'yes', 'no', 'no', 'no', 'no', 'no', 'no', 'yes', 'no')}
  if (basin == 's'){
    SW.list <-  c('aarb_n',	'abrb_s',	'arb_b',	'arb_c',	'crb_be',	'crb_s',	'crb_t',	'mrb_s',	'prb_b',	'prb_c',	'prb_p')
    SW.conv <- c ('no', 'no', 'no', 'no', 'no', 'no', 'no', 'no', 'no','no', 'no')}
  if (basin == 'all'){
    SW.list <- c('aarb_a', 'aarb_n', 'abrb_s','arb_b', 'arb_c', 'crb_a', 'crb_be', 
                 'crb_bu', 'crb_d', 'crb_j','crb_m', 'crb_p', 'crb_s', 
                 'crb_t', 'crb_u', 'mrb_s', 'prb_b', 'prb_c', 'prb_a', 
                 'prb_p', 'prb_r')    
    SW.conv <- c ('yes', 'no', 'no', 'no', 'no','yes', 'no', 'no', 'no','no',
                  'no','no','no','no','no','no','no','no','yes', 'no', 'no')}
  source('scripts/07a_PP_CreateValuetables.R')
  all.VT <- lapply(SW.list, function(x) GetVT(x, SW.conv)) # yes are aarb_a, crb_a, prb_a
  all.VT1 <- lapply(all.VT, function(x) x[-c(1:730),])
  
  
  # create one VT per basin
  vt.code <- c(1:52)
  vt.names <- c('LC1.agr', 'LC2.for', 'LC3.gr', 'LC4.wgr', 'LC5.wetl', 
                'LC6.riv', 'LC7.bu', 'LC8.barr', 'LC9.CN', 'LC10.man',
                'LC11.fl', 'W1.ws', 'W2.tmin', 'W3.tmax', 'W4.pcp', 'W5.rh', 'O.obs',
                'P.bd1', 'P.br1', 'P.cl1', 'P.sa1', 'P.si1', 'P.dem1', 'P.sl1', 
                'P.bd2', 'P.br2', 'P.cl2', 'P.sa2', 'P.si2', 'P.dem2', 'P.sl2',
                'P.bd3', 'P.br3', 'P.cl3', 'P.sa3', 'P.si3', 'P.dem3', 'P.sl3',
                'P.bd4', 'P.br4', 'P.cl4', 'P.sa4', 'P.si4', 'P.dem4', 'P.sl4', 
                'P.bd5', 'P.br5', 'P.cl5', 'P.sa5', 'P.si5', 'P.dem5', 'P.sl5', 
                'date', 'W6.pcpw', 'W7.pcpm', 'W8.pcpmx')
  all.VT.x <- lapply(1:length(all.VT1), function(x) all.VT1[[x]]$co = rep(vt.code[x], nrow(all.VT1[[x]])))
  all.VT2 <- lapply(all.VT1, setNames, nm = vt.names)
  all.VT3 <- lapply(all.VT2, function(x) unfactor(x))
  all.VT4 <- lapply(1:length(all.VT3), function(x) cbind(all.VT3[[x]], all.VT.x[[x]]))
  all.VT.fin <- do.call("rbind", all.VT4)
  all.VT.fin <- na.omit(all.VT.fin)
  all.VT.fin$mo <- month(all.VT.fin$date)
  vt.names <- c('LC1.agr', 'LC2.for', 'LC3.gr', 'LC4.wgr', 'LC5.wetl', 
                'LC6.riv', 'LC7.bu', 'LC8.barr', 'LC9.CN', 'LC10.man',
                'LC11.fl', 'W1.ws', 'W2.tmin', 'W3.tmax', 'W4.pcp', 'W5.rh', 'O.obs',
                'P.bd1', 'P.br1', 'P.cl1', 'P.sa1', 'P.si1', 'P.dem1', 'P.sl1', 
                'P.bd2', 'P.br2', 'P.cl2', 'P.sa2', 'P.si2', 'P.dem2', 'P.sl2',
                'P.bd3', 'P.br3', 'P.cl3', 'P.sa3', 'P.si3', 'P.dem3', 'P.sl3',
                'P.bd4', 'P.br4', 'P.cl4', 'P.sa4', 'P.si4', 'P.dem4', 'P.sl4', 
                'P.bd5', 'P.br5', 'P.cl5', 'P.sa5', 'P.si5', 'P.dem5', 'P.sl5', 
                'date', 'W6.pcpw', 'W7.pcpm', 'W8.pcpmx', 'SW', 'mo')
  colnames(all.VT.fin) <- vt.names
  setwd('D:/THESIS_PP/finalresults/VT/')  #setup directory beforehand
  write.csv(all.VT.fin, paste0(basin,'.csv'))
  setwd(mydir)
  all.VT.fin <- all.VT.fin[,-53]
  
  # create training-testing data
  if (td < 1){
    set.seed(123)
    train <- sample(seq_len(nrow(all.VT.fin)), size = floor((1-td)*nrow(all.VT.fin)))
    train <- all.VT.fin[-train,]}
  #sample <-  sample(1:nrow(all.VT.fin), size=(1-td)*nrow(all.VT.fin))
  #train <- all.VT.fin[-sample,]}
  if (td == 1){
    train <- all.VT.fin}
  
  if (floss == 'yes'){
    train <- train}
  if (floss == 'no'){
    train <- train [,-c(1:11)]}
  

return (train)  
}





