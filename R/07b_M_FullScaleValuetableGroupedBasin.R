## FUNCTION CREATE FULL-SCALE FORMAT VALUETABLES FOR BASINN METHOD ##

# Preliminearies
pacman::p_load(caTools, lubridate)
mydir <- setwd('D:/THESIS_PP')

getVT <- function(basin){
  
  #get VT per SW 
  if (basin == 'aarb'){
    SW.list <-  c('aarb_a', 'aarb_n')
    SW.conv <- c ('yes', 'no')}
  if (basin == 'abrb'){
    SW.list <-  'abrb_s'
    SW.conv <-'no'}
  if (basin == 'arb'){
    SW.list <-  c('arb_b', 'arb_c')
    SW.conv <- c ('no', 'no')}
  if (basin == 'crb'){
    SW.list <- c('crb_a', 'crb_be', 'crb_bu', 'crb_d', 'crb_j','crb_m', 'crb_p', 'crb_s', 'crb_t', 'crb_u')
    SW.conv <- c('yes', 'no', 'no', 'no','no','no','no','no','no','no')}
  if (basin == 'mrb'){
    SW.list <-  'mrb_s'
    SW.conv <-'no'}
  if (basin == 'prb'){
    SW.list <- c('prb_b', 'prb_c', 'prb_a', 'prb_p', 'prb_r')
    SW.conv <- c('no','no','yes', 'no', 'no')}
  
  source('07_Scripts/07a_PP_CreateValuetables.R')
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
 
return (all.VT4)  
}





