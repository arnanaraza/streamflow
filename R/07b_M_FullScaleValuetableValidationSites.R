### FUNCTION TO CREATE COMBINED VALUETABLE FOR 6 VALIDATION SITES ### 

# Preliminearies
pacman::p_load(caTools, lubridate)
mydir <- setwd('D:/THESIS_PP')

getVT1 <- function(SW.list, SW.conv){
  
  source('scripts/07b_M_CreateTrainingDataValidation.r')
  all.VT <- lapply(SW.list, function(x) validVT(x, SW.conv)) # yes are aarb_a, crb_a, prb_a
  all.VT1 <- lapply(all.VT, function(x) x[-c(1:730),])
  
  
  # create one VT per basin
  vt.code <- c(1:52)
  vt.names <- c('LC1.agr', 'LC2.for', 'LC3.gr', 'LC4.wgr', 'LC5.wetl', 
                'LC6.riv', 'LC7.bu', 'LC8.barr', 'LC9.CN', 'LC10.man',
                'LC11.fl', 'W1.ws', 'W2.tmin', 'W3.tmax', 'W4.pcp', 'W5.rh',
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





