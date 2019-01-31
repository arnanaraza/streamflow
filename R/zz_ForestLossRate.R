## FUNCTION TO COMPUTE FOREST LOSS RATE RELATIVE TO BASELINE FOREST COVER
library(rgdal)
lossRate <- function(SW){
  shape <- readOGR(dsn = 'D:/THESIS_PP/mid-results', layer = "sw")
  f.table <- read.csv('D:/THESIS_PP/mid-results/sw_lcov_floss2.csv')
  if (SW == 'aarb_a') {
    SW.no <- c(7, 8, 9, 10, 11, 14, 15, 16, 17, 19, 20, 23, 24) }
  if (SW == 'aarb_n') {
    SW.no <- 8}
  if (SW == 'abrb_s') {
    SW.no <- 59}
  if (SW == 'arb_b')  { 
    SW.no <- 166}
  if (SW == 'arb_c')  { 
    SW.no <- 188}
  if (SW == 'crb_a') {
    SW.no <- 156}
  if (SW == 'crb_be') {
    SW.no <- 150}
  if (SW == 'crb_bu') {
    SW.no <- c(68,69,71,74,75,76,78,79,82,83,84,86,88,90,91,92,93,94,96,98,99,100,101,103,104,105,106,107,108,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,138,137,141,143,144,145,147,148,149,150,151,152,153,154,155,156,157,158,159,160,163)}  
  if (SW == 'crb_d') {
    SW.no <- c(163, 160, 159, 158, 157)}
  if (SW == 'crb_j') {
    SW.no <- c(137, 147, 139, 149, 143, 153, 151, 154, 157, 158, 159, 160, 163)} 
  if (SW == 'crb_m') {
    SW.no <- c(135, 138, 144, 145, 146, 148, 152, 155, 150)}
  if (SW == 'crb_p') {
    SW.no <- c(91, 92, 98, 99, 101, 102, 104, 110, 114, 119, 120, 126, 130, 132, 141)}
  if (SW == 'crb_s') {
    SW.no <- c(62, 57, 50)}
  if (SW == 'crb_t') {
    SW.no <- 52}
  if (SW == 'crb_u') {
    SW.no <- c(94,96,100,103,105,106,107,108,111,112,113,115,116,117,118,121,122,123,124,125,127,128,129,131,133,134,135,136,137,138,139,143,144,145,147,149,150,151,152,153,154,155,156,157,158,159,160,163)}
  if (SW == 'mrb_s') {
    SW.no <- 242}
  if (SW == 'prb_b') {
    SW.no <- 189}
  if (SW == 'prb_c') {
    SW.no <- c(185,187,193,194,198)}
  if (SW == 'prb_a') {
    SW.no <- c(203, 204, 205,209,210,192, 218,167,168,170,185,186,187,189,190,191,193,194,195,196,198,201,202,206,207,208,211,212,213,214,215,216,217,225)}
  if (SW == 'prb_p')  { 
    SW.no <- c(209, 210, 205, 218)}
  if (SW == 'prb_r') {
    SW.no <- c(189,190,195,196,201)}
  
  
  SW.fl <- filter(f.table, f.table$Subbasin %in% SW.no) 
  SW.fl <- SW.fl[,-c(1,3)]
  SW.fl$gridcode_1 <- as.integer(as.character(SW.fl$gridcode_1))
  SW.fl$gridcode_2 <- as.integer(as.character(SW.fl$gridcode_2))
  
  
  #groups forest loss and land cover area
  fl.grp <- SW.fl %>% 
    group_by(SW.fl$gridcode_1) %>% 
    summarise_all(funs(sum))
  fl.grp1 <- SW.fl %>% 
    group_by(SW.fl$gridcode_2) %>% 
    summarise_all(funs(sum))
  fl.grp <- fl.grp[,-c(2:4)]
  fl.grp1 <- fl.grp1[,-c(2:4)]
  fl.grp <- na.omit(fl.grp)
  fl.grp1 <- na.omit(fl.grp1)
  col.floss <- na.omit(SW.fl)
  col.floss <- sum(col.floss$ha)
  col.for <- SW.fl[grep("2", SW.fl$gridcode_1), ]
  col.for <- sum(col.for$ha)
  loss.rate <- col.floss/col.for
  col.for16 <- col.for-col.floss
  
return (col.for16)
}

SW.list <- c('aarb_a', 'aarb_n', 'abrb_s','arb_b', 'arb_c', 'crb_a', 'crb_be', 
             'crb_bu', 'crb_d', 'crb_j','crb_m', 'crb_p', 'crb_s', 
             'crb_t', 'crb_u', 'mrb_s', 'prb_b', 'prb_c', 'prb_a', 
             'prb_p', 'prb_r')

floss.list <- as.data.frame(mapply (lossRate, SW=SW.list))
floss.list1 <- as.data.frame((floss.list))
