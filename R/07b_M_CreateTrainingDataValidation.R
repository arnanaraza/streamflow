### FUNCTION THAT RETURNS COVARIATE VALUETABLE OF WATERSHED-OF-INTEREST ### 

# Preliminearies
pacman::p_load(raster, rgdal, dplyr, plyr, data.table, reshape2, varhandle, xts, Hmisc, splitstackshape)
mydir <- setwd('D:/THESIS_PP')

# Function proper using SW codes -- prb_p, crb_a, crb_l ... 

validVT <- function (SW, conv){
  shape <- readOGR(dsn = 'D:/THESIS_PP/mid-results', layer = "sw")
  
  
  # Load individual valuetables
  f.table <- read.csv('D:/THESIS_PP/mid-results/sw_lcov_floss2.csv')
  st.table <- read.csv('D:/THESIS_PP/mid-results/static.csv')
  c.table <- read.csv('D:/THESIS_PP/mid-results/conversion1.csv')
  w.table <- read.csv ('D:/THESIS_PP/mid-results/reg_weather1.csv')
  s.table <- read.csv('D:/THESIS_PP/mid-results/obs_data1.csv')
  s.table <- s.table[, -grep("Date.", colnames(s.table))] #remove date col
  
  
  # Setup SW of interest, a lot of if statements

   
  if (SW == 'crb_vs')  { 
    SW.no <- c(33, 40, 34)
    SW.w <-  subset(w.table, select = grep("Tuguegarao", names(w.table)))}
  if (SW == 'abrb_vl')  { 
    SW.no <- c(51, 56, 63, 65, 66, 73, 80, 81, 85, 89, 109)
    SW.w <-  subset(w.table, select = grep("Sinait", names(w.table)))}
  if (SW == 'prb_vl')  { 
    SW.no <- c(231, 232, 240)
    SW.w <-  subset(w.table, select = grep("TCA.Camiling", names(w.table)))}
  if (SW == 'abrb_vs')  { 
    SW.no <- 56
    SW.w <-  subset(w.table, select = grep("Sinait", names(w.table)))}
  if (SW == 'aarb_vs')  { 
    SW.no <-4
    SW.w <-  subset(w.table, select = grep("MMSU", names(w.table)))}
  if (SW == 'arb_vs')  { 
    SW.no <- 199
    SW.w <-  subset(w.table, select = grep("TCA.Camiling", names(w.table)))}
#  if (SW == 'arb_vl')  { 
 #   SW.no <- c(140,142,161)
  #  SW.w <-  subset(w.table, select = grep("TCA.Camiling", names(w.table)))}
  
  # Assembly of SW's valuetable
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
    SW.fl.t <- as.data.frame(t(fl.grp1))
    SW.lc.t <- as.data.frame(t(fl.grp))
    colnames(SW.fl.t) <- as.character(SW.fl.t[1,])
    colnames(SW.lc.t) <- as.character(SW.lc.t[1,])
    SW.fl.t <- SW.fl.t[-1,]
    SW.lc.t <- SW.lc.t[-1,]
    lcover.no <- c(1,2,3,4,5,6,7,8)  #1-agri, 2-forest, 3-grass, 4-wooded grass, 5-wetland, 6-rivers, 7-builtup, 8-barren
    missing <- setdiff(lcover.no, names(SW.lc.t))  # find names of missing land covers
    SW.lc.t[as.character(missing)] <- 0        # fill with NAs
    SW.lc.t <- SW.lc.t[as.character(lcover.no)]    
    
    #integrate weather table for year counts
    w.table$X <- seq(from = as.Date("1998-01-01"), to = as.Date("2016-12-31"), by = 'day')
    w.table$X <- format(as.Date(w.table$X, format="%m/%d/%Y"),"%Y") 
    yr.count <- as.data.frame(table(unlist(w.table$X)))
    yr.count <- yr.count [,-1] #checker for number of days/year
    yrs.repli <-  lapply(1:length(yr.count), function(x) SW.lc.t[rep(rownames(SW.lc.t), times=yr.count[x]), 1:length(SW.lc.t)])
    SW.fl.t <- cbind(b = 0, SW.fl.t)
    SW.fl.t <- cbind(b = 0, SW.fl.t)
    SW.fl.t <- cbind(b = 0, SW.fl.t) #to fill 1998-2000 yrs
    
    
    #apply a function over the years to cummulative deduct forest loss yearly
    z <- list()
    fun <- function (x, y) {
      x[2] = (x[2] - y)
      z <- x[2]
      return (z)}
    y <- lapply(1:length(SW.fl.t), function (x) sum(SW.fl.t[1,1:x]))
    fl.cov <- y
    lf <- mapply (fun, yrs.repli, y=y)
    lf <- lapply(lf, function(x) ifelse(x < 0, 0, x)) 
    
    
    #substitute new forest column 
    lf.list  <- mapply(function(old, new, which) {
      old[,which] <- new
      old
    }, yrs.repli, lf, '2', SIMPLIFY = FALSE)
    

    #integrate conversion
    if (conv == 'yes'){
      c.table <- t(c.table) 
      c.table <- c.table[1:length(SW.fl.t)]
      zz <- list()
      fun.conv <- function (x, y, z) {
        x[z] <- x[z] + y
        zz <- x[z]
        return (zz)  
      }
      y <- lapply(1:length(SW.fl.t), function (x) sum(SW.fl.t[1,1:x]))
      z <- c.table
      conv <- mapply (fun.conv, lf.list, y=y, z=z)
      conv.names <- (names(conv))
      
      lf.list1  <- mapply(function(old, new, which) {
        old[,which] <- new
        old
      }, lf.list, conv, conv.names, SIMPLIFY = FALSE)
      
      #add CN
      CNs <- c(81, 60, 81, 0, 70, 70, 90, 90)
      fun.CN <- function (x) {
        colnames(x) <- gsub('X', '', colnames(x), fixed=T)
        c.names <- as.vector(names(x))
        CN.list <- lapply(as.numeric(c.names), function (y) (sum(x[1,y]/sum(x[1,]) * CNs[y])))
        CN.list <- ifelse(CN.list < 0, 0, CN.list)
        CN.list <- sum(ldply(CN.list, data.frame))
        return (CN.list)}
      CN.all <- mapply (fun.CN, lf.list1)
      CN.all <- as.data.frame(CN.all)
      z <- list()
      fun.CN1 <- function (x, y){
        x$CN = y
        z <- x$CN
        return(z)}
      y <- lapply(c(1:19),function(z) CN.all[z,])
      CN.fin <- mapply (fun.CN1, x=lf.list1, y=y)
      lf.list2  <- mapply(function(old, new, which) {
        old[,which] <- new
        old
      }, lf.list1, CN.fin, 'CN', SIMPLIFY = FALSE)
      
      
      #add manning's coefficient
      m.coef <- c(0.17, 0.60, 0.15, 0.15, 0, 0, 0.01, 0.01)
      fun.Man <- function (x) {
        colnames(x) <- gsub('X', '', colnames(x), fixed=T)
        c.names <- as.vector(names(x))
        m.list <- lapply(as.numeric(c.names), function (y) (sum(x[1,y]/sum(x[1,]) * m.coef[y])))
        m.list <- ifelse(m.list < 0, 0, m.list)
        m.list <- sum(ldply(m.list, data.frame))
        return (m.list)  
      }
      man.all <- mapply (fun.Man, lf.list2)
      man.all <- as.data.frame(man.all)
      m.list <- list()
      fun.Man1 <- function (x, y){
        x$manning = y
        m.list <- x$manning
        return(m.list)
      }
      y <- lapply(c(1:19),function(z) man.all[z,])
      man.fin <- mapply (fun.Man1, x=lf.list2, y=y)
      lf.list3  <- mapply(function(old, new, which) {
        old[,which] <- new
        old
      }, lf.list2, man.fin, 'manning', SIMPLIFY = FALSE)
      
      
      # Add floss as a stand-alone covariate
      f.list <- list()
      fun.fl <- function (x, y) {
        x$fl = y
        f.list <- x$fl
        return(f.list)
      }
      y <- lapply(1:19, function (x) sum(SW.fl.t[1,1:x]))
      fl.fin <- mapply (fun.fl, lf.list3, y=y)
      lf.list4  <- mapply(function(old, new, which) {
        old[,which] <- new
        old
      }, lf.list3, fl.fin, 'fl', SIMPLIFY = FALSE)
      
      #into one dataframe 
      df.lf <- ldply(lf.list4, data.frame)
      
      
      #merging other tables
      s.w.lf <- as.data.frame(cbind(df.lf, SW.w, SW.s))
      
      
      #merging with static valuetable
      SW.static <- filter(st.table, st.table$class %in% SW.no)
      SW.static <- SW.static[,-c(1,9)]
      SW.st.table <- lapply(names(SW.static), function(x) table(SW.static[x]))
      SW.st.df <- as.data.frame(plyr::ldply(SW.st.table, rbind)) #getting value count
      row.names(SW.st.df) <- c('bd', 'br', 'cl', 'sa', 'si', 'dem', 'slope')
      SW.st.df$id <- c(1:length(row.names(SW.st.df)))
      SW.st.melt <- melt(SW.st.df, id.vars = 'id', variable.name = 'all') #in one long column
      SW.st.trans <- as.data.frame(t(SW.st.melt[,2:3])) #transpose
      colnames(SW.st.trans) <- SW.st.melt[,2]
      SW.st.trans <- SW.st.trans[-1,]
      SW.st.repli <- SW.st.trans[rep(rownames(SW.st.trans), times=length(row.names(w.table))), 1:length(SW.st.trans)] #replicate
      colnames(SW.st.repli) <- c('bd1', 'br1', 'cl1', 'sa1', 'si1', 'dem1', 'slope1', 'bd2', 'br2', 'cl2', 'sa2', 'si2', 'dem2', 'slope2', 'bd3', 'br3', 'cl3', 'sa3', 'si3', 'dem3', 'slope3', 'bd4', 'br4', 'cl4', 'sa4', 'si4', 'dem4', 'slope4', 'bd5', 'br5', 'cl5', 'sa5', 'si5', 'dem5', 'slope5')
      s.w.lf.st <-  cbind(s.w.lf, SW.st.repli)
      
      
      # Remove all NA column (if any) 
      for (col in names(s.w.lf.st)) {
        missing <- sum(is.na(s.w.lf.st[,col]))
        if (missing == 6940){
          s.w.lf.st[,col] <- 0}}
      s.w.lf.st$date <- seq(from = as.Date("1998-01-01"), to = as.Date("2016-12-31"), by = 'day')
      
      # Create weekly and monthly pcp
      w.avg <- apply.weekly(xts(s.w.lf.st[,-length(s.w.lf.st)], order.by= s.w.lf.st[,length(s.w.lf.st)]), FUN = mean)
      coi <- w.avg[,15]
      coi <-  coi[rep(seq_len(nrow(coi)), each=7),]
      coi <- as.data.frame(coi)
      coi <- coi[-c(1:4),]
      
      m.avg <- apply.monthly(xts(s.w.lf.st[,-length(s.w.lf.st)], order.by= s.w.lf.st[,length(s.w.lf.st)]), FUN = mean)
      m <- as.data.frame(m.avg[,15])
      freq <- monthDays(row.names(m))
      m <- cbind (m,freq)
      coi1 <- m[rep(row.names(m), m$freq), 1:2]
      
      w.max <- apply.weekly(xts(s.w.lf.st[[15]], order.by= s.w.lf.st[,53]), max)
      coi2 <-  w.max[rep(seq_len(nrow(w.max)), each=7),]
      coi2 <- as.data.frame(coi2)
      coi2 <- coi2[-c(1:4),]
      
      s.w.lf.st$weekpcp <- coi
      s.w.lf.st$mopcp <- coi1[[1]]
      s.w.lf.st$maxpcp <- coi2
      
      
      vt.names <- c('LC1.agr', 'LC2.for', 'LC3.gr', 'LC4.wgr', 'LC5.wetl', 
                    'LC6.riv', 'LC7.bu', 'LC8.barr', 'LC9.CN', 'LC10.man',
                    'LC11.fl', 'W1.ws', 'W2.tmin', 'W3.tmax', 'W4.pcp', 'W5.rh',
                    'P.bd1', 'P.br1', 'P.cl1', 'P.sa1', 'P.si1', 'P.dem1', 'P.sl1', 
                    'P.bd2', 'P.br2', 'P.cl2', 'P.sa2', 'P.si2', 'P.dem2', 'P.sl2',
                    'P.bd3', 'P.br3', 'P.cl3', 'P.sa3', 'P.si3', 'P.dem3', 'P.sl3',
                    'P.bd4', 'P.br4', 'P.cl4', 'P.sa4', 'P.si4', 'P.dem4', 'P.sl4', 
                    'P.bd5', 'P.br5', 'P.cl5', 'P.sa5', 'P.si5', 'P.dem5', 'P.sl5', 
                    'date', 'W6.pcpw', 'W7.pcpm', 'W8.pcpmx')
      
      colnames(s.w.lf.st) <- vt.names
      
      # Write valuetable
      #setwd('D:/THESIS_PP/final results1/')
      #subs <- as.name(SW)
      #fname <- sprintf('%s.csv',deparse(substitute(subs)))
      #write.csv(s.w.lf.st, file=fname)
      #setwd(mydir)
      }
      
    if (conv == 'no') {
      #add CN
      CNs <- c(81, 60, 81, 0, 70, 70, 90, 90)
      fun.CN <- function (x) {
        colnames(x) <- gsub('X', '', colnames(x), fixed=T)
        c.names <- as.vector(names(x))
        CN.list <- lapply(as.numeric(c.names), function (y) (sum(x[1,y]/sum(x[1,]) * CNs[y])))
        CN.list <- ifelse(CN.list < 0, 0, CN.list)
        CN.list <- sum(ldply(CN.list, data.frame))
        return (CN.list)}
      CN.all <- mapply (fun.CN, lf.list)
      CN.all <- as.data.frame(CN.all)
      z <- list()
      fun.CN1 <- function (x, y){
        x$CN = y
        z <- x$CN
        return(z)}
      y <- lapply(c(1:19),function(z) CN.all[z,])
      CN.fin <- mapply (fun.CN1, x=lf.list, y=y)
      lf.list2  <- mapply(function(old, new, which) {
        old[,which] <- new
        old
      }, lf.list, CN.fin, 'CN', SIMPLIFY = FALSE)
      
      
      #add manning's coefficient
      m.coef <- c(0.17, 0.60, 0.15, 0.15, 0, 0, 0.01, 0.01)
      fun.Man <- function (x) {
        colnames(x) <- gsub('X', '', colnames(x), fixed=T)
        c.names <- as.vector(names(x))
        m.list <- lapply(as.numeric(c.names), function (y) (sum(x[1,y]/sum(x[1,]) * m.coef[y])))
        m.list <- ifelse(m.list < 0, 0, m.list)
        m.list <- sum(ldply(m.list, data.frame))
        return (m.list)  
      }
      man.all <- mapply (fun.Man, lf.list2)
      man.all <- as.data.frame(man.all)
      m.list <- list()
      fun.Man1 <- function (x, y){
        x$manning = y
        m.list <- x$manning
        return(m.list)
      }
      y <- lapply(c(1:19),function(z) man.all[z,])
      man.fin <- mapply (fun.Man1, x=lf.list2, y=y)
      lf.list3  <- mapply(function(old, new, which) {
        old[,which] <- new
        old
      }, lf.list2, man.fin, 'manning', SIMPLIFY = FALSE)
      
      # Add floss as a stand-alone covariate
      f.list <- list()
      fun.fl <- function (x, y) {
        x$fl = y
        f.list <- x$fl
        return(f.list)
      }
      y <- lapply(1:19, function (x) sum(SW.fl.t[1,1:x]))
      fl.fin <- mapply (fun.fl, lf.list3, y=y)
      lf.list4  <- mapply(function(old, new, which) {
        old[,which] <- new
        old
      }, lf.list3, fl.fin, 'fl', SIMPLIFY = FALSE)
      
      #into one dataframe 
      df.lf <- ldply(lf.list4, data.frame)
      
      
      #merging other tables
      s.w.lf <- as.data.frame(cbind(df.lf, SW.w))
      
      
      #merging with static valuetable
      SW.static <- filter(st.table, st.table$class %in% SW.no)
      SW.static <- SW.static[,-c(1,9)]
      SW.st.table <- lapply(names(SW.static), function(x) table(SW.static[x]))
      SW.st.df <- as.data.frame(plyr::ldply(SW.st.table, rbind)) #getting value count
      row.names(SW.st.df) <- c('bd', 'br', 'cl', 'sa', 'si', 'dem', 'slope')
      SW.st.df$id <- c(1:length(row.names(SW.st.df)))
      SW.st.melt <- melt(SW.st.df, id.vars = 'id', variable.name = 'all') #in one long column
      SW.st.trans <- as.data.frame(t(SW.st.melt[,2:3])) #transpose
      colnames(SW.st.trans) <- SW.st.melt[,2]
      SW.st.trans <- SW.st.trans[-1,]
      SW.st.repli <- SW.st.trans[rep(rownames(SW.st.trans), times=length(row.names(w.table))), 1:length(SW.st.trans)] #replicate
      colnames(SW.st.repli) <- c('bd1', 'br1', 'cl1', 'sa1', 'si1', 'dem1', 'slope1', 'bd2', 'br2', 'cl2', 'sa2', 'si2', 'dem2', 'slope2', 'bd3', 'br3', 'cl3', 'sa3', 'si3', 'dem3', 'slope3', 'bd4', 'br4', 'cl4', 'sa4', 'si4', 'dem4', 'slope4', 'bd5', 'br5', 'cl5', 'sa5', 'si5', 'dem5', 'slope5')
      s.w.lf.st <-  cbind(s.w.lf, SW.st.repli)
      
      
      # Remove all NA column (if any) 
      for (col in names(s.w.lf.st)) {
        missing <- sum(is.na(s.w.lf.st[,col]))
        if (missing == 6940){
          s.w.lf.st[,col] <- 0}}
      s.w.lf.st$date <- seq(from = as.Date("1998-01-01"), to = as.Date("2016-12-31"), by = 'day')
      
      # Create weekly and monthly pcp
      w.avg <- apply.weekly(xts(s.w.lf.st[,-length(s.w.lf.st)], order.by= s.w.lf.st[,length(s.w.lf.st)]), FUN = mean)
      coi <- w.avg[,15]
      coi <-  coi[rep(seq_len(nrow(coi)), each=7),]
      coi <- as.data.frame(coi)
      coi <- coi[-c(1:4),]
      
      m.avg <- apply.monthly(xts(s.w.lf.st[,-length(s.w.lf.st)], order.by= s.w.lf.st[,length(s.w.lf.st)]), FUN = mean)
      m <- as.data.frame(m.avg[,15])
      freq <- monthDays(row.names(m))
      m <- cbind (m,freq)
      coi1 <- m[rep(row.names(m), m$freq), 1:2]
      
      w.max <- apply.weekly(xts(s.w.lf.st[[15]], order.by= s.w.lf.st[,52]), max)
      coi2 <-  w.max[rep(seq_len(nrow(w.max)), each=7),]
      coi2 <- as.data.frame(coi2)
      coi2 <- coi2[-c(1:4),]
      
      s.w.lf.st$weekpcp <- coi
      s.w.lf.st$mopcp <- coi1[[1]]
      s.w.lf.st$maxpcp <- coi2
      
      vt.names <- c('LC1.agr', 'LC2.for', 'LC3.gr', 'LC4.wgr', 'LC5.wetl', 
                    'LC6.riv', 'LC7.bu', 'LC8.barr', 'LC9.CN', 'LC10.man',
                    'LC11.fl', 'W1.ws', 'W2.tmin', 'W3.tmax', 'W4.pcp', 'W5.rh',
                    'P.bd1', 'P.br1', 'P.cl1', 'P.sa1', 'P.si1', 'P.dem1', 'P.sl1', 
                    'P.bd2', 'P.br2', 'P.cl2', 'P.sa2', 'P.si2', 'P.dem2', 'P.sl2',
                    'P.bd3', 'P.br3', 'P.cl3', 'P.sa3', 'P.si3', 'P.dem3', 'P.sl3',
                    'P.bd4', 'P.br4', 'P.cl4', 'P.sa4', 'P.si4', 'P.dem4', 'P.sl4', 
                    'P.bd5', 'P.br5', 'P.cl5', 'P.sa5', 'P.si5', 'P.dem5', 'P.sl5', 
                    'date', 'W6.pcpw', 'W7.pcpm', 'W8.pcpmx')
      
      colnames(s.w.lf.st) <- vt.names
      
      
      
      # Write valuetable
      
      #setwd('D:/THESIS_PP/final results1/')
      #subs <- as.name(SW)
      #fname <- sprintf('%s.csv',deparse(substitute(subs)))
      #write.csv(s.w.lf.st, file=fname)
      #setwd(mydir)
    }
    return(s.w.lf.st)
    
  }  
    

    
  
    
