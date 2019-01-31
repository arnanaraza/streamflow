## FUNCTION TO REGRESS STREAMFLOW TO PEAK FLOW INDICATOR  ## 


# Load individual raw tables
# per yearly avg
all.list <- c('aarb_a', 'aarb_n', 'abrb_s','arb_b', 'arb_c', 'crb_a', 'crb_be',  'crb_bu', 'crb_d', 'crb_j','crb_m', 'crb_p', 'crb_s',  'crb_t', 'crb_u', 'mrb_s', 'prb_b', 'prb_c', 'prb_a', 'prb_p', 'prb_r')
all.pp.table <- lapply(1:21, function (x) data.table(all.graphs.yes[[x]], key='predicted'))
all.pp.table <- lapply(1:21, function(x) cbind(all.pp.table[[x]], pp.ratio=all.pp.table[[x]]$pcp/all.pp.table[[x]]$predicted))
all.pp.year <- lapply(1:21, function(x) aggregate(all.pp.table[[x]]$pp.ratio ~ year(date) + forest, data=all.pp.table[[x]],FUN=sum))
colnames <- c('date','forest', 'pp.ratio')
all.pp.year <- lapply(all.pp.year, setNames, colnames)
sel.pp.year <- lapply(pp.list, function(x) all.pp.year[[x]])
ggplot(bind_rows(sel.pp.year, .id="df"), aes(date, pp.ratio, colour=df)) +
  geom_line()

# per 100 days
all.pp.table1 <- lapply(1:21, function(x) all.pp.table[[x]] = all.pp.table[[x]][2500:5500,])
all.pp.year1 <- lapply(1:21, function(x) aggregate(all.pp.table1[[x]]$pp.ratio ~ year(date) + forest, data=all.pp.table1[[x]],FUN=mean))
colnames <- c('date', 'forest', 'pp.ratio')
all.pp.year1 <- lapply(all.pp.year1, setNames, colnames)

pp.list <- c(1:21)
all.pp.year2 <- lapply(pp.list, function(x) all.pp.year1[[x]])
ggplot(bind_rows(all.pp.year2), aes(date, pp.ratio)) +
  geom_line() +
  xlab("year") +
  scale_y_continuous("peak flow - rainfall ratio") + 
  labs(title="peak flow-rainfall ratio and forest loss correlation, crb_p")
  

# Regress 
plotReg_PP <- function(df, SW) {
  my.formula.pcp  <-df$pp.ratio ~ df$forest
  plot <- ggplot(df,aes(x=pp.ratio,y=forest))+geom_point()+
    xlab("rainfall-peak flow ratio") +
    scale_y_continuous("forest loss (ha)") + 
    geom_smooth(method = "lm", se = FALSE) +
    labs(title=paste0('relation of rainfall-peakflow ratio to forest loss, ', SW)) +
    stat_poly_eq(formula = my.formula.pcp, 
                 eq.with.lhs = "italic(hat(y))~`=`~",
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE) 
  #plot(plot)
  }

sep.list <- c('aarb_n', 'crb_d', 'crb_p')

pp.all.plot <- lapply((1:length(pp.list)), function(x) plotReg_PP(sel.pp.year[[x]], all.list[[x]]))
pp.all.save <- multiplot(plotlist = pp.all.plot, cols = 3)

pp.plot <- lapply((1:length(all.list)), function(x) plotReg_PP(all.pp.year2[[x]], all.list[[x]]))
pp.save <- multiplot(plotlist = pp.plot, cols = 3)
