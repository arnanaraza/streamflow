## FUNCTION TO REGRESS SEASONAL FLOW TO FOREST LOSS ## 



# Load individual raw tables
valid.reg <- read.csv('D:/THESIS_PP/valid.reg.csv')
valid.reg.wet <- read.csv('D:/THESIS_PP/valid.reg.wet.csv')
all.reg <- read.csv('D:/THESIS_PP/dry_pcp_floss_REG_3month.csv')
all.reg.wet <- read.csv('D:/THESIS_PP/wet_pcp_floss_REG_3moPCP.csv')

  
valid.out <- split(valid.reg , f = valid.reg$SW)
valid.list <- names(valid.out)

valid.out.wet <- split(valid.reg.wet , f = valid.reg.wet$SW)
valid.list.wet <- names(valid.out)

all.out <- split(all.reg , f = all.reg$SW)
all.list <- names(all.out)

all.out.wet <- split(all.reg.wet , f = all.reg.wet$SW)
all.list.wet <- names(all.out.wet)

nodams.list <-  c('aarb_a', 'aarb_n', 'abrb_s', 'arb_b', 'crb_d', 'crb_j', 'crb_p', 'mrb_s', 'prb_b', 'prb_c', 'prb_r')### crb_d, crb_j, crb_p (Upstream sheds no dams)
nodams.out <- lapply(1:length(nodams.list), function (x) filter(all.reg, SW == nodams.list[[x]]))
dams.list <- c('arb_c', 'crb_a', 'crb_be', 'crb_bu', 'crb_m', 'crb_s', 'crb_t', 'crb_u', 'prb_a', 'prb_p' )## crb_bu (largest), prb_a
dams.out <- lapply(1:length(dams.list), function (x) filter(all.reg, SW == dams.list[[x]]))

disp.list <- c('arb_b', 'crb_d', 'crb_m', 'crb_be', 'prb_c', 'prb_r')# with dam - crb_m, crb_be
disp.out <- lapply(1:length(disp.list), function (x) filter(all.reg, SW == disp.list[[x]]))


# Regress 
plotReg_floss <- function(reg.table, SW) {
  
my.formula  <-reg.table$wet.flow ~ reg.table$f.loss
plot1 <- ggplot(reg.table,aes(x=wet.flow,y=f.loss))+geom_point()+
  xlab("wet season streamflow (l/sec)") +
  scale_y_continuous("forest loss (ha)") + 
  labs(title=as.character(SW)) +
  geom_smooth(method = "lm", se = FALSE) +
  stat_poly_eq(formula = my.formula, 
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) 

return(plot1)
}

# Plot all
all.plot <- lapply((1:length(all.out)), function(x) plotReg_floss(all.out[[x]], all.list[[x]]))
all.save <- multiplot(plotlist = all.plot, cols = 3)
ggsave(plot=all.save, filename=paste0('D:/THESIS_PP/final results pcp/',  Sys.Date(), 'all_floss_dryflow_ts.png'), device='png', dpi=100, width = 11, height = 8, units='in')

all.plot.pcp.wet <- lapply((1:length(all.out.wet)), function(x) plotReg_floss(all.out.wet[[x]], all.list.wet[[x]]))
all.save.pcp.wet <- multiplot(plotlist = all.plot.pcp.wet, cols = 3)



  # Plot per interest

# with and without regulating structures
dam.plot <- lapply((1:length(dams.out)), function(x) plotReg_floss(dams.out[[x]], dams.list[[x]]))
dam.save <- multiplot(plotlist = dam.plot, cols = 3)

nodam.plot <- lapply((1:length(nodams.out)), function(x) plotReg_floss(nodams.out[[x]], nodams.list[[x]]))
nodam.save <- multiplot(plotlist = nodam.plot, cols = 3)

# selection 
disp.plot <- lapply((1:length(disp.out)), function(x) plotReg_floss(disp.out[[x]], disp.list[[x]]))
disp.save <- multiplot(plotlist = disp.plot, cols = 2)

# validation
valid.plot <- lapply((1:length(valid.out)), function(x) plotReg_floss(valid.out[[x]], valid.list[[x]]))
valid.save <- multiplot(plotlist = valid.plot, cols = 3)

valid.plot.wet <- lapply((1:length(valid.out.wet)), function(x) plotReg_floss(valid.out.wet[[x]], valid.list.wet[[x]]))
valid.save.wet <- multiplot(plotlist = valid.plot.wet, cols = 3)


my.formula  <-all.reg.wet$wet.flow ~ all.reg.wet$f.loss
plot1 <- ggplot(all.reg.wet,aes(x=wet.flow,y=f.loss))+geom_point()+
  xlab("wet season streamflow (l/sec)") +
  scale_y_continuous("forest loss (ha)") + 
  labs(title='mean daily wet season streamflow and forest loss, 21 subwatersheds') +
  geom_smooth(method = "lm", se = FALSE) +
  stat_poly_eq(formula = my.formula, 
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) 
plot(plot1)

my.formula  <-valid.reg.wet$wet.flow ~ valid.reg.wet$f.loss
plot2 <- ggplot(valid.reg.wet,aes(x=wet.flow,y=f.loss))+geom_point()+
  xlab("wet season streamflow (l/sec)") +
  scale_y_continuous("forest loss (ha)") + 
  labs(title='mean daily wet season streamflow and forest loss, 6 validation subwatersheds') +
  geom_smooth(method = "lm", se = FALSE) +
  stat_poly_eq(formula = my.formula, 
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) 
plot(plot2)
