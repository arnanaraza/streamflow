### FUNCTION TO CREATE HYDROGRAPHS FOR 6 VALIDATION SITES RETURNING PRECIPITATION, FOREST LOSS, AND STREAMFLOW TABLES### 

# Preliminearies
pacman::p_load(deldir, dplyr, highcharter, webshot)
mydir <- setwd('D:/THESIS_PP')

# Function that takes SW valuetable, SW name and predicted streamflow as arguments

plotGraphs1 <- function(VT, pred, SW) {
  df.name <- deparse(substitute(VT))
  pcp.flow <- cbind(pred, VT[[15]]) 
  pcp.flow <- cbind(pcp.flow, VT[[11]])
  colnames(pcp.flow) <- c('predicted', 'date', 'pcp', 'forest')
  
  hc <- highchart() %>% 
    hc_title(text = paste0('rainfall and streamflow time series for ',as.character(SW)),size=0.5)%>% 
    hc_yAxis_multiples(list(title = list(text = "rainfall depth (mm)"), reversed = TRUE), 
                       list(title = list(text = "flow (liters/s)"), 
                            opposite = TRUE)) %>% 
    hc_add_series(data = pcp.flow$pcp,type='column', name='rainfall') %>% 
    hc_add_series(data = pcp.flow$predicted, type = "spline", yAxis = 1, lineWidth=0.1, name='streamflow') %>% 
    hc_xAxis(categories = as.Date(pcp.flow$date), title = list(text = "date"))
  hc
  png(filename=(paste0('D:/THESIS_PP/final results valid/',Sys.Date(), '_', SW,'_hydrograph', '.png')), 
      width=1000,height=500,res=100)

  hc1 <- highchart() %>% 
    hc_title(text = paste0('forest loss and streamflow time series for ',as.character(SW)), size=0.5 )%>% 
    hc_yAxis_multiples(list(title = list(text = "forest cover (ha)"), reversed = TRUE), 
                       list(title = list(text = "flow (liters/s)"), 
                            opposite = TRUE)) %>% 
    hc_add_series(data = pcp.flow$forest, type = "column", name='forest loss') %>%
    hc_add_series(data = pcp.flow$predicted, type = "spline", yAxis = 1, lineWidth=0.1, name='streamflow') %>% 
    hc_xAxis(categories =as.Date(pcp.flow$date), title = list(text = "date"))
  hc1
  png(filename=paste0('D:/THESIS_PP/final results valid/', Sys.Date(), '_', SW,'_flow-loss', '.png'), 
        width=1000,height=500,res=100)

  return(pcp.flow)
}



