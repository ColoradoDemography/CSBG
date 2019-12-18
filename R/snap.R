#' snap generates tables and plotly charts for Supplemental Nutrition Assistance Program (SNAP)
#'   data from Hunger Free Colorado
#'  CSBG Dashboard 11/2019  A. Bickford
#' @param DBPool the DOLA database pool
#' @lvl the selected agency
#' @param listID is the list of selected county codes
#' @param cyrYr Current year value
#' @return plotly graphic, data table and data file
#' @export

snap <- function(DBPool,lvl,listID,curYR){

  # Collecting List of Counties
 
  ctyfips <- as.numeric(substr(listID$list1,3,5))
  
  
 
# Extracting SNAP data
  SNAPSQL <- "SELECT * FROM data.csbg_snap;"
 
  f.SNAP <- dbGetQuery(DBPool, SNAPSQL) %>% filter(fips %in% ctyfips  & year == curYR)
  
   f.SNAPctyVAL <- f.SNAP 
   
   f.SNAPctyVAL <- f.SNAPctyVAL[,c("fips", "county", "year", 
                                     "snappct", "snapelig", "snappart",
                                     "snapnonpart")] %>%
                  mutate(snappct = snappart/snapelig,
                       snapnonpct = snapnonpart/snapelig)
   f.SNAPctyVAL$county <- paste0(f.SNAPctyVAL$county," County")
   
   f.SNAPctyVAL <- f.SNAPctyVAL[,c("fips", "county", "year", 
                                     "snappct", "snapnonpct", "snapelig", "snappart",
                                     "snapnonpart")]
   
   f.SNAPctyVAL$county <- paste0(f.SNAPctyVAL$county," County")
   
   if(length(ctyfips) > 1){
     f.SNAPagyVAL <- f.SNAP %>%
       group_by(year) %>%
       summarize(snapelig = sum(snapelig),
                 snappart	= sum(snappart),
                 snapnonpart = sum(snapnonpart))  %>%
       mutate(snappct = snappart/snapelig,
              snapnonpct = snapnonpart/snapelig)
     
    f.SNAPagyVAL$fips <- 0
    f.SNAPagyVAL$county <- listID$plName1
    
    f.SNAPagyVAL <- f.SNAPagyVAL[,c("fips", "county", "year", 
                                     "snappct", "snapnonpct", "snapelig", "snappart",
                                   "snapnonpart")]
    
    f.SNAPctyVAL <- bind_rows(f.SNAPagyVAL, f.SNAPctyVAL)

   }

 
 f.SNAPctyVAL$county <- sub("County County","County",f.SNAPctyVAL$county)
# creating Plotly Chart

     f.SNAPcty_tot <- gather(f.SNAPctyVAL,SNAP,count,c(snappart,snapnonpart), factor_key=TRUE)
     f.SNAPcty_pct <- gather(f.SNAPctyVAL,SNAP,pct,c(snappct,snapnonpct), factor_key=TRUE)

    f.SNAPcty_tot$SNAP <- plyr::revalue(f.SNAPcty_tot$SNAP, c("snappart" = "Participating",
                                                              "snapnonpart" = "Not Participating"))
    f.SNAPcty_tot$SNAP <- factor(f.SNAPcty_tot$SNAP, levels = c("Participating",
                                                              "Not Participating"))

        
    f.SNAPcty_pct$SNAP <- plyr::revalue(f.SNAPcty_pct$SNAP, c("snappct" = "Participating",
                                                              "snapnonpct" = "Not Participating"))
    f.SNAPcty_pct$SNAP <- factor(f.SNAPcty_pct$SNAP, levels = c("Participating",
                                                              "Not Participating"))
    
    f.SNAPcty_PL <- inner_join(f.SNAPcty_pct,f.SNAPcty_tot[,c(1,7,8)], by=c("fips","SNAP"))
  
    f.SNAPcty_PL$indText  <- paste0( f.SNAPctyVAL$county," Year: ",f.SNAPcty_PL$year," Participation: ",f.SNAPcty_PL$SNAP,", Percent: ", percent(f.SNAPcty_PL$pct * 100)," Count: ",NumFmt(f.SNAPcty_PL$count))  
    grTitle <- paste0("Supplemental Nutrition Assistance Program (SNAP) Participation, ",listID$plName1," ",curYR)
    outCap <- captionSrc("SNAP","","")
    xAxis <- list(title = "Participation")
    yAxis <- list(title = 'Percent',tickformat = "%")

if(length(ctyfips) > 1 ){
SNAPPlot <- plot_ly(f.SNAPcty_PL, 
                   x = ~SNAP, 
                   y = ~pct, 
                   type = 'bar', text = ~indText, hoverinfo = 'text',
                   transforms = list(
                      list(
                        type = 'filter',
                        target = ~county,
                        operation = '=',
                        value = unique(f.SNAPcty_PL$county)[1]))) %>%
 layout( title=grTitle, yaxis = yAxis, xaxis=xAxis,
          showlegend = FALSE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCap,
                      font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4),
    updatemenus = list(
      list(
        type = 'dropdown',
        active = 0,
        buttons = list(
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.SNAPcty_PL$county)[1]),
                     label = unique(f.SNAPcty_PL$county)[1]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.SNAPcty_PL$county)[2]),
                     label = unique(f.SNAPcty_PL$county)[2]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.SNAPcty_PL$county)[3]),
                     label = unique(f.SNAPcty_PL$county)[3]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.SNAPcty_PL$county)[4]),
                     label = unique(f.SNAPcty_PL$county)[4]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.SNAPcty_PL$county)[5]),
                     label = unique(f.SNAPcty_PL$county)[5]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.SNAPcty_PL$county)[6]),
                     label = unique(f.SNAPcty_PL$county)[6]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.SNAPcty_PL$county)[7]),
                     label = unique(f.SNAPcty_PL$county)[7])
            )
        )))
} else {
   SNAPPlot <- plot_ly(f.SNAPcty_PL, 
                      x = ~SNAP, y = ~pct,  type = 'bar',
                      text = ~indText, hoverinfo = 'text') %>%
    layout( title=grTitle, yaxis = yAxis, xaxis=xAxis,
          showlegend = FALSE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCap,
                      font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4))
   
}
    
# Creating Table data file

    
    f.SNAP_POP <- f.SNAPctyVAL[,c(1,2,7,8,6)] 
    f.SNAP_POP[,3:5] <- sapply(f.SNAP_POP[,3:5],NumFmt) 
    
    f.SNAP_POP$type = "Count"
    f.SNAP_POP <- f.SNAP_POP[,c(1,2,6,3:5)]
   
    # Recoding
    names(f.SNAP_POP)<- c("fips","Agency/County", "Value", "Participating", "Not Participating", "Eligible")
    
    
    f.SNAP_PCT <- f.SNAPctyVAL[,c(1,2,4,5)] 
    f.SNAP_PCT$snaptotpct <- f.SNAP_PCT$snappct + f.SNAP_PCT$snapnonpct
    f.SNAP_PCT[,3:5] <- lapply(f.SNAP_PCT[,3:5], function(x) x * 100)
    f.SNAP_PCT[,3:5] <- sapply(f.SNAP_PCT[,3:5],percent) 
   
    f.SNAP_PCT$type = "Percentage"
    f.SNAP_PCT <- f.SNAP_PCT[,c(1,2,6,3:5)]
    # Recoding
    names(f.SNAP_PCT)<- c("fips","Agency/County", "Value", "Participating", "Not Participating", "Eligible")
    

    f.SNAPcty_tab <- bind_rows(f.SNAP_PCT, f.SNAP_POP) %>% arrange(fips,desc(Value))
   f.SNAPcty_tab <- f.SNAPcty_tab[,c(2:6)]

     #Clearing county
    if(length(ctyfips) == 1) {
      npanel1 <- 1
    } else {
      npanel1 = length(ctyfips) + 1
    }
    
    f.SNAPcty_tab <- clrGeoname(f.SNAPcty_tab,"Agency/County",npanel1,2)

    

 # Flex Table
  tab_head <- paste0("Supplemental Nutrition Assistance Program (SNAP) Participation, ",listID$plName1, " ",curYR)
  
  
  f.snapFlex <- flextable(
       f.SNAPcty_tab,
       col_keys = names(f.SNAPcty_tab)) %>%
       add_header_row(values=tab_head,top=TRUE,colwidths=5) %>%
       add_footer_row(values=outCap,top=FALSE,colwidths=5) 
      

  
    

  

  #bind list
  outList <- list("plot"= SNAPPlot, "data" = f.SNAPcty_PL, "table" = f.SNAPcty_tab, "FlexTable" = f.snapFlex,"caption" = outCap)
  
  return(outList)
}