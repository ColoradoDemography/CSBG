#' wic generates tables and plotly charts for Women's Infants and Children (WIC) 
#'   data from Kids County
#'  CSBG Dashboard 11/2019  A. Bickford
#' @param DBPool the DOLA database pool
#' @lvl the selected agency
#' @param listID is the list of selected county codes
#' @param cyrYr Current year value
#' @return plotly graphic, data table and data file
#' @export

wic <- function(DBPool,lvl,listID,curYR){

  # Collecting List of Counties
 
  ctyfips <- as.numeric(substr(listID$list1,3,5))
  
  
 
# Extracting WIC data
  WICSQL <- "SELECT * FROM data.csbg_wic;"
  f.WIC <- dbGetQuery(DBPool, WICSQL) %>% filter(fips %in% ctyfips  & year == curYR)
  
   f.WICctyVAL <- f.WIC 
   
   f.WICctyVAL <- f.WICctyVAL[,c("fips", "county", "wicpart","wicelig")]
   f.WICctyVAL <- f.WICctyVAL %>% mutate(wicnonpart = wicelig - wicpart,
                         wicpartpct = wicpart/wicelig,
                         wicnonpartpct = wicnonpart/wicelig,
                         wiceligpct = 1)
  
   f.WICctyVAL <- f.WICctyVAL[,c("fips", "county", "wicpart","wicnonpart", "wicelig",
                                 "wicpartpct","wicnonpartpct","wiceligpct")] 
   
   f.WICctyVAL$county <- paste0(f.WICctyVAL$county," County")
   
   if(length(ctyfips) > 1){
     f.WICagyVAL <- f.WIC %>%
      summarize(wicpart = sum(wicpart),
                 wicelig	= sum(wicelig))  %>%
      mutate(wicnonpart = wicelig - wicpart,
             wicpartpct = wicpart/wicelig,
             wicnonpartpct = wicnonpart/wicelig,
             wiceligpct = 1)
     
    f.WICagyVAL$fips <- 0
    f.WICagyVAL$county <- listID$plName1
    
    f.WICagyVAL <- f.WICagyVAL[,c("fips", "county", "wicpart","wicnonpart", "wicelig",
                                 "wicpartpct","wicnonpartpct","wiceligpct")]  
    
    f.WICctyVAL <- bind_rows(f.WICagyVAL, f.WICctyVAL)

   }

    
# creating Plotly Chart
    f.WICcty_PL <-   gather(f.WICctyVAL,WIC,value,c(wicpartpct,wicnonpartpct), factor_key=TRUE)
    f.WICcty_PL$WIC <- plyr::revalue(f.WICcty_PL$WIC, c("wicpartpct" = "Participating",
                                                        "wicnonpartpct" = "Not Participating"))
    f.WICcty_PL$WIC <- factor(f.WICcty_PL$WIC, levels = c("Participating",
                                                              "Not Participating"))
   
    
    f.WICcty_PL$indText  <- paste0( f.WICctyVAL$county," Participation: ",f.WICcty_PL$WIC,", Percent: ", percent(f.WICcty_PL$value * 100))  
    grTitle <- paste0("Women, Infants and Children (WIC) Participation, ",listID$plName1," ",curYR)
    outCap <- captionSrc("WIC","","")
    xAxis <- list(title = "Participation")
    yAxis <- list(title = 'Percent',tickformat = "%")


if(length(ctyfips) > 1 ){
WICPlot <- plot_ly(f.WICcty_PL, 
                   x = ~WIC, 
                   y = ~value, 
                   type = 'bar', text = ~indText, hoverinfo = 'text',
                   transforms = list(
                      list(
                        type = 'filter',
                        target = ~county,
                        operation = '=',
                        value = unique(f.WICcty_PL$county)[1]))) %>%
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
                     args = list("transforms[0].value", unique(f.WICcty_PL$county)[1]),
                     label = unique(f.WICcty_PL$county)[1]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.WICcty_PL$county)[2]),
                     label = unique(f.WICcty_PL$county)[2]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.WICcty_PL$county)[3]),
                     label = unique(f.WICcty_PL$county)[3]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.WICcty_PL$county)[4]),
                     label = unique(f.WICcty_PL$county)[4]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.WICcty_PL$county)[5]),
                     label = unique(f.WICcty_PL$county)[5]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.WICcty_PL$county)[6]),
                     label = unique(f.WICcty_PL$county)[6]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.WICcty_PL$county)[7]),
                     label = unique(f.WICcty_PL$county)[7])
            )
        )))
} else {
   WICPlot <- plot_ly(f.WICcty_PL, 
                      x = ~WIC, y = ~value,  type = 'bar',
                      text = ~indText, hoverinfo = 'text') %>%
    layout( title=grTitle, yaxis = yAxis, xaxis=xAxis,
          showlegend = FALSE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCap,
                      font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4))
   
}
    
# Creating Table data file

    f.WIC_POP <- f.WICctyVAL[,c(1:5)] 
    f.WIC_POP[,3:5] <- sapply(f.WIC_POP[,3:5],NumFmt) 
    f.WIC_POP$type = "Count"
    f.WIC_POP <- f.WIC_POP[,c(1,2,6,3:5)]
    
        # Recoding
    names(f.WIC_POP)<- c("fips","Agency/County", "Value", "Participating", "Not Participating", "Eligible")

    
    f.WIC_PCT <- f.WICctyVAL[,c(1,2,6:8)] 
    f.WIC_PCT[,3:5] <- lapply(f.WIC_PCT[,3:5], function(x) x * 100)
    f.WIC_PCT[,3:5] <- sapply(f.WIC_PCT[,3:5],percent) 
    
    f.WIC_PCT$type = "Percentage"
     f.WIC_PCT <- f.WIC_PCT[,c(1,2,6,3:5)]
    
        # Recoding
    names(f.WIC_PCT)<- c("fips","Agency/County", "Value", "Participating", "Not Participating", "Eligible")

    

   f.WICcty_tab <- bind_rows(f.WIC_PCT, f.WIC_POP) %>% arrange(fips,desc(Value))
   f.WICcty_tab <- f.WICcty_tab[,c(2:6)]

     #Clearing county
    if(length(ctyfips) == 1) {
      npanel1 <- 1
    } else {
      npanel1 = length(ctyfips) + 1
    }
    
    f.WICcty_tab <- clrGeoname(f.WICcty_tab,"Agency/County",npanel1,2)

    

 # Flex Table
  tab_head <- paste0("Women, Infants and Children (WIC) Participation, ",listID$plName1, " ",curYR)
  
  
  f.WICFlex <- flextable(
       f.WICcty_tab,
       col_keys = names(f.WICcty_tab)) %>%
       add_header_row(values=tab_head,top=TRUE,colwidths=5) %>%
       add_footer_row(values=outCap,top=FALSE,colwidths=5) 


  #bind list
  outList <- list("plot"= WICPlot, "data" = f.WICcty_tab, "FlexTable" = f.WICFlex,"caption" = outCap)
  
  return(outList)
}