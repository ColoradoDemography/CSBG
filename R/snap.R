#' snap generates tables and plotly charts for Supplemental Nutrition Assistance Program (SNAP)/Food Stamps
#'   data from Hunger Free Colorado
#'  CSBG Dashboard 11/2019 REVISED  9/2022 A. Bickford
#' @param DBPool the DOLA database pool
#' @param lvl the selected agency
#' @param listID is the list of selected county codes
#' @param cyrYr Current year value
#' @return plotly graphic, data table and data file
#' @export

snap <- function(DBPool,lvl,ACS,listID,curYR){

  # Collecting List of Counties

  ctyfips <- listID$list1
  
# Extracting SNAP data
  
  f.SNAPctyVAL <- get_acs(geography = "county", table="S2201", state = "CO", year = curYr,
                    key=censAPI, output="wide", survey = "acs5") %>%
    filter(GEOID %in% ctyfips) %>%
    arrange(GEOID) %>%
    mutate(NAME = str_replace(NAME,", Colorado",""),
           TOT_POV_EST = S2201_C01_021E,
           TOT_POV_MOE = S2201_C01_021M,
           TOT_SNAP_EST = S2201_C03_001E,
           TOT_SNAP_MOE = S2201_C03_001M,
           SNAP_POV_EST = S2201_C03_021E,
           SNAP_POV_MOE = S2201_C03_021M,
           NSNAP_POV_EST = TOT_POV_EST - SNAP_POV_EST,
           NSNAP_POV_MOE = sqrt(TOT_POV_MOE^2 - SNAP_POV_MOE^2),
           SNAP_EST_PCT = SNAP_POV_EST/TOT_POV_EST,
           SNAP_MOE_PCT = pctMOE(TOT_POV_EST, TOT_POV_MOE, SNAP_POV_MOE,SNAP_EST_PCT),
           NSNAP_EST_PCT = NSNAP_POV_EST/TOT_POV_EST,
           NSNAP_MOE_PCT = pctMOE(TOT_POV_EST, TOT_POV_MOE, NSNAP_POV_MOE,NSNAP_EST_PCT),
           SNAP_POV_EST_PCT = SNAP_POV_EST/TOT_SNAP_EST,
           SNAP_POV_MOE_PCT = pctMOE(TOT_SNAP_EST, TOT_SNAP_MOE, SNAP_POV_MOE,SNAP_POV_EST_PCT)
    ) %>%
    select(GEOID, NAME, TOT_POV_EST : SNAP_POV_MOE_PCT)
  
   
   
   if(length(ctyfips) > 1){
     f.SNAPagyVAL <- f.SNAPctyVAL %>%
       summarize(
         TOT_POV_EST = sum(TOT_POV_EST),
         TOT_POV_MOE = sum(TOT_POV_MOE^2),
         TOT_SNAP_EST = sum(TOT_SNAP_EST),
         TOT_SNAP_MOE = sum(TOT_SNAP_MOE^2),
         SNAP_POV_EST = sum(SNAP_POV_EST),
         SNAP_POV_MOE = sum(SNAP_POV_MOE^2),
         NSNAP_POV_EST = sum(NSNAP_POV_EST),
         NSNAP_POV_MOE = sum(NSNAP_POV_MOE^2)
       )  %>%
       mutate(TOT_POV_MOE = sqrt(TOT_POV_MOE),
              TOT_SNAP_MOE = sqrt(TOT_SNAP_MOE),
              SNAP_POV_MOE = sqrt(SNAP_POV_MOE),
              NSNAP_POV_MOE = sqrt(NSNAP_POV_MOE),
              SNAP_EST_PCT = SNAP_POV_EST/TOT_POV_EST,
              SNAP_MOE_PCT = pctMOE(TOT_POV_EST, TOT_POV_MOE, SNAP_POV_MOE,SNAP_EST_PCT),
              NSNAP_EST_PCT = NSNAP_POV_EST/TOT_POV_EST,
              NSNAP_MOE_PCT = pctMOE(TOT_POV_EST, TOT_POV_MOE, NSNAP_POV_MOE,NSNAP_EST_PCT),
              SNAP_POV_EST_PCT = SNAP_POV_EST/TOT_SNAP_EST,
              SNAP_POV_MOE_PCT = pctMOE(TOT_SNAP_EST, TOT_SNAP_MOE, SNAP_POV_MOE,SNAP_POV_EST_PCT)
       )
     
    f.SNAPagyVAL <- f.SNAPagyVAL %>%
        mutate(GEOID = "08000",
               NAME = listID$plName1 ) %>%
        select(GEOID, NAME, TOT_POV_EST : SNAP_POV_MOE_PCT)
    f.SNAPctyVAL <- bind_rows(f.SNAPagyVAL, f.SNAPctyVAL)
   }

# creating Plotly Chart

     f.SNAPcty_pct <- f.SNAPctyVAL %>%
       select(GEOID,	NAME, SNAP_POV_EST_PCT, SNAP_EST_PCT, NSNAP_EST_PCT) %>%
       gather(SNAP,pct,SNAP_POV_EST_PCT : NSNAP_EST_PCT, factor_key=TRUE)

    f.SNAPcty_pct$SNAP <- plyr::revalue(f.SNAPcty_pct$SNAP, c("SNAP_POV_EST_PCT" = "Households\nReceiving SNAP Benefits\nBelow Poverty Level",
                                                              "SNAP_EST_PCT" = "Households\nBelow Poverty Level\nReceiving SNAP Benefits",
                                                              "NSNAP_EST_PCT" = "Households\nBelow Poverty Level\nNot Receiving SNAP Benefits"))
    f.SNAPcty_pct$SNAP <- factor(f.SNAPcty_pct$SNAP, levels = c("Households\nReceiving SNAP Benefits\nBelow Poverty Level",
                                                                "Households\nBelow Poverty Level\nReceiving SNAP Benefits",
                                                                "Households\nBelow Poverty Level\nNot Receiving SNAP Benefits"))
    f.SNAPcty_PL <- f.SNAPcty_pct
  
    f.SNAPcty_PL$indText  <- paste0("Participation: ",f.SNAPcty_PL$SNAP,", Percent: ", percent(f.SNAPcty_PL$pct * 100))  
    grTitle <- paste0("Supplemental Nutrition Assistance Program (SNAP) Participation, ",listID$plName1)
    outCap <- captionSrc("ACS",ACS,"S2201")
    xAxis <- list(title='SNAP Participation')
    yAxis <- list(title = 'Percent',tickformat = ".1%")
    txtNames <- unique(f.SNAPcty_PL$county)

if(length(ctyfips) > 1 ){
SNAPPlot <- plot_ly(f.SNAPcty_PL, 
                   x = ~SNAP, 
                   y = ~pct, 
                   type = 'bar', text = ~indText,  textposition = "none",hoverinfo = 'text',
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
        buttons = genDropdown(txtNames)
        )))
} else {
   SNAPPlot <- plot_ly(f.SNAPcty_PL, 
                      x = ~SNAP, y = ~pct,  type = 'bar',
                      text = ~indText,  textposition = "none",hoverinfo = 'text') %>%
    layout(  yaxis = yAxis, xaxis=xAxis, title=grTitle,
          showlegend = FALSE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCap,
                      font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4))
   
}
    
# Creating Table data file

   
    f.SNAP_POP <- f.SNAPctyVAL[,c(1:10)] 
    f.SNAP_POP[,3:10] <- lapply(f.SNAP_POP[,3:10],NumFmt) 
    
    f.SNAP_POP$type = "Count"
    f.SNAP_POP <- f.SNAP_POP[,c(1,2,11,3:10)]
    
    names(f.SNAP_POP)[1:3]<- c("fips", "Agency/ County","Value")
   
    f.SNAP_PCT <- f.SNAPctyVAL[,c(1,2,11:16)] 
    f.SNAP_PCT[,3:8] <- lapply(f.SNAP_PCT[,3:8], function(x) percent(x * 100))
  
   
    f.SNAP_PCT$type = "Percentage"
    f.SNAP_PCT$TOT_POV_EST = ""
    f.SNAP_PCT$TOT_POV_MOE = ""

    f.SNAP_PCT <- f.SNAP_PCT[,c(1,2,9:11,7,8,3:6)]
    # Recoding
    names(f.SNAP_PCT)<- names(f.SNAP_POP)
    

    f.SNAPcty_tab <- bind_rows(f.SNAP_PCT, f.SNAP_POP) %>% arrange(fips,Value)
    
     #Clearing county
    if(length(ctyfips) == 1) {
      npanel1 <- 1
    } else {
      npanel1 = length(ctyfips) + 1
    }
    
    f.SNAPcty_tab <- clrGeoname(f.SNAPcty_tab,"Agency/ County",npanel1,2)

    

 # Flex Table
  tab_head <- paste0("Supplemental Nutrition Assistance Program (SNAP) Participation, ",listID$plName1, " ",curYR)
  col_header <- c("", "","","Households in Poverty", "Households Receiving SNAP Benefits",
                  "Households in Poverty Receiving SNAP Benefits", "Households in Poverty Not Receiving SNAP Benefits")

  f.snapFlex <- flextable(
       f.SNAPcty_tab,
       col_keys = names(f.SNAPcty_tab)) %>%
       fontsize(size=9, part='all') %>%
       add_header_row(value=col_header,colwidths= c(1,1,1,2,2,2,2)) %>%
       add_header_row(values=tab_head,colwidths=11) %>%
       set_header_labels(TOT_POV_EST = 'Estimate',
                         TOT_POV_MOE = 'Margin of Error',
                         TOT_SNAP_EST = 'Estimate',
                         TOT_SNAP_MOE = 'Margin of Error',
                         SNAP_POV_EST = 'Estimate',
                         SNAP_POV_MOE = 'Margin of Error',
                         NSNAP_POV_EST = 'Estimate',
                         NSNAP_POV_MOE = 'Margin of Error') %>%
       add_footer_row(values=outCap,top=FALSE,colwidths=11) %>%
       align(i=2:3, j=1:11, align="center", part="header") %>%
       align(j=1:3, align="left", part="body") %>%
       align(j=4:11, align="right", part="body") %>%
       width(j= 1, width=.9) %>%
       width(j=2:11,width=.9) %>%
       height(part="footer", height=0.4) %>%
       height(part="header",i=2,height=0.6) 
    

  names(f.SNAPcty_tab) <- c("fips", "Agency/ County","Value",
                            "Households in Poverty Estmate", "Households in Poverty MOE", 
                            "Households Receiving SNAP Benefits Estimate", "Households Receiving SNAP Benefits MOE",
                            "Households in Poverty Receiving SNAP Benefits Estimate", "Households in Poverty Receiving SNAP Benefits MOE", 
                            "Households in Poverty Not Receiving SNAP Benefits Estimate", "Households in Poverty Not Receiving SNAP Benefits MOE" )
  #bind list
  outList <- list("plot"= SNAPPlot, "data" = f.SNAPcty_PL, "table" = f.SNAPcty_tab, "FlexTable" = f.snapFlex,"caption" = outCap)
  
  return(outList)
}