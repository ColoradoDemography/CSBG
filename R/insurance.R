#' insurance generates tables and plotly charts for Women's Infants and Children (ins) 
#'   data from Colorado Health Institute
#'  CSBG Dashboard 11/2019  A. Bickford
#' @param DBPool the DOLA database pool
#' @lvl the selected agency
#' @param listID is the list of selected county codes
#' @param cyrYr Current year value
#' @return plotly graphic, data table and data file
#' @export

insurance <- function(DBPool,lvl,listID,curYR){

  # Collecting List of Counties

  ctyfips <- as.numeric(substr(listID$list1,3,5))

# Extracting ins data
  insSQL <- "SELECT * FROM data.csbg_insurance;"
  f.ins <- dbGetQuery(DBPool, insSQL) %>% filter(fips %in% ctyfips)
  
   f.insctyVAL <- f.ins 
   
   f.insctyVAL <- f.insctyVAL[, c("fips", "county", "dirinsurance_n", "empinsurance_n", "medicaid_n", 
                                  "noinsurance_n", "schinsurance_n", "population_n", "dirinsurance_p", 
                                   "empinsurance_p", "medicaid_p", "noinsurance_p", "schinsurance_p")]
   
   f.insctyVAL$county <- paste0(f.insctyVAL$county," County")
   
   if(length(ctyfips) > 1){
     f.insagyVAL <- f.ins %>%
      summarize(dirinsurance_n	 = sum(dirinsurance_n),
                empinsurance_n	 = sum(empinsurance_n),
                medicaid_n	 = sum(medicaid_n),
                noinsurance_n	 = sum(noinsurance_n),
                 schinsurance_n	 = sum( schinsurance_n),
                population_n	 = sum(population_n)
                )  %>%
      mutate(dirinsurance_p	= dirinsurance_n/population_n,
              empinsurance_p	= empinsurance_n/population_n,
              medicaid_p	= medicaid_n/population_n,
              noinsurance_p	= noinsurance_n/population_n,
              schinsurance_p	= schinsurance_n/population_n)
     
    f.insagyVAL$fips <- 0
    f.insagyVAL$county <- listID$plName1
    
    f.insagyVAL <- f.insagyVAL[,c("fips", "county", "dirinsurance_n", "empinsurance_n", "medicaid_n", 
                                  "noinsurance_n", "schinsurance_n", "population_n", "dirinsurance_p", 
                                   "empinsurance_p", "medicaid_p", "noinsurance_p", "schinsurance_p")]  
    
    f.insctyVAL <- bind_rows(f.insagyVAL, f.insctyVAL)

   }

    
# creating Plotly Chart
    f.inscty_PL <-   gather(f.insctyVAL,ins,value,dirinsurance_p:schinsurance_p, factor_key=TRUE)
    f.inscty_PL$ins <- plyr::revalue(f.inscty_PL$ins, c("dirinsurance_p"	 = "Uninsured",
                                                       "empinsurance_p"	 =   "Employer-sponsored",
                                                        "medicaid_p"	 =  "Medicaid",
                                                        "noinsurance_p"	 = "Individually purchased",
                                                        "schinsurance_p"	 =  "Child Health Plan Plus"
))
    f.inscty_PL$ins <- factor(f.inscty_PL$ins, levels = c( "Individually purchased",
                                                           "Employer-sponsored", 
                                                           "Medicaid",
                                                          "Uninsured",
                                                          "Child Health Plan Plus"))
   
    
    f.inscty_PL$indText  <- paste0( f.insctyVAL$county," Health Insurance: ",f.inscty_PL$ins,", Percent: ", percent(f.inscty_PL$value * 100))  
    grTitle <- paste0("Health Insturance by Source, ",listID$plName1," ",curYR)
    outCap <- captionSrc("INS","","")
    xAxis <- list(title = "Source of Insurance")
    yAxis <- list(title = 'Percent',tickformat = "%")


if(length(ctyfips) > 1 ){
insPlot <- plot_ly(f.inscty_PL, 
                   x = ~ins, 
                   y = ~value, 
                   type = 'bar', text = ~indText, hoverinfo = 'text',
                   transforms = list(
                      list(
                        type = 'filter',
                        target = ~county,
                        operation = '=',
                        value = unique(f.inscty_PL$county)[1]))) %>%
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
                     args = list("transforms[0].value", unique(f.inscty_PL$county)[1]),
                     label = unique(f.inscty_PL$county)[1]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.inscty_PL$county)[2]),
                     label = unique(f.inscty_PL$county)[2]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.inscty_PL$county)[3]),
                     label = unique(f.inscty_PL$county)[3]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.inscty_PL$county)[4]),
                     label = unique(f.inscty_PL$county)[4]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.inscty_PL$county)[5]),
                     label = unique(f.inscty_PL$county)[5]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.inscty_PL$county)[6]),
                     label = unique(f.inscty_PL$county)[6]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.inscty_PL$county)[7]),
                     label = unique(f.inscty_PL$county)[7])
            )
        )))
} else {
   insPlot <- plot_ly(f.inscty_PL, 
                      x = ~ins, y = ~value,  type = 'bar',
                      text = ~indText, hoverinfo = 'text') %>%
    layout( title=grTitle, yaxis = yAxis, xaxis=xAxis,
          showlegend = FALSE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCap,
                      font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4))
   
}
    
# Creating Table data file

    f.ins_POP <- f.insctyVAL[,c(1:8)] 
    f.ins_POP[,3:8] <- sapply(f.ins_POP[,3:8],NumFmt) 
    f.ins_POP$type = "Count"
    f.ins_POP <- f.ins_POP[,c(1,2,9,3:7)]
    
        # Recoding
    names(f.ins_POP)<- c("fips","county", "Value", "dirinsurance", "empinsurance", "medicaid", 
                                  "noinsurance", "schinsurance")

    f.ins_PCT <- f.insctyVAL[,c(1,2,9:13)] 
    f.ins_PCT[,3:7] <- lapply(f.ins_PCT[,3:7], function(x) x * 100)
    f.ins_PCT[,3:7] <- sapply(f.ins_PCT[,3:7],percent) 
    
    f.ins_PCT$type = "Percentage"
     f.ins_PCT <- f.ins_PCT[,c(1,2,8,3:7)]
    
        # Recoding
     names(f.ins_PCT)<- c("fips","county", "Value", "dirinsurance", "empinsurance", "medicaid", 
                                  "noinsurance", "schinsurance")

    

   f.inscty_tab <- bind_rows(f.ins_PCT, f.ins_POP) %>% arrange(fips,desc(Value))
   f.inscty_tab <- f.inscty_tab[,c(2:8)]

     #Clearing county
    if(length(ctyfips) == 1) {
      npanel1 <- 1
    } else {
      npanel1 = length(ctyfips) + 1
    }
    
    f.inscty_tab <- clrGeoname(f.inscty_tab,"county",npanel1,2)

    names(f.inscty_tab) <- c("Agency/County","Value", "Individually purchased",
                             "Employer-sponsored", "Medicaid",
                             "Uninsured", "Child Health Plan Plus") 

 # Flex Table
  tab_head <- paste0("Health Insturance by Source, ",listID$plName1, " ",curYR)
  
  
  f.insFlex <- flextable(
       f.inscty_tab,
       col_keys = names(f.inscty_tab)) %>%
       add_header_row(values=tab_head,top=TRUE,colwidths=7) %>%
       add_footer_row(values=outCap,top=FALSE,colwidths=7) 


  #bind list
  outList <- list("plot"= insPlot, "data" = f.inscty_tab, "FlexTable" = f.insFlex,"caption" = outCap)
  
  return(outList)
}