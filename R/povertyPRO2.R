#' povertyPRO2 generaes tables and ployly charts from SAIPE county data for the current year
#'  CSBG Dashboard 11/2019  A. Bickford
#'  Pulling data from SAIPE API  
#' @param DBPool the DOLA database pool
#' @lvl the selected agency
#' @param listID is the list of selected county codes
#' @param ACS is the current ACS data file
#' @param cyrYr Current year value
#' @return plotly graphic, data table and data file
#' @export

povertyPRO2 <- function(lvl,listID,ACS,curYr,censKey){

  # Collecting List of Counties
 
  ctyfips <- as.numeric(substr(listID$list1,3,5))
  
# Building the data files  

# Extracting SAIPE data from the saipe API, Gathering all county records
  saipeAPI::set_api_key(censKey)
f.saiperaw <- saipeAPI::saipe_county(year = curYr, var = c("STABREV", "YEAR", "GEOID", "NAME", 
                                                           "SAEPOV0_17_PT", "SAEPOV5_17R_PT", "SAEPOVALL_PT",
                                                           "SAEPOVU_0_17", "SAEPOVU_5_17R", "SAEPOVU_ALL"))


f.saipe <- f.saiperaw %>% 
        filter(GEOID %in% listID$list1) %>%
        mutate(fips = as.numeric(county),
               geoname = NAME,
               year = YEAR,
               totpop = SAEPOVU_ALL,
               pople17 = SAEPOVU_0_17,
               pop0517 = SAEPOVU_5_17R,
               povpop  = SAEPOVALL_PT,
               povle17 = SAEPOV0_17_PT,
               pov0517 = SAEPOV5_17R_PT
               )

# Generating output data file 
   f.saipectyVAL <- f.saipe %>%
       mutate(popgt17 = totpop - pople17,
              povgt17 = povpop - povle17,
              povpct0517 = pov0517/pop0517,
              povpctgt17 = povgt17/popgt17,
              povpcttot = povpop/totpop)
   
   f.saipectyVAL <- f.saipectyVAL[,c("fips", "geoname", "year", 
                                     "pop0517","popgt17", "totpop",
                                     "pov0517", "povgt17", "povpop",
                                     "povpct0517", "povpctgt17","povpcttot")]
   
   if(length(ctyfips) > 1){
     f.saipeagyVAL <- f.saipe %>%
       group_by(year) %>%
       summarize(totpop = sum(totpop),
                 povpop	= sum(povpop),
                 pople17 = sum(pople17),
                 povle17 = sum(povle17),
                 pop0517 = sum(pop0517),
                 pov0517 = sum(pov0517))  %>%
       mutate(popgt17 = totpop - pople17,
              povgt17 = povpop - povle17,
              povpct0517 = pov0517/pop0517,
              povpctgt17 = povgt17/popgt17,
              povpcttot = povpop/totpop)
     
    f.saipeagyVAL$fips <- 0
    f.saipeagyVAL$geoname <- listID$plName1
    
    f.saipeagyVAL <- f.saipeagyVAL[,c("fips", "geoname", "year", 
                                     "pop0517","popgt17", "totpop",
                                     "pov0517", "povgt17", "povpop",
                                     "povpct0517", "povpctgt17","povpcttot")]
    
    f.saipectyVAL <- bind_rows(f.saipeagyVAL, f.saipectyVAL)

   }


# creating Plotly Chart
    f.saipecty_CL <- gather(f.saipectyVAL[,c(1:3,7:9)],age_cat,count,pov0517:povpop, factor_key=TRUE) 
    f.saipecty_CL$age_cat <- plyr::revalue(f.saipecty_CL$age_cat, c("pov0517" = "5 to 17",
                                                              "povgt17" = "18 and Older",
                                                              "povpop" = "Total"))
    f.saipecty_CL$age_cat <- factor(f.saipecty_CL$age_cat, levels = c("5 to 17",
                                                              "18 and Older", "Total"))


    f.saipecty_PL <- gather(f.saipectyVAL[,c(1:3,10:12)],age_cat,value,povpct0517:povpcttot, factor_key=TRUE) 
    f.saipecty_PL$age_cat <- plyr::revalue(f.saipecty_PL$age_cat, c("povpct0517" = "5 to 17",
                                                              "povpctgt17" = "18 and Older",
                                                              "povpcttot" = "Total"))
    f.saipecty_PL$age_cat <- factor(f.saipecty_PL$age_cat, levels = c("5 to 17",
                                                              "18 and Older", "Total"))
    
    f.saipecty_PLOT <- inner_join(f.saipecty_PL,f.saipecty_CL[,c(1,4,5)], by=c("fips","age_cat"))
    f.saipecty_PLOT$indText  <- paste0( f.saipectyVAL$geoname," Year: ",f.saipecty_PLOT$year," Ages: ",f.saipecty_PLOT$age_cat,", Percent in Poverty: ", percent(f.saipecty_PLOT$value * 100)," Estimate: ",NumFmt(f.saipecty_PLOT$count))  
    grTitle <- paste0("Percent Below Federal Poverty Level, ",listID$plName1," ",curYr)
    outCap <- captionSrc("SAIPE","","")
    xAxis <- list(title = "Age Category")
    yAxis <- list(title = 'Percent',tickformat = "%")

if(length(ctyfips) > 1 ){
POVPlot <- plot_ly(f.saipecty_PLOT, 
                   x = ~age_cat, 
                   y = ~value, 
                   type = 'bar', text = ~indText, hoverinfo = 'text',
                   transforms = list(
                      list(
                        type = 'filter',
                        target = ~geoname,
                        operation = '=',
                        value = unique(f.saipecty_PLOT$geoname)[1]))) %>%
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
                     args = list("transforms[0].value", unique(f.saipecty_PLOT$geoname)[1]),
                     label = unique(f.saipecty_PLOT$geoname)[1]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.saipecty_PLOT$geoname)[2]),
                     label = unique(f.saipecty_PLOT$geoname)[2]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.saipecty_PLOT$geoname)[3]),
                     label = unique(f.saipecty_PLOT$geoname)[3]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.saipecty_PLOT$geoname)[4]),
                     label = unique(f.saipecty_PLOT$geoname)[4]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.saipecty_PLOT$geoname)[5]),
                     label = unique(f.saipecty_PLOT$geoname)[5]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.saipecty_PLOT$geoname)[6]),
                     label = unique(f.saipecty_PLOT$geoname)[6]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.saipecty_PLOT$geoname)[7]),
                     label = unique(f.saipecty_PLOT$geoname)[7])
            )
        )))
} else {
   POVPlot <- plot_ly(f.saipecty_PLOT, 
                      x = ~age_cat, y = ~value,  type = 'bar',
                      text = ~indText, hoverinfo = 'text') %>%
    layout( title=grTitle, yaxis = yAxis, xaxis=xAxis,
          showlegend = FALSE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCap,
                      font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4))
   
}
    
# Creating Table data file

    f.saipe_POP <- f.saipectyVAL[,c(1:6)] 
    if(typeof(f.saipe_POP) == "list") {
      f.saipe_POP <- as.data.frame(f.saipe_POP)
    }
    
    f.saipe_POP[,4:6] <- sapply(f.saipe_POP[,4:6],NumFmt) 
    f.saipe_POPW <- gather(f.saipe_POP,age_cat,value, pop0517:totpop,factor_key=TRUE) %>%
           spread(year,value)
    f.saipe_POPW$type = "Total Population"
    f.saipe_POPW$age_cat <- as.character(f.saipe_POPW$age_cat)
    
    f.saipe_POV <- f.saipectyVAL[,c(1:3,7:9)] 
    if(typeof(f.saipe_POV) == "list") {
      f.saipe_POV <- as.data.frame(f.saipe_POV)
    }
    
    
    f.saipe_POV[,4:6] <- sapply(f.saipe_POV[,4:6],NumFmt) 
    f.saipe_POVW <- gather(f.saipe_POV,age_cat,value, pov0517:povpop,factor_key=TRUE) %>%
           spread(year,value)
    f.saipe_POVW$type = "Population Below FPL"
    f.saipe_POVW$age_cat <- as.character(f.saipe_POVW$age_cat)
    
    f.saipe_PCT <- f.saipectyVAL[,c(1:3,10:12)] 
    if(typeof(f.saipe_PCT) == "list") {
      f.saipe_PCT <- as.data.frame(f.saipe_PCT)
    }

    f.saipe_PCT[,4:6] <- sapply(f.saipe_PCT[,4:6], function(x) percent(x * 100))

    f.saipe_PCTW <- gather(f.saipe_PCT,age_cat,value, povpct0517:povpcttot,factor_key=TRUE) %>%
           spread(year,value)
    f.saipe_PCTW$type = "Percentage Below FPL"
    f.saipe_PCTW$age_cat <- as.character(f.saipe_PCTW$age_cat)
    

    f.saipecty_tab <- bind_rows(mutate_all(f.saipe_PCTW,as.character), mutate_all(f.saipe_POVW,as.character), mutate_all(f.saipe_POPW,as.character)) %>% arrange(fips,type)
    f.saipecty_tab <-f.saipecty_tab[,c(1,2,5,3,4)]
    names(f.saipecty_tab) <- c("fips","geoname","type", "age_cat","value")
    
    f.saipecty_tab$age_cat <- plyr::revalue(f.saipecty_tab$age_cat, 
                  c(
                    "povpct0517" = 	"5 to 17",
                    "povpctgt17" =  	"18 and Older",
                    "povpcttot" = "Total",
                    "pov0517" = 	"5 to 17",
                    "povgt17" = 	"18 and Older",
                    "povpop" = 	"Total",
                    "pop0517" = 	"5 to 17",
                    "popgt17" = 	"18 and Older",
                    "totpop" = 	"Total"))
    
    f.saipecty_tabW <-  f.saipecty_tab %>% spread(age_cat, value)
  
    
     #Clearing geoname
    if(length(ctyfips) == 1) {
      npanel1 <- 1
    } else {
      npanel1 = length(ctyfips) + 1
    }
 
    f.saipecty_tabW <- clrGeoname(f.saipecty_tabW,"geoname",npanel1,3)
    f.saipecty_tabW <- f.saipecty_tabW[,c(2,3,5,4,6)]
    names(f.saipecty_tabW)[1] <- "Agency/County"
    names(f.saipecty_tabW)[2] <- "Value"

 # Flex Table
  tab_head <- paste0("Percent Below Federal Poverty Level, ",listID$plName1, " ",curYr)
  
  
  f.povFlex <- flextable(
       f.saipecty_tabW,
       col_keys = names(f.saipecty_tabW)) %>%
       add_header_row(values=tab_head,top=TRUE,colwidths=5) %>%
       add_footer_row(values=outCap,top=FALSE,colwidths=5) %>%
       align(j=1:2, align="left", part="body") %>%
       width(j= 1, width=3) %>%
       width(j=2, width=1.8) %>%
       width(j=3:5,width=1) %>%
       height(part="body", height=0.4) %>%
       height(part="footer", height=0.4) %>%
       height(part="header", i=2,height=1)
      

  
    

  

  #bind list
  outList <- list("plot"= POVPlot, "data" = f.saipecty_PLOT, "table" = f.saipecty_tabW, "FlexTable" = f.povFlex,"caption" = outCap)
  
  return(outList)
}