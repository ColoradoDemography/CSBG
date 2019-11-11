#' educPRO Creates a Chart comparing educational attainment of two areas
#'
#' Modified from ms_ed in codemogProfile AB 12/2017
# 'Produces plotly plot, data and table for selected CSBG agencies 10/2019
#'
#' @param lvl is the data type ("Regional Summary" or "Counties)
#' @param listID is the list of selected county codes
#' @param curYr is the single year value to be extracted by county_sya
#' @return plotly graphic, data table and data file
#' @export
educPRO <- function(lvl,listID, ACS,curYr){

  # Collecting List of Counties

 f.educcty <- codemog_api(data="b17003",db=ACS,sumlev="50",geography="sumlev",meta="no")
 f.educcty[,8:30] <- sapply(f.educcty[,8:30],as.numeric)
 
  ctyfips <- as.character(as.numeric(substr(listID$list1,3,5)))
  
  
  state="08"
 
  

  f.educctyVAL <- f.educcty %>%
            filter(county %in% ctyfips) %>%
            group_by(county) %>%
           mutate(LTHS.POV	 = sum(b17003004, b17003009),
                  HSGRAD.POV	 = sum(b17003005, b17003010),
                  COLL.POV	 = sum(b17003006, b17003011),
                  BA.POV	 = sum(b17003007, b17003012),
                  TOT.POV	 = sum(b17003003, b17003008),
                  LTHS.TOT	 = sum(b17003004, b17003009, b17003015, b17003020),
                  HSGRAD.TOT	 = sum(b17003005, b17003010, b17003016, b17003021),
                  COLL.TOT	 = sum(b17003006, b17003011, b17003017, b17003022),
                  BA.TOT	 = sum(b17003007, b17003012, b17003018, b17003023),
                  TOT.TOT	 = sum(b17003003, b17003008, b17003014, b17003019),
                  LTHS.POV.P = LTHS.POV/TOT.POV,
                  HSGRAD.POV.P = HSGRAD.POV/TOT.POV,
                  COLL.POV.P = COLL.POV/TOT.POV,
                  BA.POV.P = BA.POV/TOT.POV,
                  TOT.POV.P = 1,
                  LTHS.TOT.P = LTHS.TOT/TOT.TOT,
                  HSGRAD.TOT.P = HSGRAD.TOT/TOT.TOT,
                  COLL.TOT.P = COLL.TOT/TOT.TOT,
                  BA.TOT.P = BA.TOT/TOT.TOT,
                  TOT.TOT.P = 1,
                  LTHS.POV.PCT = LTHS.POV/LTHS.TOT,
                  HSGRAD.POV.PCT = HSGRAD.POV/HSGRAD.TOT,
                  COLL.POV.PCT = COLL.POV/COLL.TOT,
                  BA.POV.PCT = BA.POV/BA.TOT,
                  TOT.POV.PCT = TOT.POV/TOT.TOT)

   f.educctyVAL <- f.educctyVAL[,c("geoname", "county", "LTHS.POV", "HSGRAD.POV", "COLL.POV",
                                    "BA.POV", "TOT.POV", "LTHS.TOT", "HSGRAD.TOT", "COLL.TOT",
                                    "BA.TOT", "TOT.TOT", "LTHS.POV.P", "HSGRAD.POV.P", "COLL.POV.P",
                                    "BA.POV.P", "TOT.POV.P", "LTHS.TOT.P", "HSGRAD.TOT.P",
                                    "COLL.TOT.P", "BA.TOT.P", "TOT.TOT.P",
                                    "LTHS.POV.PCT", "HSGRAD.POV.PCT", "COLL.POV.PCT","BA.POV.PCT","TOT.POV.PCT")]               

 if(length(ctyfips) > 1){
     f.educagyVAL <- f.educcty %>%
            filter(county %in% ctyfips) %>%
            summarize(LTHS.POV	 = sum(b17003004, b17003009),
                  HSGRAD.POV	 = sum(b17003005, b17003010),
                  COLL.POV	 = sum(b17003006, b17003011),
                  BA.POV	 = sum(b17003007, b17003012),
                  TOT.POV	 = sum(b17003003, b17003008),
                  LTHS.TOT	 = sum(b17003004, b17003009, b17003015, b17003020),
                  HSGRAD.TOT	 = sum(b17003005, b17003010, b17003016, b17003021),
                  COLL.TOT	 = sum(b17003006, b17003011, b17003017, b17003022),
                  BA.TOT	 = sum(b17003007, b17003012, b17003018, b17003023),
                  TOT.TOT	 = sum(b17003003, b17003008, b17003014, b17003019)) %>%
            mutate(LTHS.POV.P = LTHS.POV/TOT.POV,
                  HSGRAD.POV.P = HSGRAD.POV/TOT.POV,
                  COLL.POV.P = COLL.POV/TOT.POV,
                  BA.POV.P = BA.POV/TOT.POV,
                  TOT.POV.P = 1,
                  LTHS.TOT.P = LTHS.TOT/TOT.TOT,
                  HSGRAD.TOT.P = HSGRAD.TOT/TOT.TOT,
                  COLL.TOT.P = COLL.TOT/TOT.TOT,
                  BA.TOT.P = BA.TOT/TOT.TOT,
                  TOT.TOT.P = 1,
                  LTHS.POV.PCT = LTHS.POV/LTHS.TOT,
                  HSGRAD.POV.PCT = HSGRAD.POV/HSGRAD.TOT,
                  COLL.POV.PCT = COLL.POV/COLL.TOT,
                  BA.POV.PCT = BA.POV/BA.TOT,
                  TOT.POV.PCT = TOT.POV/TOT.TOT)
     
   f.educagyVAL$geoname <- listID$plName1  
   f.educagyVAL$county <- "1000"

   f.educagyVAL <- f.educagyVAL[,c("geoname", "county", "LTHS.POV", "HSGRAD.POV", "COLL.POV",
                                    "BA.POV", "TOT.POV", "LTHS.TOT", "HSGRAD.TOT", "COLL.TOT",
                                    "BA.TOT", "TOT.TOT", "LTHS.POV.P", "HSGRAD.POV.P", "COLL.POV.P",
                                    "BA.POV.P", "TOT.POV.P", "LTHS.TOT.P", "HSGRAD.TOT.P",
                                    "COLL.TOT.P", "BA.TOT.P", "TOT.TOT.P",
                                    "LTHS.POV.PCT", "HSGRAD.POV.PCT", "COLL.POV.PCT","BA.POV.PCT","TOT.POV.PCT")] 
   
   f.educctyVAL <- bind_rows(f.educagyVAL,f.educctyVAL)
 } 
  
# Prepping output objects

 f.educctyVAL$geoname <- sub(", Colorado","",f.educctyVAL$geoname)
  

 f.educctyPovC <- f.educctyVAL[,c("county", "LTHS.POV", "HSGRAD.POV", "COLL.POV", "BA.POV", "TOT.POV")] %>%
                 gather(var,value,LTHS.POV:TOT.POV) %>%
                 separate(var,c("educatt","level"))
 
 f.educctyPovP <- f.educctyVAL[,c("geoname", "county", "LTHS.POV.P", "HSGRAD.POV.P", "COLL.POV.P", "BA.POV.P", "TOT.POV.P")] %>%
              gather(var,value,LTHS.POV.P:TOT.POV.P)  %>%
                 separate(var,c("educatt","level","type"))
    
 f.educctyTotC <- f.educctyVAL[,c("county", "LTHS.TOT", "HSGRAD.TOT", "COLL.TOT", "BA.TOT", "TOT.TOT")] %>%
              gather(var,value,LTHS.TOT:TOT.TOT) %>%
                 separate(var,c("educatt","level"))
 
 f.educctyTotP <- f.educctyVAL[,c("geoname","county", "LTHS.TOT.P", "HSGRAD.TOT.P", "COLL.TOT.P", "BA.TOT.P", "TOT.TOT.P")] %>%
               gather(var,value,LTHS.TOT.P:TOT.TOT.P) %>%
                 separate(var,c("educatt","level","type"))
 
 f.educctyRow <- f.educctyVAL[,c("county", "LTHS.POV.PCT", "HSGRAD.POV.PCT", "COLL.POV.PCT", 
                                 "BA.POV.PCT", "TOT.POV.PCT")] %>%
                 gather(var,value,LTHS.POV.PCT:TOT.POV.PCT) %>%
                 separate(var,c("educatt","level","type"))
 

 # Creating Plot data file
 f.educctyPlot <- bind_rows(f.educctyPovP,f.educctyTotP) 
 f.educctyPlot <- f.educctyPlot[which(f.educctyPlot$educatt != "TOT"),]
 f.educctyPlot$level <- ifelse(f.educctyPlot$level == "POV","Persons Below FPL","All Persons")
 
  f.educctyPlot$educatt <- plyr::revalue(f.educctyPlot$educatt,  c("LTHS"="Less Than High School",
                         "HSGRAD" = "High School Graduate",
                         "COLL" = "Some College, Associates Degree",
                         "BA" = "Bachelor's Degree or Higher"))
  
  f.educctyPlot$educatt <- factor(f.educctyPlot$educatt, 
                                  levels= c("Less Than High School",
                                           "High School Graduate",
                                           "Some College, Associates Degree",
                                           "Bachelor's Degree or Higher")) 
  
  f.educctyPlot$level <- factor(f.educctyPlot$level,
                                levels= c("Persons Below FPL","All Persons"))
 
 # Plotly plot
  f.educctyPlot$indText  <- paste0( f.educctyPlot$geoname," Educational Attainment: ", f.educctyPlot$educatt," ",percent( f.educctyPlot$value * 100))  
    grTitle <- paste0("Table 4: Educational Attainment by Federal Poverty Level, ",listID$plName1,"\nPersons Age 25 and Older")


if(length(ctyfips) > 1 ){
 EDUCPlot <- f.educctyPlot %>%
  plot_ly(
    type = 'bar', 
    x = ~educatt, 
    y = ~value,
    color=~level,
    text = ~indText,
    hoverinfo = 'text',
    transforms = list(
      list(
        type = 'filter',
        target = ~geoname,
        operation = '=',
        value = unique(f.educctyPlot$geoname)[1]
      )
  )) %>% layout( title=grTitle, yaxis = list(title = 'Percent',tickformat = "%"), xaxis=list(title='Educational Attainment'),
          showlegend = TRUE, barmode = "group",
    updatemenus = list(
      list(
        type = 'dropdown',
        active = 0,
        buttons = list(
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.educctyPlot$geoname)[1]),
               label = unique(f.educctyPlot$geoname)[1]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.educctyPlot$geoname)[2]),
               label = unique(f.educctyPlot$geoname)[2]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.educctyPlot$geoname)[3]),
               label = unique(f.educctyPlot$geoname)[3]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.educctyPlot$geoname)[4]),
               label = unique(f.educctyPlot$geoname)[4]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.educctyPlot$geoname)[5]),
               label = unique(f.educctyPlot$geoname)[5]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.educctyPlot$geoname)[6]),
               label = unique(f.educctyPlot$geoname)[6]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.educctyPlot$geoname)[6]),
               label = unique(f.educctyPlot$geoname)[7])
      )
  )))
} else {
   EDUCPlot <- f.educctyPlot %>%
     plot_ly(
    type = 'bar', 
    x = ~educatt, 
    y = ~value,
    color=~level,
    text = ~indText,
    hoverinfo = 'text',
    transforms = list(
      list(
        type = 'filter',
        target = ~geoname,
        operation = '=',
        value = unique(f.educctyPlot$geoname)[1]
      )
  ))   %>% layout( title=grTitle, yaxis = list(title = 'Percent',tickformat = "%"), xaxis=list(title='Educational Attainment'),
          showlegend = TRUE, barmode="group")
   
}
 
 #Creating Table data file
 
 f.educctyTab <- inner_join(f.educctyPovP,f.educctyPovC, by=c("county","educatt")) %>%
                 inner_join(., f.educctyTotP[,2:6], by=c("county","educatt")) %>%
                 inner_join(., f.educctyTotC, by=c("county","educatt")) %>% 
                 inner_join(.,f.educctyRow, by=c("county","educatt"))
 
 f.educctyTab <-  f.educctyTab[,c(1:3,6,8,11,13,16)] %>%
                 arrange(county)
  f.educctyTab <-  f.educctyTab[,c(1,3:8)]
  
  # Recoding and Formatting

  f.educctyTab$educatt <- plyr::revalue(f.educctyTab$educatt,  c("LTHS"="Less Than High School",
                         "HSGRAD" = "High School Graduate",
                         "COLL" = "Some College, Associates Degree",
                         "BA" = "Bachelor's Degree or Higher",
                         "TOT" = "Total"))

   f.educctyTab$value.x <- percent(f.educctyTab$value.x * 100)
   f.educctyTab$value.x.x <- percent(f.educctyTab$value.x.x * 100)
   f.educctyTab$value <- percent(f.educctyTab$value * 100)

  f.educctyTab$value.y <- format(round(f.educctyTab$value.y ,digits=0),  big.mark=",")
  f.educctyTab$value.y.y <- format(round(f.educctyTab$value.y.y ,digits=0),  big.mark=",")
  
  f.educctyTab <- clrGeoname(f.educctyTab,ctyfips,5)
  
  names(f.educctyTab) <- c("Agency/County","Educational Attainment","Percent Below FPL", "Number Below FPL",
                           "Percent of Total","Total","Percent of Total Below FPL")

  # Flex Table
  tab_head <- paste0("Table 4: Educational Attainment by Federal Poverty Level, ",listID$plName1,"\nPersons Age 25 and Older")
  
    f.edFlex <- flextable(
      f.educctyTab,
       col_keys = names(f.educctyTab)) %>%
       add_header_row(values=tab_head,top=TRUE,colwidths=7) %>%
       add_footer_row(values=captionSrc("ACS",ACS,"B17003"),top=FALSE,colwidths=7) %>%
       align(j=1:2, align="left", part="body") 
  
 
  #bind list
  outList <- list("plot"= EDUCPlot, "data" =  f.educctyTab, "FlexTable" = f.edFlex)
  
  return(outList)
}