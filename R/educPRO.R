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
 outCap <- captionSrc("ACS",ACS,"B17003")
 f.educcty <- codemog_api(data="b17003",db=ACS,sumlev="50",geography="sumlev",meta="no")
 f.educcty[,c(3,8:30)] <- sapply(f.educcty[,c(3,8:30)],as.numeric)
 
  ctyfips <- as.character(as.numeric(substr(listID$list1,3,5)))
  
  
  state="08"
 
  

  f.educctyVAL <- f.educcty %>%
            filter(county %in% ctyfips) %>%
            group_by(county) %>%
           mutate(LTHS.POV.C	 = sum(b17003004, b17003009),
                  HSGRAD.POV.C	 = sum(b17003005, b17003010),
                  COLL.POV.C	 = sum(b17003006, b17003011),
                  BA.POV.C	 = sum(b17003007, b17003012),
                  TOT.POV.C	 = sum(b17003003, b17003008),
                  LTHS.TOT.C	 = sum(b17003004, b17003009, b17003015, b17003020),
                  HSGRAD.TOT.C	 = sum(b17003005, b17003010, b17003016, b17003021),
                  COLL.TOT.C	 = sum(b17003006, b17003011, b17003017, b17003022),
                  BA.TOT.C	 = sum(b17003007, b17003012, b17003018, b17003023),
                  TOT.TOT.C	 = sum(b17003003, b17003008, b17003014, b17003019),
                  LTHS.POV.P = LTHS.POV.C/TOT.POV.C,
                  HSGRAD.POV.P = HSGRAD.POV.C/TOT.POV.C,
                  COLL.POV.P = COLL.POV.C/TOT.POV.C,
                  BA.POV.P = BA.POV.C/TOT.POV.C,
                  TOT.POV.P = TOT.POV.C/TOT.POV.C,
                  LTHS.TOT.P = LTHS.TOT.C/TOT.TOT.C,
                  HSGRAD.TOT.P = HSGRAD.TOT.C/TOT.TOT.C,
                  COLL.TOT.P = COLL.TOT.C/TOT.TOT.C,
                  BA.TOT.P = BA.TOT.C/TOT.TOT.C,
                  TOT.TOT.P = TOT.TOT.C/TOT.TOT.C)

   f.educctyVAL <- f.educctyVAL[,c("geoname", "county", "LTHS.POV.C", "HSGRAD.POV.C", "COLL.POV.C",
                                    "BA.POV.C", "TOT.POV.C", "LTHS.TOT.C", "HSGRAD.TOT.C", "COLL.TOT.C",
                                    "BA.TOT.C", "TOT.TOT.C", "LTHS.POV.P", "HSGRAD.POV.P", "COLL.POV.P",
                                    "BA.POV.P", "TOT.POV.P", "LTHS.TOT.P", "HSGRAD.TOT.P",
                                    "COLL.TOT.P", "BA.TOT.P", "TOT.TOT.P")]               

 if(length(ctyfips) > 1){
     f.educagyVAL <- f.educcty %>%
            filter(county %in% ctyfips) %>%
            summarize(LTHS.POV.C	 = sum(b17003004, b17003009),
                  HSGRAD.POV.C	 = sum(b17003005, b17003010),
                  COLL.POV.C	 = sum(b17003006, b17003011),
                  BA.POV.C	 = sum(b17003007, b17003012),
                  TOT.POV.C	 = sum(b17003003, b17003008),
                  LTHS.TOT.C	 = sum(b17003004, b17003009, b17003015, b17003020),
                  HSGRAD.TOT.C	 = sum(b17003005, b17003010, b17003016, b17003021),
                  COLL.TOT.C	 = sum(b17003006, b17003011, b17003017, b17003022),
                  BA.TOT.C	 = sum(b17003007, b17003012, b17003018, b17003023),
                  TOT.TOT.C	 = sum(b17003003, b17003008, b17003014, b17003019)) %>%
            mutate( LTHS.POV.P = LTHS.POV.C/TOT.POV.C,
                  HSGRAD.POV.P = HSGRAD.POV.C/TOT.POV.C,
                  COLL.POV.P = COLL.POV.C/TOT.POV.C,
                  BA.POV.P = BA.POV.C/TOT.POV.C,
                  TOT.POV.P = TOT.POV.C/TOT.POV.C,
                  LTHS.TOT.P = LTHS.TOT.C/TOT.TOT.C,
                  HSGRAD.TOT.P = HSGRAD.TOT.C/TOT.TOT.C,
                  COLL.TOT.P = COLL.TOT.C/TOT.TOT.C,
                  BA.TOT.P = BA.TOT.C/TOT.TOT.C,
                  TOT.TOT.P = TOT.TOT.C/TOT.TOT.C)
     
   f.educagyVAL$geoname <- listID$plName1  
   f.educagyVAL$county <- 0

   f.educagyVAL <- f.educagyVAL[,c("geoname", "county", "LTHS.POV.C", "HSGRAD.POV.C", "COLL.POV.C",
                                    "BA.POV.C", "TOT.POV.C", "LTHS.TOT.C", "HSGRAD.TOT.C", "COLL.TOT.C",
                                    "BA.TOT.C", "TOT.TOT.C", "LTHS.POV.P", "HSGRAD.POV.P", "COLL.POV.P",
                                    "BA.POV.P", "TOT.POV.P", "LTHS.TOT.P", "HSGRAD.TOT.P",
                                    "COLL.TOT.P", "BA.TOT.P", "TOT.TOT.P")] 
   
   f.educctyVAL <- bind_rows(f.educagyVAL,f.educctyVAL)
 } 
  
# Prepping output objects

 f.educctyVAL$geoname <- sub(", Colorado","",f.educctyVAL$geoname)
  

 f.educctyPovC <- f.educctyVAL[,c("geoname","county", "LTHS.POV.C", "HSGRAD.POV.C", "COLL.POV.C", "BA.POV.C", "TOT.POV.C")] %>%
                 gather(var,count,LTHS.POV.C:TOT.POV.C) %>%
                 separate(var,c("educatt","lvl","type"))
 
 f.educctyPovP <- f.educctyVAL[,c("geoname", "county", "LTHS.POV.P", "HSGRAD.POV.P", "COLL.POV.P", "BA.POV.P", "TOT.POV.P")] %>%
              gather(var,value,LTHS.POV.P:TOT.POV.P)  %>%
                 separate(var,c("educatt","lvl","type"))
    
 f.educctyTotC <- f.educctyVAL[,c("geoname","county", "LTHS.TOT.C", "HSGRAD.TOT.C", "COLL.TOT.C", "BA.TOT.C", "TOT.TOT.C")] %>%
              gather(var,count,LTHS.TOT.C:TOT.TOT.C) %>%
                 separate(var,c("educatt","lvl","type"))
 
 f.educctyTotP <- f.educctyVAL[,c("geoname","county", "LTHS.TOT.P", "HSGRAD.TOT.P", "COLL.TOT.P", "BA.TOT.P", "TOT.TOT.P")] %>%
               gather(var,value,LTHS.TOT.P:TOT.TOT.P) %>%
                 separate(var,c("educatt","lvl","type"))
 
 

 # Creating Plot data file
 f.educctyCNT <- bind_rows(f.educctyPovC,f.educctyTotC)  %>% arrange(as.numeric(county))
 f.educctyCNT <- f.educctyCNT[which(f.educctyCNT$educatt != "TOT"),] 


 f.educctyPCT <- bind_rows(f.educctyPovP,f.educctyTotP)  %>% arrange(as.numeric(county))
 f.educctyPCT <- f.educctyPCT[which(f.educctyPCT$educatt != "TOT"),] 

 f.educctyPlot <- inner_join(f.educctyPCT,f.educctyCNT[,2:6],by=c("county","educatt","lvl"))
 
  f.educctyPlot$educatt <- plyr::revalue(f.educctyPlot$educatt,  c("LTHS"="Less Than High School",
                         "HSGRAD" = "High School Graduate",
                         "COLL" = "Some College, Associates Degree",
                         "BA" = "Bachelor's Degree or Higher"))
  
 f.educctyPlot$educatt <-  factor(f.educctyPlot$educatt, levels=c("Less Than High School",
                 "High School Graduate",
                  "Some College, Associates Degree",
                    "Bachelor's Degree or Higher")) 
  
 f.educctyPlot$lvl <- ifelse(f.educctyPlot$lvl == "POV","Persons Below FPL","All Persons")
 f.educctyPlot$lvl <- factor(f.educctyPlot$lvl, levels = c("Persons Below FPL","All Persons"))

 f.educctyPlot <-  f.educctyPlot %>% arrange(educatt,lvl,county)
 
 

 # Plotly plot
  f.educctyPlot$indText  <- paste0( f.educctyPlot$geoname," Educational Attainment: ", f.educctyPlot$educatt," Percentage: ",percent(f.educctyPlot$value * 100)," Count: ",NumFmt(f.educctyPlot$count)) 
    grTitle <- paste0("Educational Attainment by Federal Poverty Level, ",listID$plName1,"\nPersons Age 25 and Older")
    xAxis <- list(title='Educational Attainment')
    yAxis <- list(title = 'Percent',tickformat = ".1%")

if(length(ctyfips) > 1 ){
EDUCPlot <- f.educctyPlot %>%
  plot_ly(
    type = 'bar', 
    x = ~educatt, 
    y = ~value,
    color= ~lvl,
    text = ~indText,
    hoverinfo = 'text',
    transforms = list(
      list(
        type = 'filter',
        target = ~geoname,
        operation = '=',
        value = unique(f.educctyPlot$geoname)[1]
      )
  )) %>% layout( title=grTitle, yaxis = yAxis, xaxis=xAxis,
          showlegend = TRUE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCap,
                      font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4),
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
               args = list("transforms[0].value", unique(f.educctyPlot$geoname)[7]),
               label = unique(f.educctyPlot$geoname)[7])
      )
  )))
} else {
   EDUCPlot <- f.educctyPlot %>%
     plot_ly(
    type = 'bar', 
    x = ~educatt, 
    y = ~value,
    color = ~lvl,
    text = ~indText,
    hoverinfo = 'text',
    transforms = list(
      list(
        type = 'filter',
        target = ~geoname,
        operation = '=',
        value = unique(f.educctyPlot$geoname)[1]
      )
  ))   %>% layout( title=grTitle, yaxis = yAxis, xaxis=xAxis,
          showlegend = TRUE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCap,
                      font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4))
}    
 
 #Creating Table data file


 f.educctyTAB_TOTP <- spread(f.educctyTotP,educatt,value)
 f.educctyTAB_TOTC <- spread(f.educctyTotC,educatt,count)
 
 f.educctyTAB_POVP <- spread(f.educctyPovP,educatt,value)
 f.educctyTAB_POVC <- spread(f.educctyPovC,educatt,count)
 
  f.educctyTAB_TOTP$lvl <- "All Persons"
  f.educctyTAB_TOTP$type <- "Percentage"
  f.educctyTAB_TOTP[,5:9] <- lapply(f.educctyTAB_TOTP[,5:9],function(x) percent(x *100))
   
  f.educctyTAB_TOTC$lvl <- "All Persons"
  f.educctyTAB_TOTC$type <- "Count"
  f.educctyTAB_TOTC[,5:9] <- lapply(f.educctyTAB_TOTC[,5:9],NumFmt)

  f.educctyTAB_POVP$lvl <- "Persons Below FPL"
  f.educctyTAB_POVP$type <- "Percentage"
  f.educctyTAB_POVP[,5:9] <- lapply(f.educctyTAB_POVP[,5:9],function(x) percent(x *100))

   
  f.educctyTAB_POVC$lvl <- "Persons Below FPL"
  f.educctyTAB_POVC$type <- "Count" 
  f.educctyTAB_POVC[,5:9] <- lapply(f.educctyTAB_POVC[,5:9],NumFmt)

  
  f.educctyTab <- bind_rows(list(f.educctyTAB_POVP,f.educctyTAB_POVC),list(f.educctyTAB_TOTP,f.educctyTAB_TOTC)) %>%
     arrange(county,desc(lvl),desc(type))
  
  f.educctyTab <- f.educctyTab[,c(1,3,4,8,7,6,5,9)]
  


  
   #Clearing geoname
    if(length(ctyfips) == 1) {
      npanel1 <- 1
    } else {
      npanel1 = length(ctyfips) + 1
    }
  
  f.educctyTab <- clrGeoname(f.educctyTab,"geoname",npanel1,4)
  for(i in 1:nrow(f.educctyTab)){
      if(i %% 2 == 0){
        f.educctyTab[i,2] <- ""
      } 
      }
  
  names(f.educctyTab) <- c("Agency/County","Poverty Level","Value", "Less Than High School",
                          "High School Graduate",
                         "Some College, Associates Degree",
                        "Bachelor's Degree or Higher", "Total")

  # Flex Table
  tab_head <- paste0("Educational Attainment by Federal Poverty Level, ",listID$plName1,"\nPersons Age 25 and Older")
  
    f.edFlex <- flextable(
      f.educctyTab,
       col_keys = names(f.educctyTab)) %>%
       add_header_row(values=tab_head,top=TRUE,colwidths=8) %>%
       add_footer_row(values=outCap,top=FALSE,colwidths=8) %>%
       align(j=1:8, align="center", part="header") %>%
       align(j=1:2, align="left", part="body") %>%
       align(j=3:8, align="right", part="body") %>%
       align(j=1, align="left", part="footer") %>%
       width(j=1, width=3) %>%
       width(j=2,width=1.5) %>%
       width(j=3:8,width=1) %>%
       height(part="footer", height=0.4) %>%
       height(part="header", height=1)
 
    
    f.educctyDat <- bind_rows(f.educctyTotP, f.educctyPovP) 

  #bind list
  outList <- list("plot"= EDUCPlot, "data" = f.educctyPlot, "table" =  f.educctyTab, "FlexTable" = f.edFlex, "caption" = outCap)
  
  return(outList)
}