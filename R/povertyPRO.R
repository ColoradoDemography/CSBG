#' povertyPRO Creates a materials showing ppopulation by federal poverty level
#'
# 'Produces plotly plot, data and table for selected CSBG agencies 10/2019
#'
#' @param lvl is the data type ("Regional Summary" or "Counties)
#' @param listID is the list of selected county codes
#' @param ACS and PreACS Specify the vintage of the ACS 5-year file
#' @param tabtype is the table type (3  = Poverty, 5 = Poverty by age 6 =poverty age trend)
#' @return plotly graphic, data table and data file
#' @export
povertyPRO <- function(lvl,listID, ACS,PreACS,curYr,tabtype){

  # Collecting List of Counties

 f.povertycty <- codemog_api(data="b17024",db=ACS,sumlev="50",geography="sumlev",meta="no")
 f.povertycty[,8:138] <- sapply(f.povertycty[,8:138],as.numeric)
 
  ctyfips <- as.character(as.numeric(substr(listID$list1,3,5)))
  
  f.povertyctydat <- f.povertycty %>%  filter(county %in% ctyfips) 
    
  f.povertyctydat$geoname <- sub(", Colorado","", f.povertyctydat$geoname) 
  state="08"
 
  if(tabtype == 3) {
  
      f.povertyctyVAL <- f.povertyctydat %>%
            group_by(county) %>%
           mutate(
              TOT.POP = b17024001,
              POV.LT50 = sum(b17024003, b17024016, b17024029, b17024042, b17024055, b17024068, 
                             b17024081, b17024094, b17024107, b17024120),
              POV.5074 = sum(b17024004, b17024017, b17024030, b17024043, b17024056, b17024069, 
                             b17024082, b17024095, b17024108, b17024121),
              POV.7599 = sum(b17024005, b17024018, b17024031, b17024044, b17024057, b17024070, 
                             b17024083, b17024096, b17024109, b17024122),
              POV.100124 = sum(b17024006, b17024019, b17024032, b17024045, b17024058, b17024071, 
                               b17024084, b17024097, b17024110, b17024123),
              POV.125149 = sum(b17024007, b17024020, b17024033, b17024046, b17024059, b17024072, 
                               b17024085, b17024098, b17024111, b17024124),
              POV.150174 = sum(b17024008, b17024021, b17024034, b17024047, b17024060, b17024073, 
                               b17024086, b17024099, b17024112, b17024125),
              POV.175199 = sum(b17024009, b17024022, b17024035, b17024048, b17024061, b17024074, 
                               b17024087, b17024100, b17024113, b17024126, b17024010, b17024023, 
                               b17024036, b17024049, b17024062, b17024075, b17024088, b17024101, b17024114, b17024127),
              POV.200299 = sum(b17024011, b17024024, b17024037, b17024050, b17024063, b17024076, 
                               b17024089, b17024102, b17024115, b17024128),
              POV.GE300 = sum(b17024012, b17024025, b17024038, b17024051, b17024064, b17024077, 
                              b17024090, b17024103, b17024116, b17024129, b17024013, b17024026, 
                              b17024039, b17024052, b17024065, b17024078, b17024091, b17024104, 
                              b17024117, b17024130, b17024014, b17024027, b17024040, b17024053, 
                              b17024066, b17024079, b17024092, b17024105, b17024118, b17024131),
              POV.LT50.PCT = POV.LT50/TOT.POP, 
              POV.5074.PCT = POV.5074/TOT.POP, 
              POV.7599.PCT = POV.7599/TOT.POP, 
              POV.100124.PCT = POV.100124/TOT.POP, 
              POV.125149.PCT = POV.125149/TOT.POP, 
              POV.150174.PCT = POV.150174/TOT.POP, 
              POV.175199.PCT = POV.175199/TOT.POP, 
              POV.200299.PCT = POV.200299/TOT.POP, 
              POV.GE300.PCT = POV.GE300/TOT.POP,
              TOT.POP.PCT = 1)
          

   f.povertyctyVAL <- f.povertyctyVAL[,c("geoname", "county", "POV.LT50", "POV.5074", "POV.7599",
                                    "POV.100124", "POV.125149", "POV.150174", "POV.175199",
                                    "POV.200299", "POV.GE300", "TOT.POP",
                                    "POV.LT50.PCT", "POV.5074.PCT", "POV.7599.PCT",
                                    "POV.100124.PCT", "POV.125149.PCT", "POV.150174.PCT", 
                                    "POV.175199.PCT", "POV.200299.PCT", "POV.GE300.PCT", "TOT.POP.PCT")]               

 if(length(ctyfips) > 1){
      f.povertyagyVAL <- f.povertyctydat %>%
            summarize(
              TOT.POP = sum(b17024001),
              POV.LT50 = sum(b17024003, b17024016, b17024029, b17024042, b17024055, b17024068, 
                             b17024081, b17024094, b17024107, b17024120),
              POV.5074 = sum(b17024004, b17024017, b17024030, b17024043, b17024056, b17024069, 
                             b17024082, b17024095, b17024108, b17024121),
              POV.7599 = sum(b17024005, b17024018, b17024031, b17024044, b17024057, b17024070, 
                             b17024083, b17024096, b17024109, b17024122),
              POV.100124 = sum(b17024006, b17024019, b17024032, b17024045, b17024058, b17024071, 
                               b17024084, b17024097, b17024110, b17024123),
              POV.125149 = sum(b17024007, b17024020, b17024033, b17024046, b17024059, b17024072, 
                               b17024085, b17024098, b17024111, b17024124),
              POV.150174 = sum(b17024008, b17024021, b17024034, b17024047, b17024060, b17024073, 
                               b17024086, b17024099, b17024112, b17024125),
              POV.175199 = sum(b17024009, b17024022, b17024035, b17024048, b17024061, b17024074, 
                               b17024087, b17024100, b17024113, b17024126, b17024010, b17024023, 
                               b17024036, b17024049, b17024062, b17024075, b17024088, b17024101, b17024114, b17024127),
              POV.200299 = sum(b17024011, b17024024, b17024037, b17024050, b17024063, b17024076, 
                               b17024089, b17024102, b17024115, b17024128),
              POV.GE300 = sum(b17024012, b17024025, b17024038, b17024051, b17024064, b17024077, 
                              b17024090, b17024103, b17024116, b17024129, b17024013, b17024026, 
                              b17024039, b17024052, b17024065, b17024078, b17024091, b17024104, 
                              b17024117, b17024130, b17024014, b17024027, b17024040, b17024053, 
                              b17024066, b17024079, b17024092, b17024105, b17024118, b17024131) ) %>%
            mutate( POV.LT50.PCT = POV.LT50/TOT.POP, 
              POV.5074.PCT = POV.5074/TOT.POP, 
              POV.7599.PCT = POV.7599/TOT.POP, 
              POV.100124.PCT = POV.100124/TOT.POP, 
              POV.125149.PCT = POV.125149/TOT.POP, 
              POV.150174.PCT = POV.150174/TOT.POP, 
              POV.175199.PCT = POV.175199/TOT.POP, 
              POV.200299.PCT = POV.200299/TOT.POP, 
              POV.GE300.PCT = POV.GE300/TOT.POP,
              TOT.POP.PCT = 1)
     
  f.povertyagyVAL$geoname <- listID$plName1  
  f.povertyagyVAL$county <- "1000"

  f.povertyagyVAL <- f.povertyagyVAL[,c("geoname", "county", "POV.LT50", "POV.5074", "POV.7599",
                                    "POV.100124", "POV.125149", "POV.150174", "POV.175199",
                                    "POV.200299", "POV.GE300", "TOT.POP",
                                    "POV.LT50.PCT", "POV.5074.PCT", "POV.7599.PCT",
                                    "POV.100124.PCT", "POV.125149.PCT", "POV.150174.PCT",  
                                    "POV.175199.PCT", "POV.200299.PCT", "POV.GE300.PCT", "TOT.POP.PCT")]  
   
   f.povertyctyVAL <- bind_rows(f.povertyagyVAL,f.povertyctyVAL)
 } 

    
f.povertycty_C <-  f.povertyctyVAL[,1:12] 

f.povertycty_P <- f.povertyctyVAL[,c(1,2,13:22)] 

f.povertycty_PL <- f.povertycty_P %>% 
        gather(POV.LEVEL,value,POV.LT50.PCT:TOT.POP.PCT,factor_key=TRUE) %>%
        arrange(POV.LEVEL)

f.povertycty_PL <- f.povertycty_PL[which(f.povertycty_PL$POV.LEVEL != "TOT.POP.PCT"),]

f.povertycty_PL$POV.LEVEL <- plyr::revalue(f.povertycty_PL$POV.LEVEL,
                                c("POV.LT50.PCT" = "Less than 50%","POV.5074.PCT" = "50 to 74%",
                                  "POV.7599.PCT" = "75 to 99%", "POV.100124.PCT" = "100 to 124%", 
                                  "POV.125149.PCT" = "125 to 149%", "POV.150174.PCT" = "150 to 174%",
                                  "POV.175199.PCT" = "175 to 199%", "POV.200299.PCT" = "200 to 299%",
                                  "POV.GE300.PCT" = "300% and Higher"))
 f.povertycty_PL$POV.LEVEL <- factor(f.povertycty_PL$POV.LEVEL, 
                                      levels = c("Less than 50%", "50 to 74%",
                                   "75 to 99%", "100 to 124%", 
                                   "125 to 149%", "150 to 174%",
                                   "175 to 199%", "200 to 299%",
                                   "300% and Higher","Total"))
 # Plotly plot
  f.povertycty_PL$indText  <- paste0( f.povertycty_PL$geoname," Percent of Federal Poverty Level, ", f.povertycty_PL$POV.LEVEL,": ", percent( f.povertycty_PL$value * 100))  
    grTitle <- paste0("Table 3: Population by Percentage of Federal Poverty Level, ",listID$plName1)


if(length(ctyfips) > 1 ){
POVPlot <- f.povertycty_PL %>%
  plot_ly(
    type = 'bar', 
    x = ~POV.LEVEL, 
    y = ~value,
    text = ~indText,
    hoverinfo = 'text',
    transforms = list(
      list(
        type = 'filter',
        target = ~geoname,
        operation = '=',
        value = unique(f.povertycty_PL$geoname)[1]
      )
  )) %>% layout( title=grTitle, yaxis = list(title = 'Percent',tickformat = "%"), xaxis=list(title='Percentage of Federal Poverty Level'),
          showlegend = FALSE,
    updatemenus = list(
      list(
        type = 'dropdown',
        active = 0,
        buttons = list(
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.povertycty_PL$geoname)[1]),
               label = unique(f.povertycty_PL$geoname)[1]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.povertycty_PL$geoname)[2]),
               label = unique(f.povertycty_PL$geoname)[2]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.povertycty_PL$geoname)[3]),
               label = unique(f.povertycty_PL$geoname)[3]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.povertycty_PL$geoname)[4]),
               label = unique(f.povertycty_PL$geoname)[4]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.povertycty_PL$geoname)[5]),
               label = unique(f.povertycty_PL$geoname)[5]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.povertycty_PL$geoname)[6]),
               label = unique(f.povertycty_PL$geoname)[6]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.povertycty_PL$geoname)[6]),
               label = unique(f.povertycty_PL$geoname)[7])
      )
  )))
} else {
   POVPlot <- f.povertycty_PL %>%
     plot_ly(
    type = 'bar', 
    x = ~POV.LEVEL, 
    y = ~value,
    text = ~indText,
    hoverinfo = 'text',
    transforms = list(
      list(
        type = 'filter',
        target = ~geoname,
        operation = '=',
        value = unique(f.povertycty_PL$geoname)[1]
      )
  ))   %>% layout( title=grTitle, yaxis = list(title = 'Percent',tickformat = "%"), xaxis=list(title='Percentage of Federal Poverty Level'),
          showlegend = FALSE)
   
}


 #Creating Table data file

 f.povertycty_C$type <- "Count"
 f.povertycty_C <- f.povertycty_C[,c(1,2,13,3:12)]
 f.povertycty_C[,4:13] <- sapply(f.povertycty_C[,4:13],NumFmt)


  f.povertycty_P$type <- "Percentage"
 f.povertycty_P <- f.povertycty_P[,c(1,2,13,3:12)]
 f.povertycty_P[,4:13] <- lapply(f.povertycty_P[,4:13], function(x) x * 100)
 f.povertycty_P[,4:13] <- sapply(f.povertycty_P[,4:13],percent)
 names(f.povertycty_P) <- c("geoname","county", "type","POV.LT50", "POV.5074",
                                   "POV.7599", "POV.100124", 
                                   "POV.125149", "POV.150174",
                                   "POV.175199", "POV.200299",
                                   "POV.GE300","TOT.POP")
 
 f.povertycty_tab <- bind_rows(f.povertycty_C,f.povertycty_P) %>% arrange(county,desc(type))
f.povertycty_tab <- clrGeoname( f.povertycty_tab,ctyfips,2)
f.povertycty_tab <- f.povertycty_tab[,c(1,3:13)]

names(f.povertycty_tab) <- c("Agency/County","Value","Less than 50%", "50 to 74%",
                                   "75 to 99%", "100 to 124%", 
                                   "125 to 149%", "150 to 174%",
                                   "175 to 199%", "200 to 299%",
                                   "300% and Higher","Total")


# Flex Table
  tab_head <- paste0("Table 3: Population by Percentage of Federal Poverty Level, ",listID$plName1)
  
    f.povFlex <- flextable(
      f.povertycty_tab,
       col_keys = names(f.povertycty_tab)) %>%
       add_header_row(values=tab_head,top=TRUE,colwidths=12) %>%
       add_footer_row(values=captionSrc("ACS",ACS,"B17024"),top=FALSE,colwidths=12) %>%
       align(j=1:2, align="left", part="body") 


}  # tybtype == 3
  
if(tabtype == 5) {

POV.LT50 <- f.povertyctydat %>%
      group_by(geoname,county) %>%
      mutate(POV.LEVEL = "POV.LT50",
      nLE05.TOT  = b17024003,
      n0611.TOT  = b17024016,
      n1217.TOT  = b17024029,
      n1824.TOT  = b17024042,
      n2534.TOT  = b17024055,
      n3544.TOT  = b17024068,
      n4554.TOT  = b17024081,
      n5564.TOT  = b17024094,
      n6574.TOT  = b17024107,
      nGE75.TOT  = b17024120,
      nAGE.TOT = nLE05.TOT + n0611.TOT + n1217.TOT + n1824.TOT + n2534.TOT + n3544.TOT + n4554.TOT + n5564.TOT + n6574.TOT + nGE75.TOT) %>%
      select(c(geoname, county,POV.LEVEL:nAGE.TOT)) 



POV.5074 <- f.povertyctydat %>%
      group_by(geoname,county) %>%
      mutate(POV.LEVEL = "POV.5074",
      nLE05.TOT  = b17024004,
      n0611.TOT  = b17024017,
      n1217.TOT  = b17024030,
      n1824.TOT  = b17024043,
      n2534.TOT  = b17024056,
      n3544.TOT  = b17024069,
      n4554.TOT  = b17024082,
      n5564.TOT  = b17024095,
      n6574.TOT  = b17024108,
      nGE75.TOT  = b17024121,
      nAGE.TOT = nLE05.TOT + n0611.TOT + n1217.TOT + n1824.TOT + n2534.TOT + n3544.TOT + n4554.TOT + n5564.TOT + n6574.TOT + nGE75.TOT) %>%
      select(c(geoname, county,POV.LEVEL:nAGE.TOT)) 


POV.7599 <- f.povertyctydat %>%
        group_by(geoname,county) %>%
        mutate(POV.LEVEL = "POV.7599",
        nLE05.TOT  = b17024005,
        n0611.TOT  = b17024018,
        n1217.TOT  = b17024031,
        n1824.TOT  = b17024044,
        n2534.TOT  = b17024057,
        n3544.TOT  = b17024070,
        n4554.TOT  = b17024083,
        n5564.TOT  = b17024096,
        n6574.TOT  = b17024109,
        nGE75.TOT  = b17024122,
        nAGE.TOT = nLE05.TOT + n0611.TOT + n1217.TOT + n1824.TOT + n2534.TOT + n3544.TOT + n4554.TOT + n5564.TOT + n6574.TOT + nGE75.TOT) %>%
        select(c(geoname, county,POV.LEVEL:nAGE.TOT)) 


POV.100124 <- f.povertyctydat %>%
      group_by(geoname,county) %>%
      mutate(POV.LEVEL = "POV.100124",
      nLE05.TOT  = b17024006,
      n0611.TOT  = b17024019,
      n1217.TOT  = b17024032,
      n1824.TOT  = b17024045,
      n2534.TOT  = b17024058,
      n3544.TOT  = b17024071,
      n4554.TOT  = b17024084,
      n5564.TOT  = b17024097,
      n6574.TOT  = b17024110,
      nGE75.TOT  = b17024123,
      nAGE.TOT = nLE05.TOT + n0611.TOT + n1217.TOT + n1824.TOT + n2534.TOT + n3544.TOT + n4554.TOT + n5564.TOT + n6574.TOT + nGE75.TOT) %>%
      select(c(geoname, county,POV.LEVEL:nAGE.TOT)) 


POV.125149 <- f.povertyctydat %>%
      group_by(geoname,county) %>%
      mutate(POV.LEVEL = "POV.125149",
      nLE05.TOT  = b17024007,
      n0611.TOT  = b17024020,
      n1217.TOT  = b17024033,
      n1824.TOT  = b17024046,
      n2534.TOT  = b17024059,
      n3544.TOT  = b17024072,
      n4554.TOT  = b17024085,
      n5564.TOT  = b17024098,
      n6574.TOT  = b17024111,
      nGE75.TOT  = b17024124,
      nAGE.TOT = nLE05.TOT + n0611.TOT + n1217.TOT + n1824.TOT + n2534.TOT + n3544.TOT + n4554.TOT + n5564.TOT + n6574.TOT + nGE75.TOT) %>%
      select(c(geoname, county,POV.LEVEL:nAGE.TOT)) 


POV.150174 <- f.povertyctydat %>%
      group_by(geoname,county) %>%
      mutate(POV.LEVEL = "POV.150174",
      nLE05.TOT  = b17024008,
      n0611.TOT  = b17024021,
      n1217.TOT  = b17024034,
      n1824.TOT  = b17024047,
      n2534.TOT  = b17024060,
      n3544.TOT  = b17024073,
      n4554.TOT  = b17024086,
      n5564.TOT  = b17024099,
      n6574.TOT  = b17024112,
      nGE75.TOT  = b17024125,
      nAGE.TOT = nLE05.TOT + n0611.TOT + n1217.TOT + n1824.TOT + n2534.TOT + n3544.TOT + n4554.TOT + n5564.TOT + n6574.TOT + nGE75.TOT) %>%
      select(c(geoname, county,POV.LEVEL:nAGE.TOT)) 


POV.175199 <- f.povertyctydat %>%
      group_by(geoname,county) %>%
      mutate(POV.LEVEL = "POV.175199",
      nLE05.TOT  = b17024009 + b17024010,
      n0611.TOT  = b17024022 + b17024023,
      n1217.TOT  = b17024035 + b17024036,
      n1824.TOT  = b17024048 + b17024049,
      n2534.TOT  = b17024061 + b17024062,
      n3544.TOT  = b17024074 + b17024075,
      n4554.TOT  = b17024087 + b17024088,
      n5564.TOT  = b17024100 + b17024101,
      n6574.TOT  = b17024113 + b17024114,
      nGE75.TOT  = b17024126 + b17024127,
      nAGE.TOT = nLE05.TOT + n0611.TOT + n1217.TOT + n1824.TOT + n2534.TOT + n3544.TOT + n4554.TOT + n5564.TOT + n6574.TOT + nGE75.TOT) %>%
      select(c(geoname, county,POV.LEVEL:nAGE.TOT)) 



POV.200299 <- f.povertyctydat %>%
      group_by(geoname,county) %>%
      mutate(POV.LEVEL = "POV.200299",
      nLE05.TOT  = b17024011,
      n0611.TOT  = b17024024,
      n1217.TOT  = b17024037,
      n1824.TOT  = b17024050,
      n2534.TOT  = b17024063,
      n3544.TOT  = b17024076,
      n4554.TOT  = b17024089,
      n5564.TOT  = b17024102,
      n6574.TOT  = b17024115,
      nGE75.TOT  = b17024128,
      nAGE.TOT = nLE05.TOT + n0611.TOT + n1217.TOT + n1824.TOT + n2534.TOT + n3544.TOT + n4554.TOT + n5564.TOT + n6574.TOT + nGE75.TOT) %>%
      select(c(geoname, county,POV.LEVEL:nAGE.TOT)) 


POV.GE300 <- f.povertyctydat %>%
      group_by(geoname,county) %>%
      mutate(POV.LEVEL = "POV.GE300",
      nLE05.TOT  = b17024012 + b17024013 + b17024014,
      n0611.TOT  = b17024025 + b17024026 + b17024027,
      n1217.TOT  = b17024038 + b17024039 + b17024040,
      n1824.TOT  = b17024051 + b17024052 + b17024053,
      n2534.TOT  = b17024064 + b17024065 + b17024066,
      n3544.TOT  = b17024077 + b17024078 + b17024079,
      n4554.TOT  = b17024090 + b17024091 + b17024092,
      n5564.TOT  = b17024103 + b17024104 + b17024105,
      n6574.TOT  = b17024116 + b17024117 + b17024118,
      nGE75.TOT  = b17024129 + b17024130 + b17024131,
      nAGE.TOT = nLE05.TOT + n0611.TOT + n1217.TOT + n1824.TOT + n2534.TOT + n3544.TOT + n4554.TOT + n5564.TOT + n6574.TOT + nGE75.TOT) %>%
      select(c(geoname, county,POV.LEVEL:nAGE.TOT)) 




f.povertyctyVAL2 <- bind_rows(list(POV.LT50, POV.5074,	POV.7599,	POV.100124,	POV.125149,	POV.150174,	POV.175199,	POV.200299,	POV.GE300)) 
# Adding Colsums
f.povertyctysum <- f.povertyctyVAL2 %>% 
              group_by(geoname, county) %>%
              summarize_if(is.numeric,sum)
                   

f.povertyctyVAL3  <- bind_rows(f.povertyctyVAL2,f.povertyctysum) %>%
                   arrange(county) %>%
                   group_by(geoname, county, POV.LEVEL) %>%
                   mutate(nLE05.PCT	 =	nLE05.TOT/nAGE.TOT,
                          n0611.PCT	 =	n0611.TOT/nAGE.TOT,
                          n1217.PCT	 =	n1217.TOT/nAGE.TOT,
                          n1824.PCT	 =	n1824.TOT/nAGE.TOT,
                          n2534.PCT	 =	n2534.TOT/nAGE.TOT,
                          n3544.PCT	 =	n3544.TOT/nAGE.TOT,
                          n4554.PCT	 =	n4554.TOT/nAGE.TOT,
                          n5564.PCT	 =	n5564.TOT/nAGE.TOT,
                          n6574.PCT	 =	n6574.TOT/nAGE.TOT,
                          nGE75.PCT	 =	nGE75.TOT/nAGE.TOT,
                          nAGE.PCT	 =	nAGE.TOT/nAGE.TOT)

f.povertyctyVAL3$POV.LEVEL <- ifelse(is.na(f.povertyctyVAL3$POV.LEVEL),"Total",f.povertyctyVAL3$POV.LEVEL)

if(length(ctyfips) > 1) {

POV.LT50.AGY <- f.povertyctydat %>%
          summarize(POV.LEVEL = "POV.LT50",
          nLE05.TOT  = sum(b17024003),
          n0611.TOT  = sum(b17024016),
          n1217.TOT  = sum(b17024029),
          n1824.TOT  = sum(b17024042),
          n2534.TOT  = sum(b17024055),
          n3544.TOT  = sum(b17024068),
          n4554.TOT  = sum(b17024081),
          n5564.TOT  = sum(b17024094),
          n6574.TOT  = sum(b17024107),
          nGE75.TOT  = sum(b17024120)) %>%
          mutate(nAGE.TOT = nLE05.TOT + n0611.TOT + n1217.TOT + n1824.TOT + n2534.TOT + n3544.TOT + n4554.TOT + n5564.TOT + n6574.TOT + nGE75.TOT) %>%
          select(c(POV.LEVEL:nAGE.TOT)) 



POV.5074.AGY <- f.povertyctydat %>%
          summarize(POV.LEVEL = "POV.5074",
          nLE05.TOT  = sum(b17024004),
          n0611.TOT  = sum(b17024017),
          n1217.TOT  = sum(b17024030),
          n1824.TOT  = sum(b17024043),
          n2534.TOT  = sum(b17024056),
          n3544.TOT  = sum(b17024069),
          n4554.TOT  = sum(b17024082),
          n5564.TOT  = sum(b17024095),
          n6574.TOT  = sum(b17024108),
          nGE75.TOT  = sum(b17024121)) %>%
          mutate(nAGE.TOT = nLE05.TOT + n0611.TOT + n1217.TOT + n1824.TOT + n2534.TOT + n3544.TOT + n4554.TOT + n5564.TOT + n6574.TOT + nGE75.TOT) %>%
          select(c(POV.LEVEL:nAGE.TOT)) 


POV.7599.AGY <- f.povertyctydat %>%
            summarize(POV.LEVEL = "POV.7599",
            nLE05.TOT  = sum(b17024005),
            n0611.TOT  = sum(b17024018),
            n1217.TOT  = sum(b17024031),
            n1824.TOT  = sum(b17024044),
            n2534.TOT  = sum(b17024057),
            n3544.TOT  = sum(b17024070),
            n4554.TOT  = sum(b17024083),
            n5564.TOT  = sum(b17024096),
            n6574.TOT  = sum(b17024109),
            nGE75.TOT  = sum(b17024122)) %>%
            mutate(nAGE.TOT = nLE05.TOT + n0611.TOT + n1217.TOT + n1824.TOT + n2534.TOT + n3544.TOT + n4554.TOT + n5564.TOT + n6574.TOT + nGE75.TOT) %>%
            select(c(POV.LEVEL:nAGE.TOT)) 


POV.100124.AGY <- f.povertyctydat %>%
              summarize(POV.LEVEL = "POV.100124",
              nLE05.TOT  = sum(b17024006),
              n0611.TOT  = sum(b17024019),
              n1217.TOT  = sum(b17024032),
              n1824.TOT  = sum(b17024045),
              n2534.TOT  = sum(b17024058),
              n3544.TOT  = sum(b17024071),
              n4554.TOT  = sum(b17024084),
              n5564.TOT  = sum(b17024097),
              n6574.TOT  = sum(b17024110),
              nGE75.TOT  = sum(b17024123)) %>%
              mutate(nAGE.TOT = nLE05.TOT + n0611.TOT + n1217.TOT + n1824.TOT + n2534.TOT + n3544.TOT + n4554.TOT + n5564.TOT + n6574.TOT + nGE75.TOT) %>%
              select(c(POV.LEVEL:nAGE.TOT)) 


POV.125149.AGY <- f.povertyctydat %>%
              summarize(POV.LEVEL = "POV.125149",
              nLE05.TOT  = sum(b17024007),
              n0611.TOT  = sum(b17024020),
              n1217.TOT  = sum(b17024033),
              n1824.TOT  = sum(b17024046),
              n2534.TOT  = sum(b17024059),
              n3544.TOT  = sum(b17024072),
              n4554.TOT  = sum(b17024085),
              n5564.TOT  = sum(b17024098),
              n6574.TOT  = sum(b17024111),
              nGE75.TOT  = sum(b17024124)) %>%
              mutate(nAGE.TOT = nLE05.TOT + n0611.TOT + n1217.TOT + n1824.TOT + n2534.TOT + n3544.TOT + n4554.TOT + n5564.TOT + n6574.TOT + nGE75.TOT) %>%
              select(c(POV.LEVEL:nAGE.TOT)) 


POV.150174.AGY <- f.povertyctydat %>%
              summarize(POV.LEVEL = "POV.150174",
              nLE05.TOT  = sum(b17024008),
              n0611.TOT  = sum(b17024021),
              n1217.TOT  = sum(b17024034),
              n1824.TOT  = sum(b17024047),
              n2534.TOT  = sum(b17024060),
              n3544.TOT  = sum(b17024073),
              n4554.TOT  = sum(b17024086),
              n5564.TOT  = sum(b17024099),
              n6574.TOT  = sum(b17024112),
              nGE75.TOT  = sum(b17024125)) %>%
              mutate(nAGE.TOT = nLE05.TOT + n0611.TOT + n1217.TOT + n1824.TOT + n2534.TOT + n3544.TOT + n4554.TOT + n5564.TOT + n6574.TOT + nGE75.TOT) %>%
              select(c(POV.LEVEL:nAGE.TOT)) 


POV.175199.AGY <- f.povertyctydat %>%
              summarize(POV.LEVEL = "POV.175199",
              nLE05.TOT  = sum(b17024009 + b17024010),
              n0611.TOT  = sum(b17024022 + b17024023),
              n1217.TOT  = sum(b17024035 + b17024036),
              n1824.TOT  = sum(b17024048 + b17024049),
              n2534.TOT  = sum(b17024061 + b17024062),
              n3544.TOT  = sum(b17024074 + b17024075),
              n4554.TOT  = sum(b17024087 + b17024088),
              n5564.TOT  = sum(b17024100 + b17024101),
              n6574.TOT  = sum(b17024113 + b17024114),
              nGE75.TOT  = sum(b17024126 + b17024127)) %>%
              mutate(nAGE.TOT = nLE05.TOT + n0611.TOT + n1217.TOT + n1824.TOT + n2534.TOT + n3544.TOT + n4554.TOT + n5564.TOT + n6574.TOT + nGE75.TOT) %>%
              select(c(POV.LEVEL:nAGE.TOT)) 



POV.200299.AGY <- f.povertyctydat %>%
              summarize(POV.LEVEL = "POV.200299",
              nLE05.TOT  = sum(b17024011),
              n0611.TOT  = sum(b17024024),
              n1217.TOT  = sum(b17024037),
              n1824.TOT  = sum(b17024050),
              n2534.TOT  = sum(b17024063),
              n3544.TOT  = sum(b17024076),
              n4554.TOT  = sum(b17024089),
              n5564.TOT  = sum(b17024102),
              n6574.TOT  = sum(b17024115),
              nGE75.TOT  = sum(b17024128)) %>%
              mutate(nAGE.TOT = nLE05.TOT + n0611.TOT + n1217.TOT + n1824.TOT + n2534.TOT + n3544.TOT + n4554.TOT + n5564.TOT + n6574.TOT + nGE75.TOT) %>%
              select(c(POV.LEVEL:nAGE.TOT)) 


POV.GE300.AGY <- f.povertyctydat %>%
            summarize(POV.LEVEL = "POV.GE300",
            nLE05.TOT  = sum(b17024012 + b17024013 + b17024014),
            n0611.TOT  = sum(b17024025 + b17024026 + b17024027),
            n1217.TOT  = sum(b17024038 + b17024039 + b17024040),
            n1824.TOT  = sum(b17024051 + b17024052 + b17024053),
            n2534.TOT  = sum(b17024064 + b17024065 + b17024066),
            n3544.TOT  = sum(b17024077 + b17024078 + b17024079),
            n4554.TOT  = sum(b17024090 + b17024091 + b17024092),
            n5564.TOT  = sum(b17024103 + b17024104 + b17024105),
            n6574.TOT  = sum(b17024116 + b17024117 + b17024118),
            nGE75.TOT  = sum(b17024129 + b17024130 + b17024131)) %>%
            mutate(nAGE.TOT = nLE05.TOT + n0611.TOT + n1217.TOT + n1824.TOT + n2534.TOT + n3544.TOT + n4554.TOT + n5564.TOT + n6574.TOT + nGE75.TOT) %>%
            select(c(POV.LEVEL:nAGE.TOT)) 




f.povertyagyVAL2 <- bind_rows(list(POV.LT50.AGY, POV.5074.AGY,	POV.7599.AGY,	POV.100124.AGY, POV.125149.AGY,	
                                   POV.150174.AGY,	POV.175199.AGY,	POV.200299.AGY,	POV.GE300.AGY)) 
# Adding Colsums
f.povertyagysum <- f.povertyagyVAL2 %>% 
              summarize_if(is.numeric,sum)

                   

f.povertyagyVAL3  <- bind_rows(f.povertyagyVAL2,f.povertyagysum) %>%
                   mutate(geoname = listID$plName1, 
                          county = "1000") %>%
                   arrange(county) %>%
                   group_by(geoname, county, POV.LEVEL) %>%
                   mutate(nLE05.PCT	 =	nLE05.TOT/nAGE.TOT,
                          n0611.PCT	 =	n0611.TOT/nAGE.TOT,
                          n1217.PCT	 =	n1217.TOT/nAGE.TOT,
                          n1824.PCT	 =	n1824.TOT/nAGE.TOT,
                          n2534.PCT	 =	n2534.TOT/nAGE.TOT,
                          n3544.PCT	 =	n3544.TOT/nAGE.TOT,
                          n4554.PCT	 =	n4554.TOT/nAGE.TOT,
                          n5564.PCT	 =	n5564.TOT/nAGE.TOT,
                          n6574.PCT	 =	n6574.TOT/nAGE.TOT,
                          nGE75.PCT	 =	nGE75.TOT/nAGE.TOT,
                          nAGE.PCT	 =	nAGE.TOT/nAGE.TOT)


f.povertyagyVAL3 <- f.povertyagyVAL3[,c(13,14,1:12,15:25)]
f.povertyagyVAL3$POV.LEVEL <- ifelse(is.na(f.povertyagyVAL3$POV.LEVEL),"Total",f.povertyagyVAL3$POV.LEVEL)
f.povertyctyVAL3 <- bind_rows(f.povertyagyVAL3, f.povertyctyVAL3)  

}
# Creating count and percentage files

f.povertycty_C <- f.povertyctyVAL3[,1:14]
f.povertycty_P <-  f.povertyctyVAL3[,c(1:3,15:25)]

 f.povertycty_C$type <- "Count"
 f.povertycty_C[,4:14] <- sapply(f.povertycty_C[,4:14],NumFmt)
 f.povertycty_C <- f.povertycty_C[,c(1:3,15,4:14)]
 names(f.povertycty_C) <- c("geoname","county","POV.LEVEL","type","nLE05","n0611","n1217",
            "n1824", "n2534","n3544","n4454","n5564","n6574","nGE75","nAGE")

 f.povertycty_P$type <- "Percentage"

 f.povertycty_P[,4:14] <- lapply(f.povertycty_P[,4:14], function(x) x * 100)
 f.povertycty_P[,4:14] <- sapply(f.povertycty_P[,4:14],percent)
 f.povertycty_P <- f.povertycty_P[,c(1:3,15,4:14)]
 names(f.povertycty_P) <- c("geoname","county","POV.LEVEL","type","nLE05","n0611","n1217",
            "n1824", "n2534","n3544","n4454","n5564","n6574","nGE75","nAGE")
 
 f.povertycty_tab <- bind_rows( f.povertycty_C, f.povertycty_P) %>% arrange(county, desc(type))
 
 f.povertycty_tab$POV.LEVEL <- plyr::revalue(f.povertycty_tab$POV.LEVEL,
                                c("POV.LT50" = "Less than 50%","POV.5074" = "50 to 74%",
                                  "POV.7599" = "75 to 99%", "POV.100124" = "100 to 124%", 
                                  "POV.125149" = "125 to 149%", "POV.150174" = "150 to 174%",
                                  "POV.175199" = "175 to 199%", "POV.200299" = "200 to 299%",
                                  "POV.GE300" = "300% and Higher"))
 
 f.povertycty_tab <- f.povertycty_tab %>%
      arrange(county, match(POV.LEVEL,c("Less than 50%", "50 to 74%",
                                   "75 to 99%", "100 to 124%", 
                                   "125 to 149%", "150 to 174%",
                                   "175 to 199%", "200 to 299%",
                                   "300% and Higher","Total"), desc(type)))
 
 f.povertycty_tab <- f.povertycty_tab[,c(1,3:15)]
 f.povertycty_tab <- clrGeoname(f.povertycty_tab,ctyfips,20)
 f.povertycty_tab$POV.LEVEL <- ifelse(f.povertycty_tab$type == "Count","",f.povertycty_tab$POV.LEVEL)
 
 names(f.povertycty_tab) <- c("Agency/County","Poverty Level","Value","Under 6","6 to 11","12 to 17",
                               "18 to 24","25 to 34","35 to 44","45 to 54","55 to 64","65 to 74",
                               "75 and Older","Total")
 
 
# Generate Flextable
# Flex Table

  tab_head <- paste0("Table 5: Population by Percentage of Federal Poverty Level and Age, ",listID$plName1)
  
    f.povFlex <- flextable(
      f.povertycty_tab,
       col_keys = names(f.povertycty_tab)) %>%
       add_header_row(values=tab_head,top=TRUE,colwidths=14) %>%
       add_footer_row(values=captionSrc("ACS",ACS,"B17024"),top=FALSE,colwidths=14) %>%
       align(j=1:2, align="left", part="body") 

POVPlot <- list()

}  # tabtype == 5
  
 if(tabtype == 6) {

 # Creating first year table
   f.povertyctyVAL <- f.povertyctydat %>%
            group_by(geoname, county) %>% 
            mutate( ACS = ACS,
              TOT.POP.TOT = b17024001,
             age0017.TOT.TOT = sum(b17024002, b17024015, b17024028),
             age0017.FPL.TOT = sum(b17024003, b17024004, b17024005, 
                               b17024016, b17024017, b17024018, 
                               b17024029, b17024030, b17024031),
             age1854.TOT.TOT = sum(b17024041, b17024054, b17024067, b17024080),
             age1854.FPL.TOT = sum(b17024042, b17024043, b17024044, b17024055,
                               b17024056, b17024057, b17024068, b17024069,
                               b17024070, b17024081, b17024082, b17024083),
             ageGE55.TOT.TOT = sum(b17024093, b17024106, b17024119),
             ageGE55.FPL.TOT = sum(b17024094, b17024095, b17024096,
                               b17024107, b17024108, b17024109,
                               b17024120, b17024121, b17024122),
             age0017.FPL.PCT = age0017.FPL.TOT/age0017.TOT.TOT,
             age1854.FPL.PCT = age1854.FPL.TOT/age1854.TOT.TOT,
             ageGE55.FPL.PCT = ageGE55.FPL.TOT/ageGE55.TOT.TOT) 
     
    f.povertyctyVAL <- f.povertyctyVAL[,c("geoname", "county", "ACS", "TOT.POP.TOT",
                                        "age0017.TOT.TOT", "age0017.FPL.TOT", "age0017.FPL.PCT",
                                        "age1854.TOT.TOT", "age1854.FPL.TOT", "age1854.FPL.PCT",
                                        "ageGE55.TOT.TOT", "ageGE55.FPL.TOT", "ageGE55.FPL.PCT")] 
   
   
   
if(length(ctyfips) > 1) {
     f.povertyagyVAL <- f.povertyctydat %>%
           summarize( ACS = ACS,
             TOT.POP.TOT = sum(b17024001),
             age0017.TOT.TOT = sum(b17024002 + b17024015 + b17024028),
             age0017.FPL.TOT = sum(b17024003 + b17024004 + b17024005 + 
                               b17024016 + b17024017 + b17024018 + 
                               b17024029 + b17024030 + b17024031),
             age1854.TOT.TOT = sum(b17024041 + b17024054 + b17024067 + b17024080),
             age1854.FPL.TOT = sum(b17024042 + b17024043 + b17024044 + b17024055,
                               b17024056 + b17024057 + b17024068 + b17024069,
                               b17024070 + b17024081 + b17024082 + b17024083),
             ageGE55.TOT.TOT = sum(b17024093 + b17024106 + b17024119),
             ageGE55.FPL.TOT = sum(b17024094 + b17024095 + b17024096,
                               b17024107 + b17024108 + b17024109,
                               b17024120 + b17024121 +b17024122)) %>%
             mutate( age0017.FPL.PCT = age0017.FPL.TOT/age0017.TOT.TOT,
                     age1854.FPL.PCT = age1854.FPL.TOT/age1854.TOT.TOT,
                     ageGE55.FPL.PCT = ageGE55.FPL.TOT/ageGE55.TOT.TOT) %>%
             select(ACS:ageGE55.FPL.PCT)
     
         f.povertyagyVAL$geoname <- listID$plName1  
         f.povertyagyVAL$county <- "1000"

  f.povertyagyVAL <- f.povertyagyVAL[,c("geoname", "county", "ACS", "TOT.POP.TOT",
                                        "age0017.TOT.TOT", "age0017.FPL.TOT", "age0017.FPL.PCT",
                                        "age1854.TOT.TOT", "age1854.FPL.TOT", "age1854.FPL.PCT",
                                        "ageGE55.TOT.TOT", "ageGE55.FPL.TOT", "ageGE55.FPL.PCT")]  
   
   f.povertyctyVAL <- bind_rows(f.povertyagyVAL,f.povertyctyVAL)
     
     
}
   

 # Previous Period data

  f.povertyctyPRE <- codemog_api(data="b17024",db=PreACS,sumlev="50",geography="sumlev",meta="no")
  f.povertyctyPRE[,8:138] <- sapply(f.povertyctyPRE[,8:138],as.numeric)
 
  f.povertyctyPREVAL <- f.povertyctyPRE %>%  filter(county %in% ctyfips) 
    
  f.povertyctyPREVAL$geoname <- sub(", Colorado","", f.povertyctyPREVAL$geoname) 
  
  f.povertyctyVALPRE <- f.povertyctyPREVAL %>%
             group_by(geoname, county) %>% 
            mutate( ACS = PreACS,
              TOT.POP.TOT = b17024001,
             age0017.TOT.TOT = sum(b17024002, b17024015, b17024028),
             age0017.FPL.TOT = sum(b17024003, b17024004, b17024005, 
                               b17024016, b17024017, b17024018, 
                               b17024029, b17024030, b17024031),
             age1854.TOT.TOT = sum(b17024041, b17024054, b17024067, b17024080),
             age1854.FPL.TOT = sum(b17024042, b17024043, b17024044, b17024055,
                               b17024056, b17024057, b17024068, b17024069,
                               b17024070, b17024081, b17024082, b17024083),
             ageGE55.TOT.TOT = sum(b17024093, b17024106, b17024119),
             ageGE55.FPL.TOT = sum(b17024094, b17024095, b17024096,
                               b17024107, b17024108, b17024109,
                               b17024120, b17024121, b17024122),
             age0017.FPL.PCT = age0017.FPL.TOT/age0017.TOT.TOT,
             age1854.FPL.PCT = age1854.FPL.TOT/age1854.TOT.TOT,
             ageGE55.FPL.PCT = ageGE55.FPL.TOT/ageGE55.TOT.TOT) 
     
    f.povertyctyVALPRE <- f.povertyctyVALPRE[,c("geoname", "county", "ACS", "TOT.POP.TOT",
                                        "age0017.TOT.TOT", "age0017.FPL.TOT", "age0017.FPL.PCT",
                                        "age1854.TOT.TOT", "age1854.FPL.TOT", "age1854.FPL.PCT",
                                        "ageGE55.TOT.TOT", "ageGE55.FPL.TOT", "ageGE55.FPL.PCT")]
  
if(length(ctyfips) > 1) {
     f.povertyagyVALPRE <- f.povertyctyPREVAL %>%
            summarize( ACS = PreACS,
             TOT.POP.TOT = sum(b17024001),
             age0017.TOT.TOT = sum(b17024002 + b17024015 + b17024028),
             age0017.FPL.TOT = sum(b17024003 + b17024004 + b17024005 + 
                               b17024016 + b17024017 + b17024018 + 
                               b17024029 + b17024030 + b17024031),
             age1854.TOT.TOT = sum(b17024041 + b17024054 + b17024067 + b17024080),
             age1854.FPL.TOT = sum(b17024042 + b17024043 + b17024044 + b17024055,
                               b17024056 + b17024057 + b17024068 + b17024069,
                               b17024070 + b17024081 + b17024082 + b17024083),
             ageGE55.TOT.TOT = sum(b17024093 + b17024106 + b17024119),
             ageGE55.FPL.TOT = sum(b17024094 + b17024095 + b17024096,
                               b17024107 + b17024108 + b17024109,
                               b17024120 + b17024121 +b17024122)) %>%
             mutate( age0017.FPL.PCT = age0017.FPL.TOT/age0017.TOT.TOT,
                     age1854.FPL.PCT = age1854.FPL.TOT/age1854.TOT.TOT,
                     ageGE55.FPL.PCT = ageGE55.FPL.TOT/ageGE55.TOT.TOT) %>%
             select(ACS:ageGE55.FPL.PCT)
     
         f.povertyagyVALPRE$geoname <- listID$plName1  
         f.povertyagyVALPRE$county <- "1000"

  f.povertyagyVALPRE <- f.povertyagyVALPRE[,c("geoname", "county", "ACS", "TOT.POP.TOT",
                                        "age0017.TOT.TOT", "age0017.FPL.TOT", "age0017.FPL.PCT",
                                        "age1854.TOT.TOT", "age1854.FPL.TOT", "age1854.FPL.PCT",
                                        "ageGE55.TOT.TOT", "ageGE55.FPL.TOT", "ageGE55.FPL.PCT")] 
   
   f.povertyctyVALPRE <- bind_rows(f.povertyagyVALPRE,f.povertyctyVALPRE)  
}


    f.povertycty_t <- bind_rows(f.povertyctyVALPRE, f.povertyctyVAL) %>%
         gather(POV.LEVEL,value,TOT.POP.TOT:ageGE55.FPL.PCT,factor_key=TRUE) %>%
         separate(POV.LEVEL,c("age_cat","scope","measure")) 
   
    f.povtabPRE_TOT <- f.povertycty_t[which(f.povertycty_t$ACS == PreACS & f.povertycty_t$scope == "TOT" & f.povertycty_t$measure == "TOT"),]
    f.povtabPRE_FPL <-  f.povertycty_t[which(f.povertycty_t$ACS == PreACS & f.povertycty_t$scope == "FPL"),]
    
    
    f.povtabCUR_TOT <- f.povertycty_t[which(f.povertycty_t$ACS == ACS & f.povertycty_t$scope == "TOT" & f.povertycty_t$measure == "TOT"),]
    f.povtabCUR_FPL <-  f.povertycty_t[which(f.povertycty_t$ACS == ACS & f.povertycty_t$scope == "FPL"),]
    
# creating Plotly Chart

f.povertycty_PL <- bind_rows(f.povtabPRE_FPL,  f.povtabCUR_FPL) %>% filter(measure == "PCT")
f.povertycty_PL$age_cat <- ifelse(f.povertycty_PL$age_cat == "age0017", "0 to 17",
                                ifelse(f.povertycty_PL$age_cat == "age1854", "18 to 54","55 and Older"))
f.povertycty_PL$age_cat <- factor(f.povertycty_PL$age_cat, 
                                  levels = c("0 to 17", "18 to 54", "55 and Older"))
f.povertycty_PL$ACS <- paste0(substr(f.povertycty_PL$ACS,4,5) ,"-",substr(f.povertycty_PL$ACS,6,7))

   
  
    f.povertycty_PL$indText  <- paste0( f.povertycty_PL$geoname," Period: ",f.povertycty_PL$ACS," Percent of Age Category, ", f.povertycty_PL$age_cat,": ", percent( f.povertycty_PL$value * 100))  
    grTitle <- paste0("Table 6: Percent Below Federal Poverty Level by Age Trend, ",listID$plName1)


if(length(ctyfips) > 1 ){
POVPlot <- f.povertycty_PL %>%
  plot_ly(
    type = 'bar', 
    x = ~age_cat, 
    y = ~value,
    color= ~ACS,
    text = ~indText,
    hoverinfo = 'text',
    transforms = list(
      list(
        type = 'filter',
        target = ~geoname,
        operation = '=',
        value = unique(f.povertycty_PL$geoname)[1]
      )
  )) %>% layout( title=grTitle, yaxis = list(title = 'Percent',tickformat = "%"), xaxis=list(title='Percent Below Federal Poverty Level'),
          showlegend = TRUE,
    updatemenus = list(
      list(
        type = 'dropdown',
        active = 0,
        buttons = list(
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.povertycty_PL$geoname)[1]),
               label = unique(f.povertycty_PL$geoname)[1]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.povertycty_PL$geoname)[2]),
               label = unique(f.povertycty_PL$geoname)[2]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.povertycty_PL$geoname)[3]),
               label = unique(f.povertycty_PL$geoname)[3]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.povertycty_PL$geoname)[4]),
               label = unique(f.povertycty_PL$geoname)[4]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.povertycty_PL$geoname)[5]),
               label = unique(f.povertycty_PL$geoname)[5]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.povertycty_PL$geoname)[6]),
               label = unique(f.povertycty_PL$geoname)[6]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.povertycty_PL$geoname)[6]),
               label = unique(f.povertycty_PL$geoname)[7])
      )
  )))
} else {
   POVPlot <- f.povertycty_PL %>%
     plot_ly(
    type = 'bar', 
    x = ~age_cat, 
    y = ~value,
    color = ~ACS,
    text = ~indText,
    hoverinfo = 'text',
    transforms = list(
      list(
        type = 'filter',
        target = ~geoname,
        operation = '=',
        value = unique(f.povertycty_PL$geoname)[1]
      )
  ))   %>% layout( title=grTitle, yaxis = list(title = 'Percent',tickformat = "%"), xaxis=list(title='Percent Below Federal Poverty Level'),
          showlegend = TRUE)
   
}
    
# Creating Table data file
    #Formatting
    f.povtabPRE_TOT$value <- NumFmt(f.povtabPRE_TOT$value)
    f.povtabPRE_FPL$value <- ifelse(f.povtabPRE_FPL$measure == "TOT", NumFmt(f.povtabPRE_FPL$value),percent(f.povtabPRE_FPL$value*100))
    f.povtabCUR_TOT$value <- NumFmt(f.povtabCUR_TOT$value)
    f.povtabCUR_FPL$value <- ifelse(f.povtabCUR_FPL$measure == "TOT", NumFmt(f.povtabCUR_FPL$value),percent(f.povtabCUR_FPL$value * 100))
    
    f.povtabPRE_FPLW <- spread(f.povtabPRE_FPL,measure,value)
    f.povtabCUR_FPLW <- spread(f.povtabCUR_FPL,measure,value)
    
    names(f.povtabPRE_TOT)[7] <- "valueTOT"
    names(f.povtabCUR_TOT)[7] <- "valueTOT"
  
    f.povtabPRE_FPLW <- inner_join(f.povtabPRE_FPLW,f.povtabPRE_TOT, by=c("county","age_cat")) 
    f.povtabCUR_FPLW <- inner_join(f.povtabCUR_FPLW,f.povtabCUR_TOT, by=c("county","age_cat")) 
 
    f.povertycty_tab <- bind_rows(f.povtabPRE_FPLW,f.povtabCUR_FPLW) %>% arrange(county,ACS.x, age_cat)
    f.povertycty_tab <-f.povertycty_tab[,c(1,3,4,6,7,12)]
    
    f.povertycty_tab <- clrGeoname(f.povertycty_tab,ctyfips,6)
    f.povertycty_tab$age_cat <- ifelse(f.povertycty_tab$age_cat == "age0017", "0 to 17",
                                ifelse(f.povertycty_tab$age_cat == "age1854", "18 to 54","55 and Older"))
    
  names(f.povertycty_tab) <- c("V1","V2","V3","V4","V5","V6")
    
    f.povertycty_tab$V2 <- paste0(substr(f.povertycty_tab$V2,4,5) ,"-",substr(f.povertycty_tab$V2,6,7))

    
 # Flex Table
  tab_head <- paste0("Table 6: Percent Below Federal Poverty Level by Age Trend, ",listID$plName1)
  
  
  f.povFlex <- flextable(
       f.povertycty_tab,
       col_keys = names(f.povertycty_tab)) %>%
       set_header_labels(V1 = "Agency/County", V2= "Period", V3 = "Age Category", V4 = "Percent Below FPL",
                         V5 = "Persons Below FPL", V6 = "Total Persons") %>%
       add_header_row(values=tab_head,top=TRUE,colwidths=6) %>%
       add_footer_row(values=captionSrc("ACS",ACS,"B17024"),top=FALSE,colwidths=6) 
      

  names(f.povertycty_tab) <- c("Agency/County", "Period", "Age Category", "Percent Below FPL",
                         "Persons Below FPL", "Total Persons")  
    
    
} #tabtype = 6
  

  #bind list
  outList <- list("plot"= POVPlot, "data" = f.povertycty_tab, "FlexTable" = f.povFlex)
  
  return(outList)
}