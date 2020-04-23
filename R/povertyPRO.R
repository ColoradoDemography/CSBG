#' povertyPRO Creates a materials showing ppopulation by federal poverty level
#'
# 'Produces plotly plot, data and table for selected CSBG agencies 10/2019
#'
#' @param lvl is the data type ("Regional Summary" or "Counties)
#' @param listID is the list of selected county codes
#' @param ACS  Specify the vintage of the ACS 5-year file 
#' @return plotly graphic, data table and data file
#' @export
povertyPRO <- function(lvl,listID, ACS,PreACS,curYr){

  # Collecting List of Counties
   outCap <- captionSrc("ACS",ACS,"B17024") 

 f.povertycty <- codemog_api(data="b17024",db=ACS,sumlev="50",geography="sumlev",meta="no")
 f.povertycty[,c(3,8:138)] <- sapply(f.povertycty[,c(3,8:138)],as.numeric)
 
  ctyfips <- as.character(as.numeric(substr(listID$list1,3,5)))
  
  f.povertyctydat <- f.povertycty %>%  filter(county %in% ctyfips) 
    
  f.povertyctydat$geoname <- sub(", Colorado","", f.povertyctydat$geoname) 
  state="08"
 
      f.povertyctyVAL <- f.povertyctydat %>%
            group_by(county) %>%
           mutate(
              TOT.POP = b17024001,
              POV.LT50 = sum(b17024003, b17024016, b17024029, b17024042, b17024055, b17024068, 
                             b17024081, b17024094, b17024107, b17024120),
              POV.50124 = sum(b17024004, b17024017, b17024030, b17024043, b17024056, b17024069, 
                             b17024082, b17024095, b17024108, b17024121,
                             b17024005, b17024018, b17024031, b17024044, b17024057, b17024070, 
                             b17024083, b17024096, b17024109, b17024122,
                             b17024006, b17024019, b17024032, b17024045, b17024058, b17024071, 
                              b17024084, b17024097, b17024110, b17024123),
              POV.125199 = sum(b17024007, b17024020, b17024033, b17024046, b17024059, b17024072, 
                               b17024085, b17024098, b17024111, b17024124,
                               b17024008, b17024021, b17024034, b17024047, b17024060, b17024073, 
                               b17024086, b17024099, b17024112, b17024125,
                               b17024009, b17024022, b17024035, b17024048, b17024061, b17024074, 
                               b17024087, b17024100, b17024113, b17024126, b17024010, b17024023, 
                               b17024036, b17024049, b17024062, b17024075, b17024088, b17024101, b17024114, b17024127),
              POV.GE200 = sum(b17024011, b17024024, b17024037, b17024050, b17024063, b17024076, 
                               b17024089, b17024102, b17024115, b17024128,
                              b17024012, b17024025, b17024038, b17024051, b17024064, b17024077, 
                              b17024090, b17024103, b17024116, b17024129, b17024013, b17024026, 
                              b17024039, b17024052, b17024065, b17024078, b17024091, b17024104, 
                              b17024117, b17024130, b17024014, b17024027, b17024040, b17024053, 
                              b17024066, b17024079, b17024092, b17024105, b17024118, b17024131),
              POV.LT50.PCT = POV.LT50/TOT.POP, 
              POV.50124.PCT = POV.50124/TOT.POP, 
              POV.125199.PCT = POV.125199/TOT.POP, 
              POV.GE200.PCT = POV.GE200/TOT.POP, 
              TOT.POP.PCT = 1)
          

   f.povertyctyVAL <- f.povertyctyVAL[,c("geoname", "county", "POV.LT50", "POV.50124", 
                                    "POV.125199", "POV.GE200",  "TOT.POP",
                                    "POV.LT50.PCT", "POV.50124.PCT", "POV.125199.PCT",
                                    "POV.GE200.PCT", "TOT.POP.PCT")]               

 if(length(ctyfips) > 1){
      f.povertyagyVAL <- f.povertyctydat %>%
            summarize(
              TOT.POP = sum(b17024001),
              POV.LT50 = sum(b17024003, b17024016, b17024029, b17024042, b17024055, b17024068, 
                             b17024081, b17024094, b17024107, b17024120),
              POV.50124 = sum(b17024004, b17024017, b17024030, b17024043, b17024056, b17024069, 
                             b17024082, b17024095, b17024108, b17024121,
                             b17024005, b17024018, b17024031, b17024044, b17024057, b17024070, 
                             b17024083, b17024096, b17024109, b17024122,
                             b17024006, b17024019, b17024032, b17024045, b17024058, b17024071, 
                               b17024084, b17024097, b17024110, b17024123),
              POV.125199 = sum(b17024007, b17024020, b17024033, b17024046, b17024059, b17024072, 
                               b17024085, b17024098, b17024111, b17024124,
                               b17024008, b17024021, b17024034, b17024047, b17024060, b17024073, 
                               b17024086, b17024099, b17024112, b17024125,
                               b17024009, b17024022, b17024035, b17024048, b17024061, b17024074, 
                               b17024087, b17024100, b17024113, b17024126, b17024010, b17024023, 
                               b17024036, b17024049, b17024062, b17024075, b17024088, b17024101, b17024114, b17024127),
              POV.GE200 = sum(b17024011, b17024024, b17024037, b17024050, b17024063, b17024076, 
                              b17024089, b17024102, b17024115, b17024128,
                              b17024012, b17024025, b17024038, b17024051, b17024064, b17024077, 
                              b17024090, b17024103, b17024116, b17024129, b17024013, b17024026, 
                              b17024039, b17024052, b17024065, b17024078, b17024091, b17024104, 
                              b17024117, b17024130, b17024014, b17024027, b17024040, b17024053, 
                              b17024066, b17024079, b17024092, b17024105, b17024118, b17024131) ) %>%
            mutate( POV.LT50.PCT = POV.LT50/TOT.POP, 
              POV.50124.PCT = POV.50124/TOT.POP, 
              POV.125199.PCT = POV.125199/TOT.POP, 
              POV.GE200.PCT = POV.GE200/TOT.POP,
              TOT.POP.PCT = 1)
     
  f.povertyagyVAL$geoname <- listID$plName1  
  f.povertyagyVAL$county <- 0

 f.povertyagyVAL <- f.povertyagyVAL[,c("geoname", "county", "POV.LT50", "POV.50124", 
                                    "POV.125199", "POV.GE200",  "TOT.POP",
                                    "POV.LT50.PCT", "POV.50124.PCT", "POV.125199.PCT",
                                    "POV.GE200.PCT", "TOT.POP.PCT")]     
   
   f.povertyctyVAL <- bind_rows(f.povertyagyVAL,f.povertyctyVAL)
 } 

 
 f.povertycty_C <-  f.povertyctyVAL[,1:7] 

 f.povertycty_CL <- f.povertycty_C %>% 
        gather(POV.LEVEL,count,POV.LT50:TOT.POP,factor_key=TRUE) %>%
        arrange(POV.LEVEL)

 f.povertycty_CL <- f.povertycty_CL[which(f.povertycty_CL$POV.LEVEL != "TOT.POP"),] %>% arrange(county, POV.LEVEL)

 f.povertycty_CL$POV.LEVEL <- plyr::revalue(f.povertycty_CL$POV.LEVEL,
                                c("POV.LT50" = "Less than 50%","POV.50124" = "50 to 124%",
                                  "POV.125199" = "125 to 199%", "POV.GE200" = "200% and Higher"))
 f.povertycty_CL$POV.LEVEL <- factor(f.povertycty_CL$POV.LEVEL, 
                                      levels = c("Less than 50%", "50 to 124%",
                                   "125 to 199%", "200% and Higher"))

f.povertycty_P <- f.povertyctyVAL[,c(1,2,8:12)] 

f.povertycty_PL <- f.povertycty_P %>% 
        gather(POV.LEVEL,value,POV.LT50.PCT:TOT.POP.PCT,factor_key=TRUE) %>%
        arrange(POV.LEVEL)

f.povertycty_PL <- f.povertycty_PL[which(f.povertycty_PL$POV.LEVEL != "TOT.POP.PCT"),] %>% arrange(county, POV.LEVEL)

f.povertycty_PL$POV.LEVEL <- plyr::revalue(f.povertycty_PL$POV.LEVEL,
                                c("POV.LT50.PCT" = "Less than 50%","POV.50124.PCT" = "50 to 124%",
                                  "POV.125199.PCT" = "125 to 199%", "POV.GE200.PCT" = "200% and Higher"))
 f.povertycty_PL$POV.LEVEL <- factor(f.povertycty_PL$POV.LEVEL, 
                                      levels = c("Less than 50%", "50 to 124%",
                                   "125 to 199%", "200% and Higher"))

 # Plotly plot
  f.povertycty_PLOT <- inner_join(f.povertycty_PL, f.povertycty_CL[,2:4], by= c("county","POV.LEVEL"))
  f.povertycty_PLOT$indText  <- paste0( f.povertycty_PLOT$geoname," Percent of Federal Poverty Level, ", f.povertycty_PLOT$POV.LEVEL,": Percentage: ", percent(f.povertycty_PLOT$value * 100)," Count: ",NumFmt(f.povertycty_PLOT$count))  
  grTitle <- paste0("Population by Percentage of Federal Poverty Level, ",listID$plName1)
  xAxis <- list(title='Percentage of Federal Poverty Level')
  yAxis <- list(title = 'Percent',tickformat = "%")
  
if(length(ctyfips) > 1 ){
POVPlot <- f.povertycty_PLOT %>%
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
  )) %>% layout( title=grTitle, yaxis = yAxis, xaxis=xAxis,
          showlegend = FALSE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCap,
                      font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4),
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
               args = list("transforms[0].value", unique(f.povertycty_PL$geoname)[7]),
               label = unique(f.povertycty_PL$geoname)[7])
      )
  )))
} else {
   POVPlot <- f.povertycty_PLOT %>%
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
  ))   %>% layout( title=grTitle, yaxis = yAxis, xaxis=xAxis,
          showlegend = FALSE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCap,
                      font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4))
   
}


 #Creating Table data file

 f.povertycty_C$type <- "Count"
 f.povertycty_C <- f.povertycty_C[,c(1,2,8,3:7)]
 if(typeof(f.povertycty_C) == "list") {
   f.povertycty_C <- as.data.frame(f.povertycty_C)
 }
 f.povertycty_C[,4:8] <- sapply(f.povertycty_C[,4:8],NumFmt)


 f.povertycty_P$type <- "Percentage"
 f.povertycty_P <- f.povertycty_P[,c(1,2,8,3:7)]
 if(typeof(f.povertycty_P) == "list") {
   f.povertycty_P <- as.data.frame(f.povertycty_P)
 }
 
 f.povertycty_P[,4:8] <- sapply(f.povertycty_P[,4:8], function(x) percent(x * 100))
 
 
 names(f.povertycty_P) <- c("geoname", "county", "type", "POV.LT50", "POV.50124", 
                                    "POV.125199", "POV.GE200",  "TOT.POP")
 
 f.povertycty_tab <- bind_rows(mutate_all(f.povertycty_C,as.character),mutate_all(f.povertycty_P,as.character)) %>% arrange(county,desc(type))
 
 
  #Clearing geoname
    if(length(ctyfips) == 1) {
      npanel1 <- 1
    } else {
      npanel1 = length(ctyfips) + 1
    }
 
f.povertycty_tab <- clrGeoname( f.povertycty_tab,"geoname",npanel1,2)
f.povertycty_tab <- f.povertycty_tab[,c(1,3:8)]

names(f.povertycty_tab) <- c("Agency/County","Value","Less than 50%", "50 to 124%",
                                   "125 to 199%", "200% and Higher","Total")


# Flex Table
  tab_head <- paste0("Population by Percentage of Federal Poverty Level, ",listID$plName1)

    f.povFlex <- flextable(
      f.povertycty_tab,
       col_keys = names(f.povertycty_tab)) %>%
       add_header_row(values=tab_head,top=TRUE,colwidths=7) %>%
       add_footer_row(values=outCap,top=FALSE,colwidths=7) %>%
       align(j=1:2, align="left", part="body") %>%
       width(j= 1, width=3) %>%
       width(j=2:7,width=0.75) %>%
       height(part="footer", height=0.4) %>%
      height(part="header", i=2,height=0.7)



  #bind list
  outList <- list("plot"= POVPlot, "data" =  f.povertycty_PLOT, "table" = f.povertycty_tab, "FlexTable" = f.povFlex,"caption" = outCap)
  
  return(outList)
}