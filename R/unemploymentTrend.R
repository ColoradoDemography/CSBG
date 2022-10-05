#' unemploymentTrend generates tables and ployly charts BLS unemployment data
#'  CSBG Dashboard 11/2019  A. Bickford
#' @param DBPool the DOLA database pool
#' @lvl the selected agenct
#' @param listID is the list of selected county codes
#' @return plotly graphic, data table and data file
#' @export
#' 


unemploymentTrend <- function(lvl,listID,curYr){

  # Collecting List of Counties
 
  ctyfips <- as.numeric(substr(listID$list1,3,5))
  ctyfipsC <- substr(listID$list1,3,5)
 

# reading data series and selecting counties

 f.unempl_raw <- readLines("https://www.bls.gov/web/metro/laucntycur14.txt")

 unemplen <- length(f.unempl_raw) - 12

f.unemploy <- read_delim("https://www.bls.gov/web/metro/laucntycur14.txt", delim="|",  
                         col_names=FALSE, skip=6, n_max=unemplen, trim_ws = TRUE)


names(f.unemploy) <- c("series","stfips","fips","geoname","period","CLF","EMP",
                       "UNEMP","UNEMPRATE")
f.unemploy <- as.data.frame(lapply(f.unemploy,function(x) str_trim(x, side='both')))

f.unemploycty <- f.unemploy %>% 
  filter(stfips == "08" & fips %in% ctyfipsC)

# Converting variables
f.unemploycty$fips <- as.numeric(as.character(f.unemploycty$fips))
f.unemploycty[,c(6:8)] <- sapply(f.unemploycty[,c(6:8)],function(x) gsub("\\,","",x))
f.unemploycty[,c(6:8)] <- sapply(f.unemploycty[,c(6:8)],as.numeric)
f.unemploycty$UNEMPRATE <- as.numeric(as.character(f.unemploycty$UNEMPRATE))/100

f.unemploycty$period <- sub("\\(p\\)","",f.unemploycty$period)
f.unemploycty$monc <- substr(f.unemploycty$period,1,3)
f.unemploycty$monn <- plyr::revalue(f.unemploycty$monc, c("Jan" = 1,
                                                        "Feb" = 2,
                                                        "Mar" = 3,
                                                        "Apr" = 4,
                                                        "May" = 5,
                                                        "Jun" = 6,
                                                        "Jul" = 7,
                                                        "Aug" = 8,
                                                        "Sep" = 9,
                                                        "Oct" = 10,
                                                        "Nov" = 11,
                                                        "Dec" = 12))
f.unemploycty$yr <- as.numeric(substr(f.unemploycty$period,5,6)) + 2000
f.unemploycty$date <- as.Date(ISOdate(year=f.unemploycty$yr, day=1,month=f.unemploycty$monn))

f.unemploycty <- f.unemploycty[,c(3,4,13,6:9)]
f.unemploycty$geoname <- sub(", CO","",f.unemploycty$geoname)

   if(length(ctyfips) > 1){
    f.unemployagy <- f.unemploycty %>%
       group_by(date) %>%
       summarize(CLF = sum(CLF),
                 EMP = sum(EMP),
                 UNEMP =sum(UNEMP)) %>%
       mutate(UNEMPRATE = UNEMP/CLF)
     
    f.unemployagy$fips <- 0
    f.unemployagy$geoname <- listID$plName1
    
    f.unemployagy <- f.unemployagy[,c("fips", "geoname", "date", 
                                     "CLF","EMP","UNEMP","UNEMPRATE")]
    
    f.unemploycty <- bind_rows(f.unemployagy, f.unemploycty)

   }

    
# creating Plotly Chart


    f.unemploycty_PLOT <- f.unemploycty
    f.unemploycty_PLOT$indText  <- paste0(f.unemploycty_PLOT$geoname," Unemployment Rate: ", percent(f.unemploycty_PLOT$UNEMPRATE *100)," Persone Unemployed: ",NumFmt(f.unemploycty_PLOT$UNEMP))  
    grTitle <- paste0("Unemployment Rate, ",listID$plName1)
    outCap <- captionSrc("BLS","","")
    xAxis <- list("tickformat"= "%B, %Y", "tickmode"= "auto", "nticks" = 14,
                  "tick0" = f.unemploycty_PLOT[3,1], title = "Date")
    yAxis <- list(title = 'Percent',tickformat = ".1%")

if(length(ctyfips) > 1 ){
UNEMPPlot <- plot_ly(f.unemploycty_PLOT, 
                   x = ~date, 
                   y = ~UNEMPRATE, 
                   type = 'scatter', mode = 'lines', text = ~indText,  textposition = "none",hoverinfo = 'text',
                   transforms = list(
                      list(
                        type = 'filter',
                        target = ~geoname,
                        operation = '=',
                        value = unique(f.unemploycty_PLOT$geoname)[1]))) %>%
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
                     args = list("transforms[0].value", unique(f.unemploycty_PLOT$geoname)[1]),
                     label = unique(f.unemploycty_PLOT$geoname)[1]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.unemploycty_PLOT$geoname)[2]),
                     label = unique(f.unemploycty_PLOT$geoname)[2]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.unemploycty_PLOT$geoname)[3]),
                     label = unique(f.unemploycty_PLOT$geoname)[3]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.unemploycty_PLOT$geoname)[4]),
                     label = unique(f.unemploycty_PLOT$geoname)[4]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.unemploycty_PLOT$geoname)[5]),
                     label = unique(f.unemploycty_PLOT$geoname)[5]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.unemploycty_PLOT$geoname)[6]),
                     label = unique(f.unemploycty_PLOT$geoname)[6]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.unemploycty_PLOT$geoname)[7]),
                     label = unique(f.unemploycty_PLOT$geoname)[7]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.unemploycty_PLOT$geoname)[8]),
                     label = unique(f.unemploycty_PLOT$geoname)[8]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.unemploycty_PLOT$geoname)[9]),
                     label = unique(f.unemploycty_PLOT$geoname)[9]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.unemploycty_PLOT$geoname)[10]),
                     label = unique(f.unemploycty_PLOT$geoname)[10]),
                list(method = "restyle",
                     args = list("transforms[0].value", unique(f.unemploycty_PLOT$geoname)[11]),
                     label = unique(f.unemploycty_PLOT$geoname)[11])
            )
        )))
} else {
   UNEMPPlot <- plot_ly(f.unemploycty_PLOT, 
                      x = ~date, y = ~UNEMPRATE, type = 'scatter',
                      mode = 'lines', text = ~indText,  textposition = "none",hoverinfo = 'text') %>%
     layout( title=grTitle, yaxis = yAxis, xaxis=xAxis,
          showlegend = FALSE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCap,
                      font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4))
   
}
    
# Creating Table data file
    f.unemploy_tab <- f.unemploycty_PLOT[,c(2,3,7,6)] 
    f.unemploy_tab$date <- format(f.unemploy_tab$date, "%B, %Y")
    f.unemploy_tab$UNEMP <- NumFmt(f.unemploy_tab$UNEMP)
    f.unemploy_tab$UNEMPRATE <- percent(f.unemploy_tab$UNEMPRATE * 100)
    names(f.unemploy_tab)[3] <- "Unemployment Rate"
    names(f.unemploy_tab)[4] <- "Unemployment"
    
    
    if(length(ctyfips) == 1) {
      npanel1 = 1
    } else {
      npanel1 <- length(ctyfips) + 1
    }
    
    f.unemploy_tab <- clrGeoname(f.unemploy_tab,"geoname",npanel1,14)
   
  
    
    

 # Flex Table
  tab_head <- paste0("Unemployment Rate, ",listID$plName1)
  
  
  f.povFlex <- flextable(
      f.unemploy_tab,
       col_keys = names(f.unemploy_tab)) %>%
       set_header_labels(geoname = "Agency/County", date= "Month") %>%
       add_header_row(values=tab_head,top=TRUE,colwidths=4) %>%
       add_footer_row(values=captionSrc("BLS","",""),top=FALSE,colwidths=4) %>%
       align(j=1:4, align="center",part="header") %>%
       align(j=1:2, align="left", part="body") %>%
       align(j=3:4, align="right", part="body") %>%
       align(j=1, align="left", part="footer") %>%
       width(j=1, width=3) %>%
       width(j=2, width=2) %>%
       width(j=3:4,width=1.25) %>%
       height(part="footer", height=0.4) %>%
       height(part="body", height=0.5)
      

  names(f.unemploy_tab)[1] <- "Agency/County"
  names(f.unemploy_tab)[2] <- "Month"
 
    
    
  #bind list
  outList <- list("plot"= UNEMPPlot, "data" = f.unemploycty,"table" = f.unemploy_tab, "FlexTable" = f.povFlex, "caption" = outCap)
  
  return(outList)
}