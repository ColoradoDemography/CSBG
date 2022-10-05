#' insurance generates tables and plotly charts for Health insurance by age 
#' Using SAIHE data https://www.census.gov/programs-surveys/sahie.html

#'  CSBG Dashboard 11/2019  Revised 9/2022 A. Bickford
#' @param DBPool the DOLA database pool
#' @lvl the selected agency
#' @param listID is the list of selected county codes
#' @param cyrYr Current year value
#' @return plotly graphic, data table and data file
#' @export

insurance <- function(ACS,lvl,listID,curYR){

  # Collecting List of Counties

  ctyfips <- str_sub(listID$list1,3,5)
  f.ctyNames <- data.frame(str_sub(listID$list2,3,5),listID$plName2)
  names(f.ctyNames) <- c("fips", "NAME")
  
# Extracting ins data
  APIurl <- paste0("https://api.census.gov/data/timeseries/healthins/sahie?get=",
                   "NAME,NIPR_PT,NIC_PT,NUI_PT",
                   "&for=county:*&in=state:08&SEXCAT=0&YEAR=", curYr, 
                   "&AGECAT=0,1&IPRCAT=0,1&key=",censAPI)
  
  res = GET(APIurl)
  f.data <- as.data.frame(fromJSON(rawToChar(res$content)))
  names(f.data) <- f.data[1,]
  
  f.instotcty <-  f.data %>% filter(county %in% ctyfips) %>%
    select(county, AGECAT,IPRCAT,NIPR_PT,NIC_PT,NUI_PT) 
  f.instotcty[,2:6]  <- sapply(f.instotcty[,2:6], as.numeric )

  f.instotcty <- f.instotcty %>%
    pivot_wider(names_from=c(AGECAT, IPRCAT), values_from = c(NIPR_PT,NIC_PT,NUI_PT)) %>%
    mutate(fips = county,
           TOTAL = NIPR_PT_0_0,
           TOTAL_INSURED = NIC_PT_0_0,
           TOTAL_UNINSURED = NUI_PT_0_0,
           TOTAL_POV = NIPR_PT_0_1,
           TOTAL_INSURED_POV = NIC_PT_0_1,
           TOTAL_UNINSURED_POV = NUI_PT_0_1,
           AGE_LT18_TOT = NIPR_PT_0_0 - NIPR_PT_1_0,
           AGE_LT18_INSURED = NIC_PT_0_0 - NIC_PT_1_0,
           AGE_LT18_UNINSURED = NUI_PT_0_0 - NUI_PT_1_0,
           AGE_LT18_TOT_POV = NIPR_PT_0_1 - NIPR_PT_1_1,
           AGE_LT18_INSURED_POV = NIC_PT_0_1 - NIC_PT_1_1,
           AGE_LT18_UNINSURED_POV = NUI_PT_0_1 - NUI_PT_1_1,
           AGE_1864_TOT = NIPR_PT_1_0,
           AGE_1864_INSURED = NIC_PT_1_0,
           AGE_1864_UNINSURED =  NUI_PT_1_0,
           AGE_1864_TOT_POV =  NIPR_PT_1_1,
           AGE_1864_INSURED_POV =  NIC_PT_1_1,
           AGE_1864_UNINSURED_POV =  NUI_PT_1_1) %>%
    inner_join(., f.ctyNames, by="fips") %>%
    mutate(fips = paste0("08",fips)) %>%
    select(fips, NAME, TOTAL : AGE_1864_UNINSURED_POV)  
  
  if(length(ctyfips) > 1){
    f.instotagy <- f.instotcty %>%
      summarize(
        TOTAL = sum(TOTAL, na.rm=TRUE),
        TOTAL_INSURED = sum(TOTAL_INSURED, na.rm=TRUE),
        TOTAL_UNINSURED = sum(TOTAL_UNINSURED, na.rm=TRUE),
        TOTAL_POV = sum(TOTAL_POV, na.rm=TRUE),
        TOTAL_INSURED_POV = sum(TOTAL_INSURED_POV, na.rm=TRUE),
        TOTAL_UNINSURED_POV = sum(TOTAL_UNINSURED_POV, na.rm=TRUE),
        AGE_LT18_TOT = sum(AGE_LT18_TOT, na.rm=TRUE),
        AGE_LT18_INSURED = sum(AGE_LT18_INSURED, na.rm=TRUE),
        AGE_LT18_UNINSURED = sum(AGE_LT18_UNINSURED, na.rm=TRUE),
        AGE_LT18_TOT_POV = sum(AGE_LT18_TOT_POV, na.rm=TRUE),
        AGE_LT18_INSURED_POV = sum(AGE_LT18_INSURED_POV, na.rm=TRUE),
        AGE_LT18_UNINSURED_POV = sum(AGE_LT18_UNINSURED_POV, na.rm=TRUE),
        AGE_1864_TOT = sum(AGE_1864_TOT, na.rm=TRUE),
        AGE_1864_INSURED = sum(AGE_1864_INSURED, na.rm=TRUE),
        AGE_1864_UNINSURED = sum(AGE_1864_UNINSURED, na.rm=TRUE),
        AGE_1864_TOT_POV = sum(AGE_1864_TOT_POV, na.rm=TRUE),
        AGE_1864_INSURED_POV = sum(AGE_1864_INSURED_POV, na.rm=TRUE),
        AGE_1864_UNINSURED_POV = sum(AGE_1864_UNINSURED_POV, na.rm=TRUE)
      ) %>%
      mutate(fips = "08000",
             NAME = listID$plName1) %>%
      select(fips, NAME, TOTAL : AGE_1864_UNINSURED_POV)
    
    
    f.instotcty <- bind_rows(f.instotagy, f.instotcty)
  }
    

   f.instotVAL <- f.instotcty %>%
     mutate(TOTAL_INSURED_PCT = TOTAL_INSURED/TOTAL,
            TOTAL_UNINSURED_PCT = TOTAL_UNINSURED/TOTAL,
            TOTAL_INSURED_POV_PCT = TOTAL_INSURED_POV/TOTAL_POV,
            TOTAL_UNINSURED_POV_PCT = TOTAL_UNINSURED_POV/TOTAL_POV,
            AGE_LT18_INSURED_PCT = AGE_LT18_INSURED/AGE_LT18_TOT,
            AGE_LT18_UNINSURED_PCT = AGE_LT18_UNINSURED/AGE_LT18_TOT,
            AGE_LT18_INSURED_POV_PCT = AGE_LT18_INSURED_POV/AGE_LT18_TOT_POV,
            AGE_LT18_UNINSURED_POV_PCT = AGE_LT18_UNINSURED_POV/AGE_LT18_TOT_POV,
            AGE_1864_INSURED_PCT = AGE_1864_INSURED/AGE_1864_TOT,
            AGE_1864_UNINSURED_PCT = AGE_1864_UNINSURED/AGE_1864_TOT,
            AGE_1864_INSURED_POV_PCT = AGE_1864_INSURED_POV/AGE_1864_TOT_POV,
            AGE_1864_UNINSURED_POV_PCT = AGE_1864_UNINSURED_POV/AGE_1864_TOT_POV) 

 
f.instotVAL_cnt <- f.instotVAL %>% select(fips, NAME, TOTAL_INSURED :  AGE_1864_UNINSURED_POV)
f.instotVAL_cnt_plot <- f.instotVAL_cnt %>% 
  gather(INS,COUNT, TOTAL_INSURED :  AGE_1864_UNINSURED_POV, factor_key=TRUE) %>%
  filter(grepl("UNINSURED",INS)) 

f.instotVAL_pct <- f.instotVAL %>% select(fips, NAME, TOTAL_INSURED_PCT :  AGE_1864_UNINSURED_POV_PCT) 

f.instotVAL_pct_plot <- f.instotVAL_pct %>%
  gather(INS,PCT, TOTAL_INSURED_PCT :  AGE_1864_UNINSURED_POV_PCT, factor_key=TRUE) %>%
  filter(grepl("UNINSURED",INS)) %>%
  mutate(INS = str_replace(INS,"_PCT",""),
         AGE = ifelse(grepl("AGE_LT18",INS),"Under 18 years old",
               ifelse(grepl("AGE_1864",INS), "18 to 64 years old","Under 65 years old")),
         POV = ifelse(grepl("POV",INS),"At or below 200% of poverty level","All income levels"))

f.instotVAL_plot <- inner_join(f.instotVAL_pct_plot,f.instotVAL_cnt_plot, by=c("fips", "NAME", "INS"))

# creating Plotly Chart

    f.instotVAL_plot$AGE <- factor(f.instotVAL_plot$AGE, levels = c("Under 18 years old", "18 to 64 years old","Under 65 years old"))
    f.instotVAL_plot$POV<- factor(f.instotVAL_plot$POV, levels = c("All income levels", "At or below 200% of poverty level"))
 
    f.instotVAL_plot$indText  <- paste0(f.instotVAL_plot$NAME," Uninsured: \n",f.instotVAL_plot$AGE,"\n", 
                                            f.instotVAL_plot$POV,"\nPercent: ", percent(f.instotVAL_plot$PCT * 100)," Count: ",NumFmt(f.instotVAL_plot$COUNT))  
    grTitle <- paste0("Percent Uninsured by Age and Poverty Level,\n",listID$plName1," ",curYR)
    outCap <- captionSrc("SAHIE",curYr,"")
    xAxis <- list(title = "Age Group")
    yAxis <- list(title = 'Percent',tickformat = ".1%")


if(length(ctyfips) > 1 ){
  INSPlot <- f.instotVAL_plot %>%
    plot_ly(
      type = 'bar', 
      x = ~AGE, 
      y = ~PCT,
      color= ~POV,
      text = ~indText,
      hoverinfo = 'text',
      transforms = list(
        list(
          type = 'filter',
          target = ~NAME,
          operation = '=',
          value = unique(f.instotVAL_plot$NAME)[1]
        )
      )) %>% layout( title=grTitle, yaxis = yAxis, xaxis=xAxis,
                     showlegend = TRUE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                     legend = list(orientation = "h"),
                     annotations = list(text = outCap,
                                        font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4),
                     updatemenus = list(
                       list(
                         type = 'dropdown',
                         active = 0,
                         buttons = list(
                           list(method = "restyle",
                                args = list("transforms[0].value", unique(f.instotVAL_plot$NAME)[1]),
                                label = unique(f.instotVAL_plot$NAME)[1]),
                           list(method = "restyle",
                                args = list("transforms[0].value", unique(f.instotVAL_plot$NAME)[2]),
                                label = unique(f.instotVAL_plot$NAME)[2]),
                           list(method = "restyle",
                                args = list("transforms[0].value", unique(f.instotVAL_plot$NAME)[3]),
                                label = unique(f.instotVAL_plot$NAME)[3]),
                           list(method = "restyle",
                                args = list("transforms[0].value", unique(f.instotVAL_plot$NAME)[4]),
                                label = unique(f.instotVAL_plot$NAME)[4]),
                           list(method = "restyle",
                                args = list("transforms[0].value", unique(f.instotVAL_plot$NAME)[5]),
                                label = unique(f.instotVAL_plot$NAME)[5]),
                           list(method = "restyle",
                                args = list("transforms[0].value", unique(f.instotVAL_plot$NAME)[6]),
                                label = unique(f.instotVAL_plot$NAME)[6]),
                           list(method = "restyle",
                                args = list("transforms[0].value", unique(f.instotVAL_plot$NAME)[7]),
                                label = unique(f.instotVAL_plot$NAME)[7])
                         )
                       )))
} else {
  INSPlot <- f.instotVAL_plot %>%
    plot_ly(
      type = 'bar', 
      x = ~AGE, 
      y = ~PCT,
      color = ~POV,
      text = ~indText,
      hoverinfo = 'text',
      transforms = list(
        list(
          type = 'filter',
          target = ~NAME,
          operation = '=',
          value = unique(f.instotVAL_plot$NAME)[1]
        )
      ))   %>% layout( title=grTitle, yaxis = yAxis, xaxis=xAxis,
                       showlegend = TRUE, 
                       legend = list(orientation = "h"),
                       hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                       annotations = list(text = outCap,
                                          font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4))
}    
    
    
# Creating Table data file

f.inscount <- f.instotVAL_plot %>% 
              mutate(TYPE = "Count", 
                     VALUE = NumFmt(COUNT))  %>%
              select(fips, NAME, AGE, POV, TYPE,VALUE) %>%
              spread(POV,VALUE)
             
f.inspct <- f.instotVAL_plot %>%   
            mutate(TYPE = "Percent", 
            VALUE = percent(PCT * 100)) %>%
  select(fips, NAME, AGE, POV, TYPE,VALUE) %>%
  spread(POV,VALUE)

f.instotVAL_plot <- f.instotVAL_plot %>%
             mutate(COUNT = NumFmt(COUNT),
                    PCT = percent(PCT * 100)) %>%
            select(fips, NAME, AGE, POV, COUNT, PCT)

f.ins_Tab <- bind_rows(f.inscount, f.inspct) %>% arrange(fips, AGE, desc(TYPE))


#Clearing geoname
if(length(ctyfips) == 1) {
  npanel1 <- 1
} else {
  npanel1 = length(ctyfips) + 1
}
f.ins_Tab $AGE <- as.character(f.ins_Tab$AGE)
f.ins_Tab <- clrGeoname(f.ins_Tab,"fips",npanel1,6)
f.ins_Tab <- clrGeoname(f.ins_Tab,"NAME",npanel1,6)
for(i in 1:nrow(f.ins_Tab)){
  if(i %% 2 == 0){
    f.ins_Tab[i,3] <- ""
  } 
}

f.ins_Tab <- f.ins_Tab %>% select(-fips)

names(f.ins_Tab)[1:3] <- c("Agency/County","Age Group","Value")


# Flex Table
tab_head <- paste0("Uninsured by Age and Poverty Level, ",listID$plName1, " ",curYR)
col_header <- c( "","","", "Poverty Level")

f.insFlex <- flextable(
  f.ins_Tab,  col_keys = names( f.ins_Tab)) %>%
  fontsize(size=10, part='all') %>%
  add_header_row(value=col_header,colwidths= c(1,1,1,2)) %>%
  add_header_row(values=tab_head,colwidths=5) %>%
  add_footer_row(values=outCap,top=FALSE,colwidths=5) %>%
  align(j=1:5, align="center", part="header") %>%
  align(j=1:3, align="left", part="body") %>%
  align(j=4:5, align="right", part="body") %>%
  align(j=1,align="left", part="footer") %>%
  width(j=1, width=.9) %>%
  width(j=2:5,width=1.2) %>%
  height(part="footer", height=0.4) %>%
  height(part="header",i=2,height=0.6)

  #bind list
   outList <- list("plot"= INSPlot, "data" = f.instotVAL_plot , "table" = f.ins_Tab, "FlexTable" = f.insFlex,"caption" = outCap)
  return(outList)
}