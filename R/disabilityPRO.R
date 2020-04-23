#'disabilityPRO Outputs Tables and plots for the Disability by Age and FPL
#'
#'    pulls data from ACS API 
#'
#'    This table does not report MOEs for ACS series, because of the lack of cunsus MOEs...
#'
#' @param listID the list containing place id and Place names
#' @param state is the state that the original fips
#' @param ACS Specifies the ACS data set to be used, reads curACS from Shiny program
#' @return kable formatted  table and data file
#' @export
#'
disabilityPRO <- function(lvl,listID, ACS,curYr) {
  # Collecting place ids from  idList, setting default values


    ctyfips <- as.character(as.numeric(substr(listID$list1,3,5)))
    f.ctyList <- data.frame(geoname = as.character(listID$plName2),
                            county = as.numeric(ctyfips))
    f.ctyList$geoname <- as.character(f.ctyList$geoname)
    
# ctyList for marginal table    
    if(nrow(f.ctyList) > 1) {
      agy <- data.frame(geoname = as.character(listID$plName1), county = 0)
      agy$geoname <- as.character(agy$geoname)
      f.ctyList <- bind_rows(agy,f.ctyList)
    }

  # Extracting data from tidycensus 
  f.ctyDIS <- codemog_api(data="c18130",db=ACS,sumlev="50",geography="sumlev",meta="no")
   
   
   
   f.ctyDIS[,c(3,8:29)] <- sapply(f.ctyDIS[,c(3,8:29)],as.numeric)

   f.ctyDIS_cty <- f.ctyDIS %>%
             filter(county %in% ctyfips) %>%  # FIX THIS
            group_by(county) %>%
           mutate(
                TOT.0017.TOT	 = 	c18130002,
                DIS.0017.TOT	 = 	c18130003,
                DIS.0017.POV	 = 	c18130004,
                DIS.0017.NPOV	 = 	c18130005,
        
                DIS.1864.TOT	 = 	c18130010,
                DIS.1864.POV	 = 	c18130011,
                DIS.1864.NPOV	 = 	c18130012,
                
                DIS.GE65.TOT	 = 	c18130017,
                DIS.GE65.POV	 = 	c18130018,
                DIS.GE65.NPOV	 = 	c18130019,
                
               
               DIS.0017.POV.PCT	 = 	DIS.0017.POV/DIS.0017.TOT,
               DIS.0017.NPOV.PCT	 = 	DIS.0017.NPOV/DIS.0017.TOT,
               DIS.0017.TOT.PCT  = DIS.0017.TOT/DIS.0017.TOT,
              
               DIS.1864.POV.PCT	 = 	DIS.1864.POV/DIS.1864.TOT,
               DIS.1864.NPOV.PCT	 = 	DIS.1864.NPOV/DIS.1864.TOT,
               DIS.1864.TOT.PCT  = DIS.1864.TOT/DIS.1864.TOT,
               
               DIS.GE65.POV.PCT	 = 	DIS.GE65.POV/DIS.GE65.TOT,
               DIS.GE65.NPOV.PCT	 = 	DIS.GE65.NPOV/DIS.GE65.TOT,
               DIS.GE65.TOT.PCT  = DIS.GE65.TOT/DIS.GE65.TOT)
               
   
   f.ctyDIS_cty <- f.ctyDIS_cty[,c("geoname",	"county",				
                                    "DIS.0017.TOT", "DIS.0017.POV", "DIS.0017.NPOV", 
                                    "DIS.1864.TOT", "DIS.1864.POV", "DIS.1864.NPOV",
                                    "DIS.GE65.TOT", "DIS.GE65.POV", "DIS.GE65.NPOV", 
                                   "DIS.0017.POV.PCT", "DIS.0017.NPOV.PCT","DIS.0017.TOT.PCT",
                                   "DIS.1864.POV.PCT", "DIS.1864.NPOV.PCT", "DIS.1864.TOT.PCT",
                                   "DIS.GE65.POV.PCT",  "DIS.GE65.NPOV.PCT",  "DIS.GE65.TOT.PCT"
                                   )]
 
if(length(ctyfips) > 1) {
     f.ctyDIS_agy <- f.ctyDIS %>%
             filter(county %in% ctyfips) %>%
          summarize(
                  DIS.0017.TOT	 = 	sum(c18130003),
                  DIS.0017.POV	 = 	sum(c18130004),
                  DIS.0017.NPOV	 = 	sum(c18130005),
                  
                  DIS.1864.TOT	 = 	sum(c18130010),
                  DIS.1864.POV	 = 	sum(c18130011),
                  DIS.1864.NPOV	 = 	sum(c18130012),
                  
                  DIS.GE65.TOT	 = 	sum(c18130017),
                  DIS.GE65.POV	 = 	sum(c18130018),
                  DIS.GE65.NPOV	 = 	sum(c18130019)) %>%
         mutate(
               DIS.0017.POV.PCT	 = 	DIS.0017.POV/DIS.0017.TOT,
               DIS.0017.NPOV.PCT	 = 	DIS.0017.NPOV/DIS.0017.TOT,
               DIS.0017.TOT.PCT  = DIS.0017.TOT/DIS.0017.TOT,
              
               DIS.1864.POV.PCT	 = 	DIS.1864.POV/DIS.1864.TOT,
               DIS.1864.NPOV.PCT	 = 	DIS.1864.NPOV/DIS.1864.TOT,
               DIS.1864.TOT.PCT  = DIS.1864.TOT/DIS.1864.TOT,
               
               DIS.GE65.POV.PCT	 = 	DIS.GE65.POV/DIS.GE65.TOT,
               DIS.GE65.NPOV.PCT	 = 	DIS.GE65.NPOV/DIS.GE65.TOT,
               DIS.GE65.TOT.PCT  = DIS.GE65.TOT/DIS.GE65.TOT)
     
    f.ctyDIS_agy$geoname <- listID$plName1
    f.ctyDIS_agy$county <- 0

    f.ctyDIS_agy <- f.ctyDIS_agy[,c("geoname",	"county",				
                                    "DIS.0017.TOT", "DIS.0017.POV", "DIS.0017.NPOV", 
                                    "DIS.1864.TOT", "DIS.1864.POV", "DIS.1864.NPOV",
                                    "DIS.GE65.TOT", "DIS.GE65.POV", "DIS.GE65.NPOV", 
                                   "DIS.0017.POV.PCT", "DIS.0017.NPOV.PCT","DIS.0017.TOT.PCT",
                                   "DIS.1864.POV.PCT", "DIS.1864.NPOV.PCT", "DIS.1864.TOT.PCT",
                                   "DIS.GE65.POV.PCT",  "DIS.GE65.NPOV.PCT",  "DIS.GE65.TOT.PCT"
                                   )]
    
   f.ctyDIS_cty <- bind_rows(f.ctyDIS_agy, f.ctyDIS_cty)
   
}
 f.ctyDIS_cty$geoname <- sub(", Colorado","",f.ctyDIS_cty$geoname)  

 ctyList <- as.list(unique(sort(f.ctyDIS_cty$county)))

 # preparing files

     f.ctyDIS_cty[is.na(f.ctyDIS_cty)] <- 0
     f.ctyDIS_tot <- f.ctyDIS_cty[, c(1:11)]
     f.ctyDIS_pct <- f.ctyDIS_cty[,c(1, 2, 12:20)]
     
     f.ctyDISL_tot <- f.ctyDIS_tot %>% 
          gather(var, count, DIS.0017.TOT:DIS.GE65.NPOV, factor_key=TRUE) %>%
          separate(var,c("type","age_cat","meas")) %>% arrange(age_cat)
     
     
     
      f.ctyDISL_pct <- f.ctyDIS_pct %>% 
          gather(var, pct, DIS.0017.POV.PCT:DIS.GE65.TOT.PCT, factor_key=TRUE) %>%
          separate(var,c("type","age_cat","meas",NA)) %>% arrange(age_cat)

# Calculating Marginal total
      f.ctyDISL_Mar <- f.ctyDISL_tot %>%
            group_by(county, meas) %>%
             summarize(tcount = sum(count))
      
      tcount <- f.ctyDISL_Mar[which(f.ctyDISL_Mar$meas == "TOT"),]
      names(tcount)[3] <- "sumcount"
          
      f.ctyDISL_Mar1 <- inner_join(f.ctyDISL_Mar,tcount,by="county")
      f.ctyDISL_Mar1 <-  f.ctyDISL_Mar1 %>% mutate(pct = tcount/sumcount) 
      f.ctyDISL_Mar1$age_cat <- "TOT"
      f.ctyDISL_Mar1$type <- "DIS"
      f.ctyDISL_Mar1 <- f.ctyDISL_Mar1[,c(1,8,7,2,3,6)]
      f.ctyDISL_Mar2 <- inner_join(f.ctyList,f.ctyDISL_Mar1,by="county")
      names(f.ctyDISL_Mar2) <- c("geoname","county","type","age_cat","meas","count","pct")
      
     
      f.ctyDISL_MarT <- f.ctyDISL_Mar2[,c(1:6)]
      f.ctyDISL_MarP <- f.ctyDISL_Mar2[,c(1:5,7)]
      
      f.ctyDISL_tot <- bind_rows(f.ctyDISL_tot,f.ctyDISL_MarT)
      f.ctyDISL_pct <- bind_rows(f.ctyDISL_pct,f.ctyDISL_MarP)
      
# Revising Type 
    f.ctyDISL_tot$type <- "Persons with Disabilities"
  
    
   f.ctyDISL_pct$type <- "Persons with Disabilities"
 

    
# revising Age Cat
   f.ctyDISL_tot$age_cat <-plyr::revalue(f.ctyDISL_tot$age_cat, c("0017" = "Under 18",
                             "1864" = "18 to 64",
                             "GE65" = "65+",
                             "TOT" = "Total"))
    
  f.ctyDISL_pct$age_cat <-plyr::revalue(f.ctyDISL_pct$age_cat, c("0017" = "Under 18",
                             "1864" = "18 to 64",
                             "GE65" = "65+",
                             "TOT" = "Total"))
  
  f.ctyDISL_tot$age_cat <- factor(f.ctyDISL_tot$age_cat,levels= c("Under 18","18 to 64","65+","Total"))     
  f.ctyDISL_pct$age_cat <- factor(f.ctyDISL_pct$age_cat,levels= c("Under 18","18 to 64","65+","Total")) 

# revising measure
   f.ctyDISL_tot$meas <-plyr::revalue(f.ctyDISL_tot$meas, c("POV" = "Below Poverty Level",
                             "NPOV" = "Above Poverty Level",
                             "TOT" = "Total"))
    
  f.ctyDISL_pct$meas <-plyr::revalue(f.ctyDISL_pct$meas, c("POV" = "Below Poverty Level",
                             "NPOV" = "Above Poverty Level",
                             "TOT" = "Total"))
 
  f.ctyDISL_tot$meas <- factor(f.ctyDISL_tot$meas,levels= c("Below Poverty Level","Above Poverty Level","Total"))     
  f.ctyDISL_pct$meas <- factor(f.ctyDISL_pct$meas,levels= c("Below Poverty Level","Above Poverty Level","Total")) 
  
    f.ctyDISL_PLT <- inner_join(f.ctyDISL_pct,  f.ctyDISL_tot[,2:6],by=c("county","age_cat","meas"))
    # Plotly  
    f.ctyDISL_PLT <- f.ctyDISL_PLT[which(f.ctyDISL_PLT$meas == "Below Poverty Level"),] %>% arrange(factor(county, levels = ctyList))
    
    f.ctyDISL_PLT$indText  <- paste0( f.ctyDISL_PLT$geoname," Age Category: ", f.ctyDISL_PLT$age_cat," Percentage: ",percent(f.ctyDISL_PLT$pct * 100)," Count: ",NumFmt(f.ctyDISL_PLT$count))  
    grTitle <- paste0("Persons with Disabilities Below the Poverty Level, ",listID$plName1)
    outCap <- captionSrc("ACS",ACS,"C18130") 
    xAxis <- list(title = "Age Category")
    yAxis <- list(title = 'Percent',tickformat = "%")
    
 
# % persons in poverty with Disabilities
if(length(ctyfips) > 1 ){
 DISPLOT <- f.ctyDISL_PLT %>%
  plot_ly(
    type = 'bar', 
    x = ~age_cat, 
    y = ~pct,
  #  color=~type,
    text = ~indText,
    hoverinfo = 'text',
    transforms = list(
      list(
        type = 'filter',
        target = ~geoname,
        operation = '=',
        value = unique(f.ctyDISL_PLT$geoname)[1]
      )
  )) %>% layout( title=grTitle, yaxis = yAxis, xaxis=xAxis,
          showlegend = FALSE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCap,
                      font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4) ,
    updatemenus = list(
      list(
        type = 'dropdown',
        active = 0,
        buttons = list(
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyDISL_PLT$geoname)[1]),
               label = unique(f.ctyDISL_PLT$geoname)[1]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyDISL_PLT$geoname)[2]),
               label = unique(f.ctyDISL_PLT$geoname)[2]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyDISL_PLT$geoname)[3]),
               label = unique(f.ctyDISL_PLT$geoname)[3]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyDISL_PLT$geoname)[4]),
               label = unique(f.ctyDISL_PLT$geoname)[4]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyDISL_PLT$geoname)[5]),
               label = unique(f.ctyDISL_PLT$geoname)[5]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyDISL_PLT$geoname)[6]),
               label = unique(f.ctyDISL_PLT$geoname)[6]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyDISL_PLT$geoname)[7]),
               label = unique(f.ctyDISL_PLT$geoname)[7])
      )
  )))
} else {
   DISPLOT <- f.ctyDISL_PLT %>%
  plot_ly(
    type = 'bar', 
    x = ~age_cat, 
    y = ~pct,
 #   color=~type,
    text = ~indText,
    hoverinfo = 'text',
    transforms = list(
      list(
        type = 'filter',
        target = ~geoname,
        operation = '=',
        value = unique(f.ctyDISL_PLT$geoname)[1]
      )
  ))   %>% layout( title=grTitle, yaxis = yAxis, xaxis=xAxis,
          showlegend = FALSE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCap,
                      font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4))
}    
    

  
    # flex Table and output data file
   ageList <- c("Under 18","18 to 64","65+","Total")

    f.ctyDISL_tot$count <- NumFmt(f.ctyDISL_tot$count)
    f.ctyDISL_pct$pct <- percent(f.ctyDISL_pct$pct * 100)
    
   
    
    f.ctyDIS_Count <-  f.ctyDISL_tot %>% spread(meas,count)
    f.ctyDIS_Percent <-  f.ctyDISL_pct %>% spread(meas,pct)
    
    f.ctyDIS_Count$Value <- "Count"
     f.ctyDIS_Percent$Value <- "Percentage"
   
    f.ctyDIS_tab <- bind_rows(mutate_all(f.ctyDIS_Count,as.character),mutate_all(f.ctyDIS_Percent,as.character))  
    
    
    # reordering Records for Table
    
    f.ctyDIS_tab  <- f.ctyDIS_tab %>% arrange(factor(county, levels = ctyList),  
                                              factor(age_cat, levels = ageList), desc(Value))
    
    f.ctyDIS_tab <- f.ctyDIS_tab[,c(1,4,8,5:7)]
     f.ctyDIS_tab$age_cat <- as.character(f.ctyDIS_tab$age_cat)
  
    #Clearing geoname
    if(length(ctyfips) == 1) {
      npanel1 <- 1
    } else {
      npanel1 = length(ctyfips) + 1
    }
  
    f.ctyDIS_tab <- clrGeoname(f.ctyDIS_tab,"geoname",npanel1,8)
    
    
    for(i in 1:nrow(f.ctyDIS_tab)){
      if(i %% 2 == 0){
        f.ctyDIS_tab[i,2] <- ""
      } 
    }
    

     
   
    
     #Producing Flextable
 
 tab_head <- paste0("Persons with Disabilities Below the Poverty Level, ",listID$plName1)


 names(f.ctyDIS_tab) <- c("Agency/County","Age Category","Value","Below Poverty Level","Above Poverty Level","Total")

   
   f.flexDIS <- flextable(
       f.ctyDIS_tab,
       col_keys = names(f.ctyDIS_tab)) %>%
       add_header_row(values=tab_head,top=TRUE,colwidths=6) %>%
       add_footer_row(values=outCap,top=FALSE,colwidths=6) %>%
       align(j=1:2, align="left", part="body") %>%  
       width(j= 1, width=3) %>%
       width(j=2, width=1.1) %>%
       width(j=3:6,width=0.75) %>%
       height(part="footer", height=0.4) %>%
       height(part="header",i=2,height=0.8)
 

  outList <- list("plot" = DISPLOT, "FlexTable" = f.flexDIS, "data" = f.ctyDISL_pct, "table" = f.ctyDIS_tab,"caption" = outCap)
  return(outList)
}


