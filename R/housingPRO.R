#'housingPRO Outputs Tables and plots for housing tenure by FPL and Headship
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
housingPRO <- function(lvl,listID, ACS,curYr) {
  # Collecting place ids from  idList, setting default values


    ctyfips <- as.character(as.numeric(substr(listID$list1,3,5)))

  # Extracting data from tidycensus 
  f.ctyHH <- codemog_api(data="b17019",db=ACS,sumlev="50",geography="sumlev",meta="no")
   
   
   
   f.ctyHH[,c(3,8:30)] <- sapply(f.ctyHH[,c(3,8:30)],as.numeric)

   f.ctyHH_cty <- f.ctyHH %>%
             filter(county %in% ctyfips) %>%  
            group_by(county) %>%
           mutate(
                   TOT.TOT.TOT = 	b17019001,
                   TOT.TOT.OWN = 	sum(b17019004, b17019008, b17019011, b17019015, b17019019, b17019022),
                   TOT.TOT.RENT = 	sum(b17019005, b17019009, b17019012, b17019016, b17019020, b17019023),
                   TOT.MAR.TOT = 	b17019003 + b17019014,
                   TOT.MAR.OWN = 	b17019004 + b17019015,
                   TOT.MAR.RENT = 	b17019005 + b17019016,
                   TOT.MALE.TOT = 	b17019007 + b17019018,
                   TOT.MALE.OWN = 	b17019008 + b17019019,
                   TOT.MALE.RENT = 	b17019009 + b17019020,
                   TOT.FEMALE.TOT = 	b17019010 + b17019021,
                   TOT.FEMALE.OWN = 	b17019011 + b17019022,
                   TOT.FEMALE.RENT = 	b17019012 + b17019023,
                  POV.TOT.TOT = 		b17019002,
                  POV.TOT.OWN	= b17019004 + b17019008 + b17019011,
                  POV.TOT.RENT = b17019005 + b17019009 + b17019012,
                  POV.MAR.TOT = 		b17019003,
                  POV.MAR.OWN = 		b17019004,
                  POV.MAR.RENT = 		b17019005,
                  POV.MALE.TOT = 		b17019007,
                  POV.MALE.OWN = 		b17019008,
                  POV.MALE.RENT = 		b17019009,
                  POV.FEMALE.TOT = 		b17019010,
                  POV.FEMALE.OWN = 		b17019011,
                  POV.FEMALE.RENT = 		b17019012,
                  NPOV.TOT.OWN = b17019015 + b17019019 + b17019022,
                  NPOV.TOT.RENT = b17019016 + b17019020 + b17019023,
                  NPOV.TOT.TOT = 		b17019013,
                  NPOV.MAR.TOT = 		b17019014,
                  NPOV.MAR.OWN = 		b17019015,
                  NPOV.MAR.RENT = 		b17019016,
                  NPOV.MALE.TOT = 		b17019018,
                  NPOV.MALE.OWN = 		b17019019,
                  NPOV.MALE.RENT = 		b17019020,
                  NPOV.FEMALE.TOT = 		b17019021,
                  NPOV.FEMALE.OWN = 		b17019022,
                  NPOV.FEMALE.RENT = 		b17019023,
                  		
                  TOT.TOT.TOT.PCT = 		TOT.TOT.TOT/TOT.TOT.TOT,
                  TOT.TOT.OWN.PCT =  	TOT.TOT.OWN/TOT.TOT.TOT,
                  TOT.TOT.RENT.PCT =  	TOT.TOT.RENT/TOT.TOT.TOT,
                  TOT.MAR.TOT.PCT = 		TOT.MAR.TOT/TOT.MAR.TOT,
                  TOT.MAR.OWN.PCT =  	TOT.MAR.OWN/TOT.MAR.TOT,
                  TOT.MAR.RENT.PCT =  	TOT.MAR.RENT/TOT.MAR.TOT,
                  TOT.MALE.TOT.PCT = 		TOT.MALE.TOT/TOT.MALE.TOT,
                  TOT.MALE.OWN.PCT =  	TOT.MALE.OWN/TOT.MALE.TOT,
                  TOT.MALE.RENT.PCT =  	TOT.MALE.RENT/TOT.MALE.TOT,
                  TOT.FEMALE.TOT.PCT = 		TOT.FEMALE.TOT/TOT.FEMALE.TOT,
                  TOT.FEMALE.OWN.PCT =  	TOT.FEMALE.OWN/TOT.FEMALE.TOT,
                  TOT.FEMALE.RENT.PCT =  	TOT.FEMALE.RENT/TOT.MAR.TOT,
                  POV.TOT.TOT.PCT = 		POV.TOT.TOT/POV.TOT.TOT,
                  POV.TOT.OWN.PCT =  	POV.TOT.OWN/POV.TOT.TOT,
                  POV.TOT.RENT.PCT =  	POV.TOT.RENT/POV.TOT.TOT,
                  POV.MAR.TOT.PCT = 		POV.MAR.TOT/POV.MAR.TOT,
                  POV.MAR.OWN.PCT = 		POV.MAR.OWN/POV.MAR.TOT,
                  POV.MAR.RENT.PCT = 		POV.MAR.RENT/POV.MAR.TOT,
                  POV.MALE.TOT.PCT = 		POV.MALE.TOT/POV.MALE.TOT,
                  POV.MALE.OWN.PCT = 		POV.MALE.OWN/POV.MALE.TOT,
                  POV.MALE.RENT.PCT = 		POV.MALE.RENT/POV.MALE.TOT,
                  POV.FEMALE.TOT.PCT = 		POV.FEMALE.TOT/POV.FEMALE.TOT,
                  POV.FEMALE.OWN.PCT = 		POV.FEMALE.OWN/POV.FEMALE.TOT,
                  POV.FEMALE.RENT.PCT = 		POV.FEMALE.RENT/POV.FEMALE.TOT,
                  NPOV.TOT.TOT.PCT = 		NPOV.TOT.TOT/NPOV.TOT.TOT,
                  NPOV.TOT.OWN.PCT =  	NPOV.TOT.OWN/NPOV.TOT.TOT,
                  NPOV.TOT.RENT.PCT =  	NPOV.TOT.RENT/NPOV.TOT.TOT,
                  NPOV.MAR.TOT.PCT = 		NPOV.MAR.TOT/NPOV.MAR.TOT,
                  NPOV.MAR.OWN.PCT = 		NPOV.MAR.OWN/NPOV.MAR.TOT,
                  NPOV.MAR.RENT.PCT = 		NPOV.MAR.RENT/NPOV.MAR.TOT,
                  NPOV.MALE.TOT.PCT = 		NPOV.MALE.TOT/NPOV.MALE.TOT,
                  NPOV.MALE.OWN.PCT = 		NPOV.MALE.OWN/NPOV.MALE.TOT,
                  NPOV.MALE.RENT.PCT = 		NPOV.MALE.RENT/NPOV.MALE.TOT,
                  NPOV.FEMALE.TOT.PCT = 		NPOV.FEMALE.TOT/NPOV.FEMALE.TOT,
                  NPOV.FEMALE.OWN.PCT = 		NPOV.FEMALE.OWN/NPOV.FEMALE.TOT,
                  NPOV.FEMALE.RENT.PCT = 		NPOV.FEMALE.RENT/NPOV.FEMALE.TOT
           )

	
   f.ctyHH_cty <- f.ctyHH_cty[,c("geoname",	"county",	
                                  "TOT.TOT.TOT", "TOT.TOT.OWN", "TOT.TOT.RENT",
                                  "TOT.MAR.TOT", "TOT.MAR.OWN", "TOT.MAR.RENT", 
                                  "TOT.MALE.TOT", "TOT.MALE.OWN", "TOT.MALE.RENT",
                                  "TOT.FEMALE.TOT", "TOT.FEMALE.OWN", "TOT.FEMALE.RENT",
                                  "POV.TOT.TOT", "POV.TOT.OWN", "POV.TOT.RENT",
                                  "POV.MAR.TOT", "POV.MAR.OWN", "POV.MAR.RENT", 
                                  "POV.MALE.TOT", "POV.MALE.OWN", "POV.MALE.RENT",
                                  "POV.FEMALE.TOT", "POV.FEMALE.OWN", "POV.FEMALE.RENT",
                                  "NPOV.TOT.TOT", "NPOV.TOT.OWN", "NPOV.TOT.RENT",
                                  "NPOV.MAR.TOT", "NPOV.MAR.OWN", "NPOV.MAR.RENT",
                                  "NPOV.MALE.TOT", "NPOV.MALE.OWN", "NPOV.MALE.RENT",
                                  "NPOV.FEMALE.TOT", "NPOV.FEMALE.OWN", "NPOV.FEMALE.RENT",
                                  "TOT.TOT.TOT.PCT", "TOT.TOT.OWN.PCT", "TOT.TOT.RENT.PCT",
                                  "TOT.MAR.TOT.PCT", "TOT.MAR.OWN.PCT", "TOT.MAR.RENT.PCT",
                                  "TOT.MALE.TOT.PCT", "TOT.MALE.OWN.PCT", "TOT.MALE.RENT.PCT",
                                  "TOT.FEMALE.TOT.PCT", "TOT.FEMALE.OWN.PCT", "TOT.FEMALE.RENT.PCT",
                                  "POV.TOT.TOT.PCT", "POV.TOT.OWN.PCT", "POV.TOT.RENT.PCT",
                                  "POV.MAR.TOT.PCT", "POV.MAR.OWN.PCT", "POV.MAR.RENT.PCT",
                                  "POV.MALE.TOT.PCT", "POV.MALE.OWN.PCT", "POV.MALE.RENT.PCT",
                                  "POV.FEMALE.TOT.PCT", "POV.FEMALE.OWN.PCT", "POV.FEMALE.RENT.PCT",
                                  "NPOV.TOT.TOT.PCT", "NPOV.TOT.OWN.PCT", "NPOV.TOT.RENT.PCT",
                                  "NPOV.MAR.TOT.PCT", "NPOV.MAR.OWN.PCT", "NPOV.MAR.RENT.PCT",
                                  "NPOV.MALE.TOT.PCT", "NPOV.MALE.OWN.PCT", "NPOV.MALE.RENT.PCT",
                                  "NPOV.FEMALE.TOT.PCT", "NPOV.FEMALE.OWN.PCT", "NPOV.FEMALE.RENT.PCT"
                                   )]
 
if(length(ctyfips) > 1) {
     f.ctyHH_agy <- f.ctyHH %>%
             filter(county %in% ctyfips) %>%
          summarize(  
              TOT.TOT.TOT = 	sum(b17019001),
              TOT.TOT.OWN = 	sum(b17019004, b17019008, b17019011, b17019015, b17019019, b17019022),
              TOT.TOT.RENT = 	sum(b17019005, b17019009, b17019012, b17019016, b17019020, b17019023),
              TOT.MAR.TOT = 	sum(b17019003, b17019014),
              TOT.MAR.OWN = 	sum(b17019004, b17019015),
              TOT.MAR.RENT = 	sum(b17019005, b17019016),
              TOT.MALE.TOT = 	sum(b17019007, b17019018),
              TOT.MALE.OWN = 	sum(b17019008, b17019019),
              TOT.MALE.RENT = 	sum(b17019009, b17019020),
              TOT.FEMALE.TOT = 	sum(b17019010, b17019021),
              TOT.FEMALE.OWN = 	sum(b17019011, b17019022),
              TOT.FEMALE.RENT = 	sum(b17019012, b17019023),
              POV.TOT.TOT = 	sum(b17019002),
              POV.TOT.OWN = sum(b17019004, b17019008, b17019011),
              POV.TOT.RENT = sum(b17019005, b17019009, b17019012),
              POV.MAR.TOT = 	sum(b17019003),
              POV.MAR.OWN = 	sum(b17019004),
              POV.MAR.RENT = 	sum(b17019005),
              POV.MALE.TOT = 	sum(b17019007),
              POV.MALE.OWN = 	sum(b17019008),
              POV.MALE.RENT = 	sum(b17019009),
              POV.FEMALE.TOT = 	sum(b17019010),
              POV.FEMALE.OWN = 	sum(b17019011),
              POV.FEMALE.RENT = 	sum(b17019012),
              NPOV.TOT.TOT = 	sum(b17019013),
              NPOV.TOT.OWN = sum(b17019015, b17019019, b17019022),
              NPOV.TOT.RENT = sum(b17019016, b17019020, b17019023),
              NPOV.MAR.TOT = 	sum(b17019014),
              NPOV.MAR.OWN = 	sum(b17019015),
              NPOV.MAR.RENT = 	sum(b17019016),
              NPOV.MALE.TOT = 	sum(b17019018),
              NPOV.MALE.OWN = 	sum(b17019019),
              NPOV.MALE.RENT = 	sum(b17019020),
              NPOV.FEMALE.TOT = 	sum(b17019021),
              NPOV.FEMALE.OWN = 	sum(b17019022),
              NPOV.FEMALE.RENT = 	sum(b17019023)

          ) %>%
         mutate(
                  TOT.TOT.TOT.PCT = 		TOT.TOT.TOT/TOT.TOT.TOT,
                  TOT.TOT.OWN.PCT =  	TOT.TOT.OWN/TOT.TOT.TOT,
                  TOT.TOT.RENT.PCT =  	TOT.TOT.RENT/TOT.TOT.TOT,
                  TOT.MAR.TOT.PCT = 		TOT.MAR.TOT/TOT.MAR.TOT,
                  TOT.MAR.OWN.PCT =  	TOT.MAR.OWN/TOT.MAR.TOT,
                  TOT.MAR.RENT.PCT =  	TOT.MAR.RENT/TOT.MAR.TOT,
                  TOT.MALE.TOT.PCT = 		TOT.MALE.TOT/TOT.MALE.TOT,
                  TOT.MALE.OWN.PCT =  	TOT.MALE.OWN/TOT.MALE.TOT,
                  TOT.MALE.RENT.PCT =  	TOT.MALE.RENT/TOT.MALE.TOT,
                  TOT.FEMALE.TOT.PCT = 		TOT.FEMALE.TOT/TOT.FEMALE.TOT,
                  TOT.FEMALE.OWN.PCT =  	TOT.FEMALE.OWN/TOT.FEMALE.TOT,
                  TOT.FEMALE.RENT.PCT =  	TOT.FEMALE.RENT/TOT.MAR.TOT,
                  POV.TOT.TOT.PCT = 		POV.TOT.TOT/POV.TOT.TOT,
                  POV.TOT.OWN.PCT =  	POV.TOT.OWN/POV.TOT.TOT,
                  POV.TOT.RENT.PCT =  	POV.TOT.RENT/POV.TOT.TOT,
                  POV.MAR.TOT.PCT = 		POV.MAR.TOT/POV.MAR.TOT,
                  POV.MAR.OWN.PCT = 		POV.MAR.OWN/POV.MAR.TOT,
                  POV.MAR.RENT.PCT = 		POV.MAR.RENT/POV.MAR.TOT,
                  POV.MALE.TOT.PCT = 		POV.MALE.TOT/POV.MALE.TOT,
                  POV.MALE.OWN.PCT = 		POV.MALE.OWN/POV.MALE.TOT,
                  POV.MALE.RENT.PCT = 		POV.MALE.RENT/POV.MALE.TOT,
                  POV.FEMALE.TOT.PCT = 		POV.FEMALE.TOT/POV.FEMALE.TOT,
                  POV.FEMALE.OWN.PCT = 		POV.FEMALE.OWN/POV.FEMALE.TOT,
                  POV.FEMALE.RENT.PCT = 		POV.FEMALE.RENT/POV.FEMALE.TOT,
                  NPOV.TOT.TOT.PCT = 		NPOV.TOT.TOT/NPOV.TOT.TOT,
                  NPOV.TOT.OWN.PCT =  	NPOV.TOT.OWN/NPOV.TOT.TOT,
                  NPOV.TOT.RENT.PCT =  	NPOV.TOT.RENT/NPOV.TOT.TOT,
                  NPOV.MAR.TOT.PCT = 		NPOV.MAR.TOT/NPOV.MAR.TOT,
                  NPOV.MAR.OWN.PCT = 		NPOV.MAR.OWN/NPOV.MAR.TOT,
                  NPOV.MAR.RENT.PCT = 		NPOV.MAR.RENT/NPOV.MAR.TOT,
                  NPOV.MALE.TOT.PCT = 		NPOV.MALE.TOT/NPOV.MALE.TOT,
                  NPOV.MALE.OWN.PCT = 		NPOV.MALE.OWN/NPOV.MALE.TOT,
                  NPOV.MALE.RENT.PCT = 		NPOV.MALE.RENT/NPOV.MALE.TOT,
                  NPOV.FEMALE.TOT.PCT = 		NPOV.FEMALE.TOT/NPOV.FEMALE.TOT,
                  NPOV.FEMALE.OWN.PCT = 		NPOV.FEMALE.OWN/NPOV.FEMALE.TOT,
                  NPOV.FEMALE.RENT.PCT = 		NPOV.FEMALE.RENT/NPOV.FEMALE.TOT
         )
    f.ctyHH_agy$geoname <- listID$plName1
    f.ctyHH_agy$county <- 0

    f.ctyHH_agy <- f.ctyHH_agy[,c("geoname",	"county",	
                                  "TOT.TOT.TOT", "TOT.TOT.OWN", "TOT.TOT.RENT",
                                  "TOT.MAR.TOT", "TOT.MAR.OWN", "TOT.MAR.RENT", 
                                  "TOT.MALE.TOT", "TOT.MALE.OWN", "TOT.MALE.RENT",
                                  "TOT.FEMALE.TOT", "TOT.FEMALE.OWN", "TOT.FEMALE.RENT",
                                  "POV.TOT.TOT", "POV.TOT.OWN", "POV.TOT.RENT",
                                  "POV.MAR.TOT", "POV.MAR.OWN", "POV.MAR.RENT", 
                                  "POV.MALE.TOT", "POV.MALE.OWN", "POV.MALE.RENT",
                                  "POV.FEMALE.TOT", "POV.FEMALE.OWN", "POV.FEMALE.RENT",
                                  "NPOV.TOT.TOT", "NPOV.TOT.OWN", "NPOV.TOT.RENT",
                                  "NPOV.MAR.TOT", "NPOV.MAR.OWN", "NPOV.MAR.RENT",
                                  "NPOV.MALE.TOT", "NPOV.MALE.OWN", "NPOV.MALE.RENT",
                                  "NPOV.FEMALE.TOT", "NPOV.FEMALE.OWN", "NPOV.FEMALE.RENT",
                                  "TOT.TOT.TOT.PCT", "TOT.TOT.OWN.PCT", "TOT.TOT.RENT.PCT",
                                  "TOT.MAR.TOT.PCT", "TOT.MAR.OWN.PCT", "TOT.MAR.RENT.PCT",
                                  "TOT.MALE.TOT.PCT", "TOT.MALE.OWN.PCT", "TOT.MALE.RENT.PCT",
                                  "TOT.FEMALE.TOT.PCT", "TOT.FEMALE.OWN.PCT", "TOT.FEMALE.RENT.PCT",
                                  "POV.TOT.TOT.PCT", "POV.TOT.OWN.PCT", "POV.TOT.RENT.PCT",
                                  "POV.MAR.TOT.PCT", "POV.MAR.OWN.PCT", "POV.MAR.RENT.PCT",
                                  "POV.MALE.TOT.PCT", "POV.MALE.OWN.PCT", "POV.MALE.RENT.PCT",
                                  "POV.FEMALE.TOT.PCT", "POV.FEMALE.OWN.PCT", "POV.FEMALE.RENT.PCT",
                                  "NPOV.TOT.TOT.PCT", "NPOV.TOT.OWN.PCT", "NPOV.TOT.RENT.PCT",
                                  "NPOV.MAR.TOT.PCT", "NPOV.MAR.OWN.PCT", "NPOV.MAR.RENT.PCT",
                                  "NPOV.MALE.TOT.PCT", "NPOV.MALE.OWN.PCT", "NPOV.MALE.RENT.PCT",
                                  "NPOV.FEMALE.TOT.PCT", "NPOV.FEMALE.OWN.PCT", "NPOV.FEMALE.RENT.PCT"
                                   )]
    
   f.ctyHH_cty <- bind_rows(f.ctyHH_agy, f.ctyHH_cty)
   
}
 f.ctyHH_cty$geoname <- sub(", Colorado","",f.ctyHH_cty$geoname)  

 ctyList <- as.list(unique(sort(f.ctyHH_cty$county)))
 ctyName <- f.ctyHH_cty[,c(1,2)]
 ctyName <- as.list(unique(ctyName[order(f.ctyHH_cty$county),1]))
 
 # preparing files

     f.ctyHH_cty[is.na(f.ctyHH_cty)] <- 0
     f.ctyHH_tot <- f.ctyHH_cty[, c(1:38)]
     f.ctyHH_pct <- f.ctyHH_cty[,c(1, 2, 39:74)]
     
     f.ctyHHL_tot <- f.ctyHH_tot %>% 
          gather(var, count, TOT.TOT.TOT:NPOV.FEMALE.RENT, factor_key=TRUE) %>%
          separate(var,c("pov","famtype","tenure")) %>% arrange(famtype)
     
      f.ctyHHL_pct <- f.ctyHH_pct %>% 
          gather(var, pct, TOT.TOT.TOT.PCT:NPOV.FEMALE.RENT.PCT, factor_key=TRUE) %>%
          separate(var,c("pov","famtype","tenure",NA)) %>% arrange(famtype)

# revising pov
   f.ctyHHL_tot$pov <-plyr::revalue(f.ctyHHL_tot$pov, c("POV" = "Below Poverty Level",
                             "NPOV" = "Above Poverty Level", "TOT" = "Total"))
    
  f.ctyHHL_pct$pov <-plyr::revalue(f.ctyHHL_pct$pov, c("POV" = "Below Poverty Level",
                             "NPOV" = "Above Poverty Level", "TOT" = "Total"))      
    
# revising Fam Type
  
   f.ctyHHL_tot$famtype <-plyr::revalue(f.ctyHHL_tot$famtype, c("TOT" = "All Families",
                             "MAR" = "Married Couple",
                             "MALE" = "Male Householder",
                             "FEMALE" = "Female Householder"))
    
  f.ctyHHL_pct$famtype <-plyr::revalue(f.ctyHHL_pct$famtype, c("TOT" = "All Families",
                             "MAR" = "Married Couple",
                             "MALE" = "Male Householder",
                             "FEMALE" = "Female Householder"))
    

  # revising tenure
   f.ctyHHL_tot$tenure <-plyr::revalue(f.ctyHHL_tot$tenure, c("OWN" = "Own",
                             "RENT" = "Rent", "TOT" = "All Households"))

  
   f.ctyHHL_pct$tenure <-plyr::revalue(f.ctyHHL_pct$tenure, c("OWN" = "Own",
                             "RENT" = "Rent", "TOT" = "All Households"))

  
                                                             

    # Plotly 
   f.ctyHHL_PLT <- inner_join(f.ctyHHL_pct,f.ctyHHL_tot[,2:6], by=c("county","pov","famtype","tenure"))
   
   
    f.ctyHHL_PLT <- f.ctyHHL_PLT[which(f.ctyHHL_pct$pov == "Below Poverty Level" &
                                       f.ctyHHL_pct$tenure != "All Households"),] %>% arrange(famtype,tenure,county)
    f.ctyHHL_PLT$famtype <- factor(f.ctyHHL_PLT$famtype,c( "Female Householder",
                                                             "Male Householder",
                                                            "Married Couple", "All Families"))                                                           
    
    f.ctyHHL_PLT$indText  <- paste0( f.ctyHHL_PLT$geoname," Family Type: ", f.ctyHHL_PLT$famtype," Percentage: ",percent(f.ctyHHL_PLT$pct * 100)," Count: ",NumFmt(f.ctyHHL_PLT$count))  
    grTitle <- paste0("Housing Tenure, Below FPL, ",listID$plName1)
    outCap <- captionSrc("ACS",ACS,"B17010") 
    xAxis <- list(title = "Family Type")
    yAxis <- list(title = 'Percent',tickformat = ".1%")
    txtNames <- unique(f.ctyHHL_PLT$geoname)
    
    
 
 
# % persons in poverty with Disabilities
if(length(ctyfips) > 1 ){
 HHPLOT <- f.ctyHHL_PLT %>%
 plot_ly(
    type = 'bar', 
    x = ~famtype, 
    y = ~pct,
    color= ~tenure,
    text = ~indText,
    hoverinfo = 'text',
    transforms = list(
      list(
        type = 'filter',
        target = ~geoname,
        operation = '=',
        value = unique(f.ctyHHL_PLT$geoname)[1]
      )
  )) %>% layout( title=grTitle, yaxis = yAxis, xaxis=xAxis, 
          showlegend = TRUE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCap,
                      font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4) ,
    updatemenus = list(
      list(
        type = 'dropdown',
        active = 0,
        buttons = genDropdown(txtNames)
  )))
} else {
   HHPLOT <- f.ctyHHL_PLT %>%
  plot_ly(
    type = 'bar',
    x = ~famtype, 
    y = ~pct,
    color = ~tenure,
    text = ~indText,
    hoverinfo = 'text',
    transforms = list(
      list(
        type = 'filter',
        target = ~geoname,
        operation = '=',
        value = unique(f.ctyHHL_PLT$geoname)[1]
      )
  ))   %>% layout( title=grTitle, yaxis = yAxis, xaxis=xAxis,
          showlegend = TRUE, hoverlabel = "right", margin = list(l = 50, r = 50, t = 60, b = 100),  
                      annotations = list(text = outCap,
                      font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.4))
}    
    

  
    # flex Table and output data file
   
    povList <- c("Below Poverty Level", "Above Poverty Level",  "Total")
    
    famList <-  c("Female Householder", "Male Householder", "Married Couple", "All Families")
   
                       
    f.ctyHHL_tot$count <- format(round(f.ctyHHL_tot$count ,digits=0),  big.mark=",")
    f.ctyHHL_pct$pct <- percent(f.ctyHHL_pct$pct * 100)
    
     f.ctyHHL_tot$type2 <- "Count"
     f.ctyHHL_pct$type2 <- "Percentage"
    
    f.ctyHH_Count <-  f.ctyHHL_tot %>% spread(tenure,count)
    f.ctyHH_Percent <-  f.ctyHHL_pct %>% spread(tenure,pct)
   
    f.ctyHH_tab <- bind_rows(f.ctyHH_Count,f.ctyHH_Percent)  
    
    
    # reordering Records for Table

    f.ctyHH_tab  <- f.ctyHH_tab %>% arrange(factor(county, levels = ctyList),  
                                              factor(famtype, levels = famList), 
                                              factor(pov, levels = povList),
                                              desc(type2))
    
    
  
    #Clearing geoname
    if(length(ctyfips) == 1) {
      npanel1 <- 1
      npanel2 <- 4
    } else {
      npanel1 <- length(ctyfips) + 1
      npanel2 <- (length(ctyfips) + 1) * 4
    }
    
    f.ctyHH_tab <- clrGeoname(f.ctyHH_tab,"geoname",npanel1,24)
    f.ctyHH_tab <- clrGeoname(f.ctyHH_tab,"famtype",npanel2,6)
    for(i in 1:nrow(f.ctyHH_tab)){
      if(i %% 2 == 0){
        f.ctyHH_tab[i,3] <- ""
      } 
    }

    
     #Producing Flextable
 
 tab_head <- paste0("Housing Tenure by Poverty Status, ",listID$plName1)

 f.ctyHH_tab <-  f.ctyHH_tab[,c(1,4,3,5,7,8,6)]
 names(f.ctyHH_tab) <- c("Agency/County","Family Type","Poverty Level","Value","Own","Rent","All Households")

   
   f.flexHH <- flextable(
       f.ctyHH_tab,
       col_keys = names(f.ctyHH_tab)) %>%
       add_header_row(values=tab_head,top=TRUE,colwidths=7) %>%
       add_footer_row(values=outCap,top=FALSE,colwidths=7) %>%
       align(j=1:2, align="left", part="body") %>%
       width(j= 1, width=3) %>%
       width(j=2:3, width=1.6) %>%
       width(j=4, width=1) %>%
       width(j=5:7,width=0.75) %>%
       height(part="footer", height=0.4) %>%
       height(part="header",i=2, height=0.7)
 

  outList <- list("plot" = HHPLOT, "FlexTable" = f.flexHH, "data" = f.ctyHHL_PLT, "table" = f.ctyHH_tab,"caption" = outCap)
  return(outList)
}


