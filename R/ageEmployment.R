#'ageEmployment Outputs Table of Age by employment stats and charts
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
ageEmployment <- function(lvl,listID, ACS,curYr) {
  # Collecting place ids from  idList, setting default values

    ctyfips <- as.character(as.numeric(substr(listID$list1,3,5)))
   
   f.ctyEMP <- codemog_api(data="b23001",db=ACS,sumlev="50",geography="sumlev",meta="no")
   f.ctyEMP[,8:180] <- sapply(f.ctyEMP[,8:180],as.numeric)

   f.ctyEMP_cty <- f.ctyEMP %>%
             filter(county %in% ctyfips) %>%
            group_by(county) %>%
           mutate(
              TOT.1619 = sum(b23001003, b23001089),
              LF.1619 = sum(b23001004, b23001090),
              CIVIL.1619 = sum(b23001006, b23001092),
              EMP.1619 = sum(b23001007, b23001093),
              UNEMP.1619 = sum(b23001008, b23001094),
              NLF.1619 = sum(b23001009, b23001095),
              LF.1619P = LF.1619/TOT.1619,
              EMP.1619P = EMP.1619/CIVIL.1619,
              UNEMP.1619P = UNEMP.1619/CIVIL.1619,
              NLF.1619P = NLF.1619/TOT.1619,
              
              TOT.2024 = sum(b23001010, b23001096, b23001017, b23001103),
              LF.2024 = sum(b23001011, b23001097, b23001018, b23001104),
              CIVIL.2024 = sum(b23001013, b23001099, b23001020, b23001106),
              EMP.2024 = sum(b23001014, b23001100, b23001021, b23001107),
              UNEMP.2024 = sum(b23001015, b23001101, b23001022, b23001108),
              NLF.2024 = sum(b23001016, b23001102, b23001023, b23001109),
              LF.2024P = LF.2024/TOT.2024,
              EMP.2024P = EMP.2024/CIVIL.2024,
              UNEMP.2024P = UNEMP.2024/CIVIL.2024,
              NLF.2024P = NLF.2024/TOT.2024,
              
              TOT.2544 = sum(b23001024, b23001110, b23001031, b23001117, b23001038, b23001124),
              LF.2544 = sum(b23001025, b23001111, b23001032, b23001118, b23001039, b23001125),
              CIVIL.2544 = sum(b23001027, b23001113, b23001034, b23001120, b23001041, b23001127),
              EMP.2544 = sum(b23001028, b23001114, b23001035, b23001121, b23001042, b23001128),
              UNEMP.2544 = sum(b23001029, b23001115, b23001036, b23001122, b23001043, b23001129),
              NLF.2544 = sum(b23001030, b23001116, b23001037, b23001123, b23001044, b23001130),
              LF.2544P = LF.2544/TOT.2544,
              EMP.2544P = EMP.2544/CIVIL.2544,
              UNEMP.2544P = UNEMP.2544/CIVIL.2544,
              NLF.2544P = NLF.2544/TOT.2544,
              
              TOT.4554 = sum(b23001045, b23001131),
              LF.4554 = sum(b23001046, b23001132),
              CIVIL.4554 = sum(b23001048, b23001134),
              EMP.4554 = sum(b23001049, b23001135),
              UNEMP.4554 = sum(b23001050, b23001136),
              NLF.4554 = sum(b23001051, b23001137),
              LF.4554P = LF.4554/TOT.4554,
              EMP.4554P = EMP.4554/CIVIL.4554,
              UNEMP.4554P = UNEMP.4554/CIVIL.4554,
              NLF.4554P = NLF.4554/TOT.4554,
              
              TOT.5559 = sum(b23001052, b23001138),
              LF.5559 = sum(b23001053, b23001139),
              CIVIL.5559 = sum(b23001055, b23001141),
              EMP.5559 = sum(b23001056, b23001142),
              UNEMP.5559 = sum(b23001057, b23001143),
              NLF.5559 = sum(b23001058, b23001144),
              LF.5559P = LF.5559/TOT.5559,
              EMP.5559P = EMP.5559/CIVIL.5559,
              UNEMP.5559P = UNEMP.5559/CIVIL.5559,
              NLF.5559P = NLF.5559/TOT.5559,
              
              TOT.6064 = sum(b23001059, b23001145, b23001066, b23001152),
              LF.6064 = sum(b23001060, b23001146, b23001067, b23001153),
              CIVIL.6064 = sum(b23001062, b23001148, b23001069, b23001155),
              EMP.6064 = sum(b23001063, b23001149, b23001070, b23001156),
              UNEMP.6064 = sum(b23001064, b23001150, b23001071, b23001157),
              NLF.6064 = sum(b23001065, b23001151, b23001072, b23001158),
              LF.6064P = LF.6064/TOT.6064,
              EMP.6064P = EMP.6064/CIVIL.6064,
              UNEMP.6064P = UNEMP.6064/CIVIL.6064,
              NLF.6064P = NLF.6064/TOT.6064,
              
              TOT.6574 = sum(b23001073, b23001159, b23001078, b23001164),
              LF.6574 = sum(b23001074, b23001160, b23001079, b23001165),
              CIVIL.6574 = sum(b23001074, b23001160, b23001079, b23001165),
              EMP.6574 = sum(b23001075, b23001161, b23001080, b23001166),
              UNEMP.6574 = sum(b23001076, b23001162, b23001081, b23001167),
              NLF.6574 = sum(b23001077, b23001163, b23001082, b23001168),
              LF.6574P = LF.6574/TOT.6574,
              EMP.6574P = EMP.6574/LF.6574,
              UNEMP.6574P = UNEMP.6574/LF.6574,
              NLF.6574P = NLF.6574/TOT.6574,
              
              TOT.75 = sum(b23001083, b23001169),
              LF.75 = sum(b23001084, b23001170),
              CIVIL.75 = sum(b23001084, b23001170),
              EMP.75 = sum(b23001085, b23001171),
              UNEMP.75 = sum(b23001086, b23001172),
              NLF.75 = sum(b23001087, b23001173),
              LF.75P = LF.75/TOT.75,
              EMP.75P = EMP.75/CIVIL.75,
              UNEMP.75P = UNEMP.75/CIVIL.75,
              NLF.75P = NLF.75/TOT.75       )
   
   f.ctyEMP_cty <- f.ctyEMP_cty[,c("geoname", "county", "TOT.1619", "LF.1619", "CIVIL.1619", "EMP.1619", "UNEMP.1619", "NLF.1619", 
                                    "LF.1619P", "EMP.1619P", "UNEMP.1619P", "NLF.1619P", "TOT.2024", "LF.2024", "CIVIL.2024", "EMP.2024", 
                                    "UNEMP.2024", "NLF.2024", "LF.2024P", "EMP.2024P", "UNEMP.2024P", "NLF.2024P", "TOT.2544", "LF.2544", 
                                    "CIVIL.2544", "EMP.2544", "UNEMP.2544", "NLF.2544", "LF.2544P", "EMP.2544P", "UNEMP.2544P", "NLF.2544P", 
                                    "TOT.4554", "LF.4554", "CIVIL.4554", "EMP.4554", "UNEMP.4554", "NLF.4554", "LF.4554P", "EMP.4554P", 
                                    "UNEMP.4554P", "NLF.4554P", "TOT.5559", "LF.5559", "CIVIL.5559", "EMP.5559", "UNEMP.5559", "NLF.5559", 
                                    "LF.5559P", "EMP.5559P", "UNEMP.5559P", "NLF.5559P", "TOT.6064", "LF.6064", "CIVIL.6064", "EMP.6064", 
                                    "UNEMP.6064", "NLF.6064", "LF.6064P", "EMP.6064P", "UNEMP.6064P", "NLF.6064P", "TOT.6574", "LF.6574", 
                                    "CIVIL.6574", "EMP.6574", "UNEMP.6574", "NLF.6574", "LF.6574P", "EMP.6574P", "UNEMP.6574P", "NLF.6574P", 
                                    "TOT.75", "LF.75", "CIVIL.75", "EMP.75", "UNEMP.75", "NLF.75", "LF.75P", "EMP.75P", 
                                    "UNEMP.75P", "NLF.75P")]
 
if(length(ctyfips) > 1) {
     f.ctyEMP_agy <- f.ctyEMP %>%
             filter(county %in% ctyfips) %>%
          summarize(
                  TOT.1619 = sum(b23001003, b23001089),
                  LF.1619 = sum(b23001004, b23001090),
                  CIVIL.1619 = sum(b23001006, b23001092),
                  EMP.1619 = sum(b23001007, b23001093),
                  UNEMP.1619 = sum(b23001008, b23001094),
                  NLF.1619 = sum(b23001009, b23001095),
                  
                  TOT.2024 = sum(b23001010, b23001096, b23001017, b23001103),
                  LF.2024 = sum(b23001011, b23001097, b23001018, b23001104),
                  CIVIL.2024 = sum(b23001013, b23001099, b23001020, b23001106),
                  EMP.2024 = sum(b23001014, b23001100, b23001021, b23001107),
                  UNEMP.2024 = sum(b23001015, b23001101, b23001022, b23001108),
                  NLF.2024 = sum(b23001016, b23001102, b23001023, b23001109),
                  
                  TOT.2544 = sum(b23001024, b23001110, b23001031, b23001117, b23001038, b23001124),
                  LF.2544 = sum(b23001025, b23001111, b23001032, b23001118, b23001039, b23001125),
                  CIVIL.2544 = sum(b23001027, b23001113, b23001034, b23001120, b23001041, b23001127),
                  EMP.2544 = sum(b23001028, b23001114, b23001035, b23001121, b23001042, b23001128),
                  UNEMP.2544 = sum(b23001029, b23001115, b23001036, b23001122, b23001043, b23001129),
                  NLF.2544 = sum(b23001030, b23001116, b23001037, b23001123, b23001044, b23001130),
                  
                  TOT.4554 = sum(b23001045, b23001131),
                  LF.4554 = sum(b23001046, b23001132),
                  CIVIL.4554 = sum(b23001048, b23001134),
                  EMP.4554 = sum(b23001049, b23001135),
                  UNEMP.4554 = sum(b23001050, b23001136),
                  NLF.4554 = sum(b23001051, b23001137),
                  
                  TOT.5559 = sum(b23001052, b23001138),
                  LF.5559 = sum(b23001053, b23001139),
                  CIVIL.5559 = sum(b23001055, b23001141),
                  EMP.5559 = sum(b23001056, b23001142),
                  UNEMP.5559 = sum(b23001057, b23001143),
                  NLF.5559 = sum(b23001058, b23001144),
                  
                  TOT.6064 = sum(b23001059, b23001145, b23001066, b23001152),
                  LF.6064 = sum(b23001060, b23001146, b23001067, b23001153),
                  CIVIL.6064 = sum(b23001062, b23001148, b23001069, b23001155),
                  EMP.6064 = sum(b23001063, b23001149, b23001070, b23001156),
                  UNEMP.6064 = sum(b23001064, b23001150, b23001071, b23001157),
                  NLF.6064 = sum(b23001065, b23001151, b23001072, b23001158),
                  
                  TOT.6574 = sum(b23001073, b23001159, b23001078, b23001164),
                  LF.6574 = sum(b23001074, b23001160, b23001079, b23001165),
                  CIVIL.6574 = sum(b23001074, b23001160, b23001079, b23001165),
                  EMP.6574 = sum(b23001075, b23001161, b23001080, b23001166),
                  UNEMP.6574 = sum(b23001076, b23001162, b23001081, b23001167),
                  NLF.6574 = sum(b23001077, b23001163, b23001082, b23001168),
                  
                  TOT.75 = sum(b23001083, b23001169),
                  LF.75 = sum(b23001084, b23001170),
                  CIVIL.75 = sum(b23001084, b23001170),
                  EMP.75 = sum(b23001085, b23001171),
                  UNEMP.75 = sum(b23001086, b23001172),
                  NLF.75 = sum(b23001087, b23001173)
              ) %>%
           mutate(
              LF.1619P = LF.1619/TOT.1619,
              EMP.1619P = EMP.1619/CIVIL.1619,
              UNEMP.1619P = UNEMP.1619/CIVIL.1619,
              NLF.1619P = NLF.1619/TOT.1619,
              
              LF.2024P = LF.2024/TOT.2024,
              EMP.2024P = EMP.2024/CIVIL.2024,
              UNEMP.2024P = UNEMP.2024/CIVIL.2024,
              NLF.2024P = NLF.2024/TOT.2024,

              LF.2544P = LF.2544/TOT.2544,
              EMP.2544P = EMP.2544/CIVIL.2544,
              UNEMP.2544P = UNEMP.2544/CIVIL.2544,
              NLF.2544P = NLF.2544/TOT.2544,

              LF.4554P = LF.4554/TOT.4554,
              EMP.4554P = EMP.4554/CIVIL.4554,
              UNEMP.4554P = UNEMP.4554/CIVIL.4554,
              NLF.4554P = NLF.4554/TOT.4554,
              
              LF.5559P = LF.5559/TOT.5559,
              EMP.5559P = EMP.5559/CIVIL.5559,
              UNEMP.5559P = UNEMP.5559/CIVIL.5559,
              NLF.5559P = NLF.5559/TOT.5559,
              
              LF.6064P = LF.6064/TOT.6064,
              EMP.6064P = EMP.6064/CIVIL.6064,
              UNEMP.6064P = UNEMP.6064/CIVIL.6064,
              NLF.6064P = NLF.6064/TOT.6064,
              
              LF.6574P = LF.6574/TOT.6574,
              EMP.6574P = EMP.6574/CIVIL.6574,
              UNEMP.6574P = UNEMP.6574/CIVIL.6574,
              NLF.6574P = NLF.6574/TOT.6574,
              
              LF.75P = LF.75/TOT.75,
              EMP.75P = EMP.75/CIVIL.75,
              UNEMP.75P = UNEMP.75/CIVIL.75,
              NLF.75P = NLF.75/TOT.75       )
     
    f.ctyEMP_agy$geoname <- listID$plName1
    f.ctyEMP_agy$county <- "1000"

    f.ctyEMP_agy <- f.ctyEMP_agy[,c("geoname", "county", "TOT.1619", "LF.1619", "CIVIL.1619", "EMP.1619", "UNEMP.1619", "NLF.1619", 
                                "LF.1619P", "EMP.1619P", "UNEMP.1619P", "NLF.1619P", "TOT.2024", "LF.2024", "CIVIL.2024", "EMP.2024", 
                                "UNEMP.2024", "NLF.2024", "LF.2024P", "EMP.2024P", "UNEMP.2024P", "NLF.2024P", "TOT.2544", "LF.2544", 
                                "CIVIL.2544", "EMP.2544", "UNEMP.2544", "NLF.2544", "LF.2544P", "EMP.2544P", "UNEMP.2544P", "NLF.2544P", 
                                "TOT.4554", "LF.4554", "CIVIL.4554", "EMP.4554", "UNEMP.4554", "NLF.4554", "LF.4554P", "EMP.4554P", 
                                "UNEMP.4554P", "NLF.4554P", "TOT.5559", "LF.5559", "CIVIL.5559", "EMP.5559", "UNEMP.5559", "NLF.5559", 
                                "LF.5559P", "EMP.5559P", "UNEMP.5559P", "NLF.5559P", "TOT.6064", "LF.6064", "CIVIL.6064", "EMP.6064", 
                                "UNEMP.6064", "NLF.6064", "LF.6064P", "EMP.6064P", "UNEMP.6064P", "NLF.6064P", "TOT.6574", "LF.6574", 
                                "CIVIL.6574", "EMP.6574", "UNEMP.6574", "NLF.6574", "LF.6574P", "EMP.6574P", "UNEMP.6574P", "NLF.6574P", 
                                "TOT.75", "LF.75", "CIVIL.75", "EMP.75", "UNEMP.75", "NLF.75", "LF.75P", "EMP.75P", 
                                "UNEMP.75P", "NLF.75P")]
    
   f.ctyEMP_cty <- bind_rows(f.ctyEMP_agy, f.ctyEMP_cty)
   
}
 f.ctyEMP_cty$geoname <- sub(", Colorado","",f.ctyEMP_cty$geoname)  
 
 ctyList <- as.list(unique(f.ctyEMP_cty$geoname))
   
 # preparing files
     f.ctyEMP_tot <- f.ctyEMP_cty[, c(1,2, 3:8,  13:18, 23:28, 33:38, 43:48, 53:58, 63:68, 73:78)]
     f.ctyEMP_pct <- f.ctyEMP_cty[,c(1, 2, 9:12, 19:22, 29:32, 39:42, 49:52, 59:62, 69:72, 79:82)]
     
     f.ctyEMPL_tot <- f.ctyEMP_tot %>% 
          gather(var, count, TOT.1619:NLF.75, factor_key=TRUE) %>%
          separate(var,c("type","age_cat")) %>% arrange(age_cat)
     
      f.ctyEMPL_pct <- f.ctyEMP_pct %>% 
          gather(var, pct, LF.1619P:NLF.75P, factor_key=TRUE) %>%
          separate(var,c("type","age_cat")) %>% arrange(age_cat)

# Revising Type 
    f.ctyEMPL_tot$type <- plyr::revalue(f.ctyEMPL_tot$type,  c("LF"="In Labor Force",
                         "NLF" = "Not in Labor Force",
                         "EMP" = "Employed",
                         "UNEMP" = "Unemployed",
                         "CIVIL" = "Civilian Population in Labor Force",
                         "TOT" = "Total"))
  
    
   f.ctyEMPL_pct$type <- plyr::revalue(f.ctyEMPL_pct$type,  c("LF"="In Labor Force",
                         "NLF" = "Not in Labor Force",
                         "EMP" = "Employed",
                         "UNEMP" = "Unemployed"))
   
 

    
# revising Age Cat
    f.ctyEMPL_tot$age_cat <-plyr::revalue(f.ctyEMPL_tot$age_cat, c("1619" = "16 to 19",
                             "2024" = "20 to 24",
                             "2544" = "25 to 44",
                             "4554" = "45 to 55",
                             "5559" = "55 to 59",
                             "6064" = "60 to 64",
                             "6574" =  "65 to 74", 
                             "75" = "75+"))
    
    f.ctyEMPL_pct$age_cat <-plyr::revalue(f.ctyEMPL_pct$age_cat, c("1619P" = "16 to 19",
                             "2024P" = "20 to 24",
                             "2544P" = "25 to 44",
                             "4554P" = "45 to 55",
                             "5559P" = "55 to 59",
                             "6064P" = "60 to 64",
                             "6574P" =  "65 to 74", 
                             "75P" = "75+"))

    
  

    # Plotly  
    f.ctyEMPL_LF <- f.ctyEMPL_pct[which(f.ctyEMPL_pct$type == "In Labor Force"),]
    
    f.ctyEMPL_LF$indText  <- paste0( f.ctyEMPL_LF$geoname," Age Category: ", f.ctyEMPL_LF$age_cat," ",percent( f.ctyEMPL_LF$pct * 100))  
    grTitleLF <- paste0("Table 2a: Age Distribution by Percentage in Labor Force, ",listID$plName1)
    
    f.ctyEMPL_plt <- f.ctyEMPL_pct[which(f.ctyEMPL_pct$type == "Unemployed"),]
    
     f.ctyEMPL_plt$indText  <- paste0( f.ctyEMPL_plt$geoname," Age Category: ", f.ctyEMPL_plt$age_cat," ",percent( f.ctyEMPL_plt$pct * 100))  
    grTitle <- paste0("Table 2b: Age Distribution by Percentage Unemployed, ",listID$plName1)

# People in Labor Force
if(length(ctyfips) > 1 ){
 LFPlot <- f.ctyEMPL_LF %>%
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
        value = unique(f.ctyEMPL_LF$geoname)[1]
      )
  )) %>% layout( title=grTitleLF, yaxis = list(title = 'Percent',tickformat = "%"), xaxis=list(title='Age Category'),
          showlegend = FALSE,
    updatemenus = list(
      list(
        type = 'dropdown',
        active = 0,
        buttons = list(
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyEMPL_LF$geoname)[1]),
               label = unique(f.ctyEMPL_LF$geoname)[1]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyEMPL_LF$geoname)[2]),
               label = unique(f.ctyEMPL_LF$geoname)[2]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyEMPL_LF$geoname)[3]),
               label = unique(f.ctyEMPL_LF$geoname)[3]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyEMPL_LF$geoname)[4]),
               label = unique(f.ctyEMPL_LF$geoname)[4]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyEMPL_LF$geoname)[5]),
               label = unique(f.ctyEMPL_LF$geoname)[5]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyEMPL_LF$geoname)[6]),
               label = unique(f.ctyEMPL_LF$geoname)[6]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyEMPL_LF$geoname)[6]),
               label = unique(f.ctyEMPL_LF$geoname)[7])
      )
  )))
} else {
   LFPlot <- f.ctyEMPL_LF %>%
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
        value = unique(f.ctyEMPL_LF$geoname)[1]
      )
  ))   %>% layout( title=grTitleLF, yaxis = list(title = 'Percent',tickformat = "%"), xaxis=list(title='Age Category'),
          showlegend = FALSE)
}    
    
# Unemployed        
if(length(ctyfips) > 1 ){
 UEMPPlot <- f.ctyEMPL_plt %>%
  plot_ly(
    type = 'bar', 
    x = ~age_cat, 
    y = ~pct,
   # color=~type,
    text = ~indText,
    hoverinfo = 'text',
    transforms = list(
      list(
        type = 'filter',
        target = ~geoname,
        operation = '=',
        value = unique(f.ctyEMPL_pct$geoname)[1]
      )
  )) %>% layout( title=grTitle, yaxis = list(title = 'Percent',tickformat = "%"), xaxis=list(title='Age Category'),
          showlegend = FALSE,
    updatemenus = list(
      list(
        type = 'dropdown',
        active = 0,
        buttons = list(
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyEMPL_pct$geoname)[1]),
               label = unique(f.ctyEMPL_pct$geoname)[1]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyEMPL_pct$geoname)[2]),
               label = unique(f.ctyEMPL_pct$geoname)[2]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyEMPL_pct$geoname)[3]),
               label = unique(f.ctyEMPL_pct$geoname)[3]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyEMPL_pct$geoname)[4]),
               label = unique(f.ctyEMPL_pct$geoname)[4]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyEMPL_pct$geoname)[5]),
               label = unique(f.ctyEMPL_pct$geoname)[5]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyEMPL_pct$geoname)[6]),
               label = unique(f.ctyEMPL_pct$geoname)[6]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(f.ctyEMPL_pct$geoname)[6]),
               label = unique(f.ctyEMPL_pct$geoname)[7])
      )
  )))
} else {
   UEMPPlot <- f.ctyEMPL_plt %>%
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
        value = unique(f.ctyEMPL_pct$geoname)[1]
      )
  ))   %>% layout( title=grTitle, yaxis = list(title = 'Percent',tickformat = "%"), xaxis=list(title='Age Category'),
          showlegend = FALSE)
}

   
    # flex Table and output data file
    typeList <- c("In Labor Force",
                  "Civilian Population in Labor Force",
                   "Employed",
                   "Unemployed",
                   "Not in Labor Force", "Total")
                         
    f.ctyEMPL_tot$count <- format(round(f.ctyEMPL_tot$count ,digits=0),  big.mark=",")
    f.ctyEMPL_pct$pct <- percent(f.ctyEMPL_pct$pct * 100)
    
     f.ctyEMPL_tot$type2 <- "Count"
     f.ctyEMPL_pct$type2 <- "Percentage"
    
    f.ctyEMP_Count <-  f.ctyEMPL_tot %>% spread(age_cat,count)
    f.ctyEMP_Percent <-  f.ctyEMPL_pct %>% spread(age_cat,pct)
    
    f.ctyEMP_tab <- bind_rows(f.ctyEMP_Count,f.ctyEMP_Percent)
    # reordering Records for Table
    
    f.ctyEMP_tab  <- f.ctyEMP_tab %>% arrange(factor(geoname, levels = ctyList),  
                                              factor(type, levels = typeList), desc(type2))
    
    f.ctyEMP_tab$type <- ifelse(lag(f.ctyEMP_tab$type) == f.ctyEMP_tab$type,"",f.ctyEMP_tab$type)
    f.ctyEMP_tab$type <- ifelse(is.na(f.ctyEMP_tab$type),"In Labor Force",f.ctyEMP_tab$type)
    
    #Clearing geoname
    f.ctyEMP_tab <- clrGeoname(f.ctyEMP_tab,ctyfips,10)
     
    names(f.ctyEMP_tab)[1] <- "Agency/County"
    names(f.ctyEMP_tab)[3] <- "Employment Status"
    names(f.ctyEMP_tab)[4] <- "Value"
    
     #Producing Flextable
 
 tab_head <- paste0("Table 2: Age Distribution by Employment Status, ",listID$plName1)
 
 f.ctyEMP_tab <-  f.ctyEMP_tab[,c(1,3:12)]
  
   
   f.flexEMP <- flextable(
       f.ctyEMP_tab,
       col_keys = names(f.ctyEMP_tab)) %>%
       add_header_row(values=tab_head,top=TRUE,colwidths=11) %>%
       add_footer_row(values=captionSrc("ACS",ACS,"B23001"),top=FALSE,colwidths=11) %>%
       align(j=1:2, align="left", part="body") 
 
 
 
  
 
 
  
  
  outList <- list("LFPlot" = LFPlot, "UEMPPlot" =  UEMPPlot,"FlexTable" = f.flexEMP, "data" = f.ctyEMP_tab)
  return(outList)
}


