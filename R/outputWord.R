#' outputWord  Generates a word document for dowload
#'
#' outputWord is the server function that facilitates the download
#'
#' @param chkList is the list of topical areas selected
#' @param locList the list of selected locations (fips code)
#' @param lvl the selected location name
#' @param outputMat the matrix of data objects
#' @export

outputWord <- function(chkList, locList, lvl, outputMat) {

  ctyfips <- as.character(as.numeric(substr(locList$list1,3,5)))
  # Adding entry for multi county agency
  if(length(ctyfips) > 1) {
    ctyfips <- c("0",ctyfips)
  }
  
  base <- 10
  outDoc <- read_docx()
  outDoc <- outDoc  %>% 
      body_add_par(paste0("CSBG Information for ",lvl), style = "heading 1", pos = "after")
  
  if("age" %in% chkList) {
    age_list <- outputMat[[1,1]]
    age_data <- age_list[[1]]$data
    age_table <- age_list[[1]]$FlexTable
    age_caption <- age_list[[1]]$caption
    age_title <- unlist(outputMat[[1,2]])
    
    names(age_data) <- c("county","fips","agecat","Count","Percentage")
    age_data$Percentage <- as.numeric(sub("%","",age_data$Percentage))
    
    # Output table and text
        outDoc <- outDoc  %>% 
      body_add_par(age_title, style = "heading 2", pos = "after") %>%
      body_add_flextable(value = age_table) %>%
      body_add_par("",style = NULL, pos = "after") 
      
          
    # Creating ggplot
    for(i in 1:length(ctyfips)) {
       age_data2 <- age_data[which(age_data$fips == ctyfips[i] & age_data$agecat != "Total"),]
       age_data2$agecat <- factor(age_data2$agecat, levels=c("00 to 04", "05 to 17", "18 to 64", "65+"))
       
       maxLim <- max(age_data2$Percentage) + 20
       
       LocName <- unique(age_data2$county)
       ggimg1 <-ggplot(age_data2, aes(x=agecat, y=Percentage)) +
       geom_bar(stat="identity", position="dodge", color="black", fill="blue")+
             scale_y_continuous(limits = c(0, maxLim), label=percent, expand = c(0, 0)) +
              theme_codemog(base_size=base) +
              labs(title = age_title,
                   subtitle = LocName,
                   caption = age_caption,
                   x = "Age Category",
                   y= "Percentage") +
              theme(plot.title = element_text(hjust = 0.5, size=16),
                    panel.background = element_rect(fill = "white", colour = "gray50"),
                    panel.grid.major = element_line(colour = "gray80"),
                    axis.text.x = element_text(size=10),
                    axis.text.y=element_text(size=12),
                    legend.position= "none")
      
       outDoc <- outDoc %>% body_add_gg(value = ggimg1, style = "centered", width = 6, height = 4) %>%
                            body_add_par("",style = NULL, pos = "after")
         
       }
    outDoc <- outDoc  %>%  body_add_break(pos = "after")
  }

  if("ageemp" %in% chkList) {
    ageemp_list <- outputMat[[2,1]]
    ageemp_data <- ageemp_list[[1]]$data
    ageemp_table <- ageemp_list[[1]]$FlexTable
    ageemp_caption <- ageemp_list[[1]]$caption
    ageemp_title <- unlist(outputMat[[2,2]])
    ageemp_title2 <- "Age Distribution by Percentage\nin Civilian Labor Force"
    ageemp_title3 <- "Age Distribution by Percentage Unemployed"
    
    ageemp_data$pct <- as.numeric(sub("%","",ageemp_data$pct))
    
    # Output table and text
      outDoc <- outDoc  %>% 
      body_add_par(ageemp_title, style = "heading 2", pos = "after") %>%
      body_add_flextable(value = ageemp_table) %>%
      body_add_par("",style = NULL, pos = "after")
          
    # Creating ggplot
    for(i in 1:length(ctyfips)) {
       ageemp_data2 <- ageemp_data[which(ageemp_data$county == ctyfips[i] & ageemp_data$type == "In Civilian Labor Force"),]
       ageemp_data3 <- ageemp_data[which(ageemp_data$county == ctyfips[i] & ageemp_data$type == "Unemployed"),]
       
       ageemp_data2$type <- factor(ageemp_data2$type, levels=c("16 to 19", "20 to 64", "65+"))
       ageemp_data3$type <- factor(ageemp_data3$type, levels=c("16 to 19", "20 to 64", "65+"))
       
       maxLim2 <- max(ageemp_data2$pct) + 20
       maxLim3 <- max(ageemp_data3$pct) + 20
       
       LocName <- unique(ageemp_data2$geoname)
       ggimg2a <-ggplot(ageemp_data2, aes(x=age_cat, y=pct)) +
       geom_bar(stat="identity", position="dodge", color="black", fill="blue")+
             scale_y_continuous(limits = c(0, maxLim2), label=percent, expand = c(0, 0)) +
              theme_codemog(base_size=base) +
              labs(title = ageemp_title2,
                   subtitle = LocName,
                   caption = ageemp_caption,
                   x = "Age Category",
                   y= "Percentage") +
              theme(plot.title = element_text(hjust = 0.5, size=16),
                    panel.background = element_rect(fill = "white", colour = "gray50"),
                    panel.grid.major = element_line(colour = "gray80"),
                    axis.text.x = element_text(size=10),
                    axis.text.y=element_text(size=12),
                    legend.position= "none")
       
       ggimg2b <-ggplot(ageemp_data3, aes(x=age_cat, y=pct)) +
       geom_bar(stat="identity", position="dodge", color="black", fill="blue")+
             scale_y_continuous(limits = c(0, maxLim3), label=percent, expand = c(0, 0)) +
              theme_codemog(base_size=base) +
              labs(title = ageemp_title3,
                   subtitle = LocName,
                   caption = ageemp_caption,
                   x = "Age Category",
                   y= "Percentage") +
              theme(plot.title = element_text(hjust = 0.5, size=16),
                    panel.background = element_rect(fill = "white", colour = "gray50"),
                    panel.grid.major = element_line(colour = "gray80"),
                    axis.text.x = element_text(size=10),
                    axis.text.y=element_text(size=12),
                    legend.position= "none")
      
       outDoc <- outDoc %>% body_add_gg(value = ggimg2a, style = "centered", width = 6, height = 4) %>%
                            body_add_par("",style = NULL, pos = "after") %>%
                            body_add_gg(value = ggimg2b, style = "centered", width = 6, height = 4) %>%
                            body_add_par("",style = NULL, pos = "after")
         
       }
    outDoc <- outDoc  %>%  body_add_break(pos = "after")
  }   
 
 if("pov" %in% chkList) {
    pov3_list <- outputMat[[3,1]]
    pov3_data <- pov3_list[[1]]$data
    pov3_table <- pov3_list[[1]]$FlexTable
    pov3_caption <- pov3_list[[1]]$caption
    pov3_title <- unlist(outputMat[[3,2]])
    
    pov3W_data <- gather(pov3_data, pov_cat, pct, POV.LT50:POV.GE200, factor_key=TRUE)

    pov3W_data$pct <- as.numeric(sub("%","",pov3W_data$pct))
    
    pov3W_data$pov_cat <- plyr::revalue(pov3W_data$pov_cat,
                                c("POV.LT50" = "Less than 50%","POV.50124" = "50 to 124%",
                                  "POV.125199" = "125 to 199%", "POV.GE200" = "200% and Higher"))
    pov3W_data$pov_cat <- factor(pov3W_data$pov_cat, 
                                      levels = c("Less than 50%", "50 to 124%",
                                   "125 to 199%", "200% and Higher"))
    
    # Output table and text
      outDoc <- outDoc  %>% 
      body_add_par(pov3_title, style = "heading 2", pos = "after") %>%
      body_add_flextable(value = pov3_table) %>%
      body_add_par("",style = NULL, pos = "after")
          
    # Creating ggplot
    for(i in 1:length(ctyfips)) {
       pov3W_data2 <- pov3W_data[which(pov3W_data$county == ctyfips[i]),]
       
       maxLim <- max(pov3W_data2$pct) + 20
       
       LocName <- unique(pov3W_data2$geoname)
       ggimg3 <-ggplot(pov3W_data2, aes(x=pov_cat, y=pct)) +
       geom_bar(stat="identity", position="dodge", color="black", fill="blue")+
             scale_y_continuous(limits = c(0, maxLim), label=percent, expand = c(0, 0)) +
              theme_codemog(base_size=base) +
              labs(title = pov3_title,
                   subtitle = LocName,
                   caption = pov3_caption,
                   x = "Percentage of Federal Poverty Level",
                   y= "Percentage") +
              theme(plot.title = element_text(hjust = 0.5, size=16),
                    panel.background = element_rect(fill = "white", colour = "gray50"),
                    panel.grid.major = element_line(colour = "gray80"),
                    axis.text.x = element_text(size=10),
                    axis.text.y=element_text(size=12),
                    legend.position= "none")
       
       outDoc <- outDoc %>% body_add_gg(value = ggimg3, style = "centered", width = 6, height = 4) %>%
                            body_add_par("",style = NULL, pos = "after") 
       }
    outDoc <- outDoc  %>%  body_add_break(pos = "after")
  }  
  
 if("educatt" %in% chkList) {
  
    educatt_list <- outputMat[[4,1]]
    educatt_data <- educatt_list[[1]]$data
    educatt_table <- educatt_list[[1]]$FlexTable
    educatt_caption <- educatt_list[[1]]$caption
    educatt_title <- unlist(outputMat[[4,2]])
    
    # Output table and text
      outDoc <- outDoc  %>% 
      body_add_par(educatt_title, style = "heading 2", pos = "after") %>%
      body_add_flextable(value = educatt_table) %>%
      body_add_par("",style = NULL, pos = "after")
          
    # Creating ggplot
    for(i in 1:length(ctyfips)) {
       educatt_data2 <- educatt_data[which(educatt_data$county == ctyfips[i] & educatt_data$educatt != "TOT"),]
       
       educatt_data2$lvl  <- plyr::revalue(educatt_data2$lvl, c("TOT"  = "All Persons", "POV" = "Persons Below FPL"))
       educatt_data2$lvl <- factor(educatt_data2$lvl, levels=c("Persons Below FPL","All Persons"))
    
       educatt_data2$educatt <- plyr::revalue(educatt_data2$educatt,  c("LTHS"="Less Than\nHigh School",
                         "HSGRAD" = "High School\nGraduate",
                         "COLL" = "Some College,\nAssociates Degree",
                         "BA" = "Bachelor's Degree\nor Higher"))
       educatt_data2$educatt <-  factor(educatt_data2$educatt, levels=c("Less Than\nHigh School",
                 "High School\nGraduate",
                  "Some College,\nAssociates Degree",
                    "Bachelor's Degree\nor Higher")) 
       educatt_data2$value <- educatt_data2$value * 100
       maxLim <- max(educatt_data2$value) + 20
       
       LocName <- unique(educatt_data2$geoname)
       ggimg4 <-ggplot(educatt_data2, aes(x=educatt, y=value, fill=lvl)) +
       geom_bar(stat="identity", position="dodge")+
             scale_fill_manual(values=c("#00953A","#6EC4E8"),
                      name="Poverty Level") +
             scale_y_continuous(limits = c(0, maxLim), label=percent, expand = c(0, 0)) +
              theme_codemog(base_size=base) +
              labs(title = educatt_title,
                   subtitle = LocName,
                   caption = educatt_caption,
                   x = "Educational Attainment",
                   y= "Percentage") +
              theme(plot.title = element_text(hjust = 0.5, size=16),
                    panel.background = element_rect(fill = "white", colour = "gray50"),
                    panel.grid.major = element_line(colour = "gray80"),
                    axis.text.x = element_text(size=10),
                    axis.text.y=element_text(size=12))
       
       outDoc <- outDoc %>% body_add_gg(value = ggimg4, style = "centered", width = 6, height = 4) %>%
                            body_add_par("",style = NULL, pos = "after") 
       }
    outDoc <- outDoc  %>%  body_add_break(pos = "after")
 } 
  
 if("povage" %in% chkList) {
    pov5_list <- outputMat[[5,1]]
    pov5_data <- pov5_list[[1]]$data
    pov5_table <- pov5_list[[1]]$FlexTable
    pov5_caption <- pov5_list[[1]]$caption
    pov5_title <- unlist(outputMat[[5,2]])
    
    # Output table and text
      outDoc <- outDoc  %>% 
      body_add_par(pov5_title, style = "heading 2", pos = "after") %>%
      body_add_flextable(value = pov5_table) %>%
      body_add_par("",style = NULL, pos = "after")
          
    # Creating ggplot
    for(i in 1:length(ctyfips)) {
       pov5_data2 <- pov5_data[which(pov5_data$fips == ctyfips[i]),]
       pov5_data2$age_cat <- factor(pov5_data2$age_cat , levels= c("5 to 17", "18 and Older", "Total"))
      
       pov5_data2$value <- pov5_data2$value * 100
       maxLim <- max(pov5_data2$value) + 20
       
       LocName <- unique(pov5_data2$geoname)
       ggimg5 <-ggplot(pov5_data2, aes(x=age_cat, y=value)) +
       geom_bar(stat="identity", position="dodge",  color="black", fill="blue")+
             scale_y_continuous(limits = c(0, maxLim), label=percent, expand = c(0, 0)) +
              theme_codemog(base_size=base) +
              labs(title = pov5_title,
                   subtitle = LocName,
                   caption = pov5_caption,
                   x = "Age Category",
                   y= "Percentage") +
              theme(plot.title = element_text(hjust = 0.5, size=16),
                    panel.background = element_rect(fill = "white", colour = "gray50"),
                    panel.grid.major = element_line(colour = "gray80"),
                    axis.text.x = element_text(size=10),
                    axis.text.y=element_text(size=12))
       
       outDoc <- outDoc %>% body_add_gg(value = ggimg5, style = "centered", width = 6, height = 4) %>%
                            body_add_par("",style = NULL, pos = "after") 
       }
    outDoc <- outDoc  %>%  body_add_break(pos = "after")
 } 
  
 if("povagetr" %in% chkList) {
    pov6_list <- outputMat[[6,1]]
    pov6_data <- pov6_list[[1]]$data
    pov6_table <- pov6_list[[1]]$FlexTable
    pov6_caption <- pov6_list[[1]]$caption
    pov6_title <- unlist(outputMat[[6,2]])
    
    # Output table and text
      outDoc <- outDoc  %>% 
      body_add_par(pov6_title, style = "heading 2", pos = "after") %>%
      body_add_flextable(value = pov6_table) %>%
      body_add_par("",style = NULL, pos = "after")
          
    # Creating ggplot
    for(i in 1:length(ctyfips)) {
       pov6_data2 <- pov6_data[which(pov6_data$fips == ctyfips[i]),]
       pov6_data2$age_cat <- factor(pov6_data2$age_cat , levels= c("5 to 17", "18 and Older", "All Persons"))
      
       pov6_data2$value <- pov6_data2$value * 100
       maxLim <- max(pov6_data2$value) + 10
       
       LocName <- unique(pov6_data2$geoname)
       ggimg6 <-ggplot(pov6_data2, aes(x=year, y=value, color=age_cat)) +
       geom_line(size=1.2) +
             scale_color_manual(values=c("blue","orange","green"), name="Age Category") +
             scale_y_continuous(limits = c(0, maxLim), label=percent, expand = c(0, 0)) +
              theme_codemog(base_size=base) +
              labs(title = pov6_title,
                   subtitle = LocName,
                   caption = pov6_caption,
                   x = "Year",
                   y= "Percentage") +
              theme(plot.title = element_text(hjust = 0.5, size=16),
                    panel.background = element_rect(fill = "white", colour = "gray50"),
                    panel.grid.major = element_line(colour = "gray80"),
                    axis.text.x = element_text(size=10),
                    axis.text.y=element_text(size=12))
       
       outDoc <- outDoc %>% body_add_gg(value = ggimg6, style = "centered", width = 6, height = 4) %>%
                            body_add_par("",style = NULL, pos = "after") 
       }
    outDoc <- outDoc  %>%  body_add_break(pos = "after")
 } 
  
 if("povagedis" %in% chkList) {
    dis7_list <- outputMat[[7,1]]
    dis7_data <- dis7_list[[1]]$data
    dis7_table <- dis7_list[[1]]$FlexTable
    dis7_caption <- dis7_list[[1]]$caption
    dis7_title <- unlist(outputMat[[7,2]])
    
    # Output table and text
      outDoc <- outDoc  %>% 
      body_add_par(dis7_title, style = "heading 2", pos = "after") %>%
      body_add_flextable(value = dis7_table) %>%
      body_add_par("",style = NULL, pos = "after")
          
    # Creating ggplot
    for(i in 1:length(ctyfips)) {
       dis7_data2 <- dis7_data[which(dis7_data$county == ctyfips[i] & dis7_data$meas == "Below Poverty Level" ),]
       dis7_data2$age_cat <- factor(dis7_data2$age_cat , levels= c("Under 18", "18 to 64", "65+", "Total"))
      
       dis7_data2$pct <- as.numeric(sub("%","",dis7_data2$pct))
       
       maxLim <- max(dis7_data2$pct) + 20
       
       LocName <- unique(dis7_data2$geoname)
       ggimg7 <-ggplot(dis7_data2, aes(x=age_cat, y=pct)) +
       geom_bar(stat="identity", position="dodge",  color="black", fill="blue")+
             scale_y_continuous(limits = c(0, maxLim), label=percent, expand = c(0, 0)) +
              theme_codemog(base_size=base) +
              labs(title = dis7_title,
                   subtitle = LocName,
                   caption = dis7_caption,
                   x = "Age Category",
                   y= "Percentage") +
              theme(plot.title = element_text(hjust = 0.5, size=16),
                    panel.background = element_rect(fill = "white", colour = "gray50"),
                    panel.grid.major = element_line(colour = "gray80"),
                    axis.text.x = element_text(size=10),
                    axis.text.y=element_text(size=12))
       
       outDoc <- outDoc %>% body_add_gg(value = ggimg7, style = "centered", width = 6, height = 4) %>%
                            body_add_par("",style = NULL, pos = "after") 
       }
    outDoc <- outDoc  %>%  body_add_break(pos = "after")
 }
  

if("hhpov" %in% chkList) {
    fam8_list <- outputMat[[8,1]]
    fam8_data <- fam8_list[[1]]$data
    fam8_table <- fam8_list[[1]]$FlexTable
    fam8_caption <- fam8_list[[1]]$caption
    fam8_title <- unlist(outputMat[[8,2]])
    
    # Output table and text
      outDoc <- outDoc  %>% 
      body_add_par(fam8_title, style = "heading 2", pos = "after") %>%
      body_add_flextable(value = fam8_table) %>%
      body_add_par("",style = NULL, pos = "after")
          
    # Creating ggplot
    for(i in 1:length(ctyfips)) {
       fam8_data2 <- fam8_data[which(fam8_data$county == ctyfips[i]),]
       
       fam8_data2$pct <- fam8_data2$pct * 100
       
       maxLim <- max(fam8_data2$pct) + 20
       
       LocName <- unique(fam8_data2$geoname)
      ggimg8 <-ggplot(fam8_data2, aes(x=famtype, y=pct, fill=kids)) +
       geom_bar(stat="identity", position="dodge")+
             scale_fill_manual(values=c("#00953A","#6EC4E8"),
                      name="Presence of Children") +
             scale_y_continuous(limits = c(0, maxLim), label=percent, expand = c(0, 0)) +
              theme_codemog(base_size=base) +
              labs(title = fam8_title,
                   subtitle = LocName,
                   caption = fam8_caption,
                   x = "Family Type",
                   y= "Percentage") +
              theme(plot.title = element_text(hjust = 0.5, size=16),
                    panel.background = element_rect(fill = "white", colour = "gray50"),
                    panel.grid.major = element_line(colour = "gray80"),
                    axis.text.x = element_text(size=10),
                    axis.text.y=element_text(size=12))
       
       outDoc <- outDoc %>% body_add_gg(value = ggimg8, style = "centered", width = 6, height = 4) %>%
                            body_add_par("",style = NULL, pos = "after") 
       }
    outDoc <- outDoc  %>%  body_add_break(pos = "after")
}
  
 if("tenure" %in% chkList) {
    hh9_list <- outputMat[[9,1]]
    hh9_data <- hh9_list[[1]]$data
    hh9_table <- hh9_list[[1]]$FlexTable
    hh9_caption <- hh9_list[[1]]$caption
    hh9_title <- unlist(outputMat[[9,2]])
    
    # Output table and text
      outDoc <- outDoc  %>% 
      body_add_par(hh9_title, style = "heading 2", pos = "after") %>%
      body_add_flextable(value = hh9_table) %>%
      body_add_par("",style = NULL, pos = "after")
          
    # Creating ggplot
    for(i in 1:length(ctyfips)) {
       hh9_data2 <- hh9_data[which(hh9_data$county == ctyfips[i] ),]
       
       hh9_data2$pct <- hh9_data2$pct * 100
       
       maxLim <- max(hh9_data2$pct) + 20
       
       LocName <- unique(hh9_data2$geoname)
      ggimg9 <-ggplot(hh9_data2, aes(x=famtype, y=pct, fill=tenure)) +
       geom_bar(stat="identity", position="dodge")+
             scale_fill_manual(values=c("#00953A","#6EC4E8"),
                      name="Housing Tenure") +
             scale_y_continuous(limits = c(0, maxLim), label=percent, expand = c(0, 0)) +
              theme_codemog(base_size=base) +
              labs(title = hh9_title,
                   subtitle = LocName,
                   caption = hh9_caption,
                   x = "Family Type",
                   y= "Percentage") +
              theme(plot.title = element_text(hjust = 0.5, size=16),
                    panel.background = element_rect(fill = "white", colour = "gray50"),
                    panel.grid.major = element_line(colour = "gray80"),
                    axis.text.x = element_text(size=10),
                    axis.text.y=element_text(size=12))
       
       outDoc <- outDoc %>% body_add_gg(value = ggimg9, style = "centered", width = 6, height = 4) %>%
                            body_add_par("",style = NULL, pos = "after") 
       }
    outDoc <- outDoc  %>%  body_add_break(pos = "after")
 }

  
   if("snap" %in% chkList) {
    snap10_list <- outputMat[[10,1]]
    snap10_data <- snap10_list[[1]]$data
    snap10_table <- snap10_list[[1]]$FlexTable
    snap10_caption <- snap10_list[[1]]$caption
    snap10_title <- unlist(outputMat[[10,2]])
    snap10_title2 <- "Supplemental Nutrition Assistance Program\n(SNAP)"
    
    # Output table and text
      outDoc <- outDoc  %>% 
      body_add_par(snap10_title, style = "heading 2", pos = "after") %>%
      body_add_flextable(value = snap10_table) %>%
      body_add_par("",style = NULL, pos = "after")
          
    # Creating ggplot
    for(i in 1:length(ctyfips)) {
       snap10_data2 <- snap10_data[which(snap10_data$fips == ctyfips[i] ),]
       
       snap10_data2$pct <- snap10_data2$pct * 100
       
       maxLim <- max(snap10_data2$pct) + 20
     
       
       LocName <- unique(snap10_data2$county)
     ggimg10 <-ggplot(snap10_data2, aes(x=SNAP, y=pct)) +
       geom_bar(stat="identity", position="dodge",  color="black", fill="blue")+
             scale_y_continuous(limits = c(0, maxLim), label=percent, expand = c(0, 0)) +
              theme_codemog(base_size=base) +
              labs(title = snap10_title2,
                   subtitle = LocName,
                   caption = snap10_caption,
                   x = "Program Participation",
                   y= "Percentage") +
              theme(plot.title = element_text(hjust = 0.5, size=16),
                    panel.background = element_rect(fill = "white", colour = "gray50"),
                    panel.grid.major = element_line(colour = "gray80"),
                    axis.text.x = element_text(size=10),
                    axis.text.y=element_text(size=12))
       
       outDoc <- outDoc %>% body_add_gg(value = ggimg10, style = "centered", width = 6, height = 4) %>%
                            body_add_par("",style = NULL, pos = "after") 
       }
    outDoc <- outDoc  %>%  body_add_break(pos = "after")
   }
  
   if("wic" %in% chkList) {
    wic11_list <- outputMat[[11,1]]
    wic11_data <- wic11_list[[1]]$data
    wic11_table <- wic11_list[[1]]$FlexTable
    wic11_caption <- wic11_list[[1]]$caption
    wic11_title <- unlist(outputMat[[11,2]])
    
    # Output table and text
      outDoc <- outDoc  %>% 
      body_add_par(wic11_title, style = "heading 2", pos = "after") %>%
      body_add_flextable(value = wic11_table) %>%
      body_add_par("",style = NULL, pos = "after")
          
    # Creating ggplot
    for(i in 1:length(ctyfips)) {
       wic11_data2 <- wic11_data[which(wic11_data$fips == ctyfips[i] ),]
       
       wic11_data2$pct <- wic11_data2$pct * 100
       
       maxLim <- max(wic11_data2$pct) + 20
       
       LocName <- unique(wic11_data2$county)
     ggimg11 <-ggplot(wic11_data2, aes(x=WIC, y=pct)) +
       geom_bar(stat="identity", position="dodge",  color="black", fill="blue")+
             scale_y_continuous(limits = c(0, maxLim), label=percent, expand = c(0, 0)) +
              theme_codemog(base_size=base) +
              labs(title = wic11_title,
                   subtitle = LocName,
                   caption = wic11_caption,
                   x = "Program Participation",
                   y= "Percentage") +
              theme(plot.title = element_text(hjust = 0.5, size=16),
                    panel.background = element_rect(fill = "white", colour = "gray50"),
                    panel.grid.major = element_line(colour = "gray80"),
                    axis.text.x = element_text(size=10),
                    axis.text.y=element_text(size=12))
       
       outDoc <- outDoc %>% body_add_gg(value = ggimg11, style = "centered", width = 6, height = 4) %>%
                            body_add_par("",style = NULL, pos = "after") 
       }
    outDoc <- outDoc  %>%  body_add_break(pos = "after")
   }
  
 
  if("insurance" %in% chkList) {
    ins12_list <- outputMat[[12,1]]
    ins12_data <- ins12_list[[1]]$data
    ins12_table <- ins12_list[[1]]$FlexTable
    ins12_caption <- ins12_list[[1]]$caption
    ins12_title <- unlist(outputMat[[12,2]])
    
    # Output table and text
      outDoc <- outDoc  %>% 
      body_add_par(ins12_title, style = "heading 2", pos = "after") %>%
      body_add_flextable(value = ins12_table) %>%
      body_add_par("",style = NULL, pos = "after")
          
    # Creating ggplot
    for(i in 1:length(ctyfips)) {
       ins12_data2 <- ins12_data[which(ins12_data$fips == ctyfips[i] ),]
       
       ins12_data2$ins <- plyr::revalue(ins12_data2$ins, c( "Uninsured" = "Uninsured",
                                                       "Employer Sponsored" = "Employer\nSponsored",
                                                        "Medicaid" = "Medicaid",
                                                        "Individually Purchased" = "Individually\nPurchased" ,
                                                         "Child Health Plan Plus" = "Child Health\nPlan Plus"))
      ins12_data2$ins <- factor(ins12_data2$ins, levels = c( "Individually\nPurchased",
                                                           "Employer\nSponsored", 
                                                           "Medicaid",
                                                          "Uninsured",
                                                          "Child Health\nPlan Plus"))

       
       ins12_data2$pct <- ins12_data2$pct * 100
       
       maxLim <- max(ins12_data2$pct) + 20

       LocName <- unique(ins12_data2$county)
     ggimg12 <-ggplot(ins12_data2, aes(x=ins, y=pct)) +
       geom_bar(stat="identity", position="dodge",  color="black", fill="blue")+
             scale_y_continuous(limits = c(0, maxLim), label=percent, expand = c(0, 0)) +
              theme_codemog(base_size=base) +
              labs(title = ins12_title,
                   subtitle = LocName,
                   caption = ins12_caption,
                   x = "Type of Insurance",
                   y= "Percentage") +
              theme(plot.title = element_text(hjust = 0.5, size=16),
                    panel.background = element_rect(fill = "white", colour = "gray50"),
                    panel.grid.major = element_line(colour = "gray80"),
                    axis.text.x = element_text(size=10),
                    axis.text.y=element_text(size=12))
       
       outDoc <- outDoc %>% body_add_gg(value = ggimg12, style = "centered", width = 6, height = 4) %>%
                            body_add_par("",style = NULL, pos = "after") 
       }
    outDoc <- outDoc  %>%  body_add_break(pos = "after")
   }
   
  return(outDoc)
}
  