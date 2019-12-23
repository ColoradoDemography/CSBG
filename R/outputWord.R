#' outputWord  Generates a word document for dowload
#'
#' outputWord is the server function that facilitates the download
#'
#' @param chkList is the list of topical areas selected
#' @param locList the list of selected locations (fips code)
#' @param lvl the selected location name
#' @param outputMat the matrix of data objects
#' @export

outputWord <- function(chkList, locList, lvl, outputMat,fileMat) {

   x <- list()

  ctyfips <- as.character(as.numeric(substr(locList$list1,3,5)))
  # Adding entry for multi county agency
  if(length(ctyfips) > 1) {
    ctyfips <- c("0",ctyfips)
  }
  
  base <- 10
  
  if("age" %in% chkList) {
    grCount <- 4
    age_list <- outputMat[[1,1]]
    age_data <- age_list[[1]]$data
    age_caption <- age_list[[1]]$caption
    age_title <- unlist(outputMat[[1,2]])
    age_title <- paste0(age_title,",\n",locList$plName1)
    
    age_data$age_cat <- factor(age_data$age_cat, levels=c("00 to 04", "05 to 17", "18 to 64", "65+"))
    age_data$age_pct <- age_data$age_pct * 100
    
  for(i in 1:length(ctyfips)) {
    age_data2 <- age_data[which(age_data$county == ctyfips[i]),]
    maxLim <- max(age_data2$age_pct) + 20
    
    LocName <- unique(age_data2$geoname)
    ggimg1 <- age_data2 %>% ggplot(aes(x=age_cat, y=age_pct)) +
    geom_bar(stat="identity", position="dodge", color="black", fill="blue")+
          scale_y_continuous(limits = c(0, maxLim), label=percent, expand = c(0, 0)) +
           theme_codemog(base_size=base) +
           labs(title = age_title,
                subtitle = LocName,
                caption = age_caption,
                x = "Age Category",
                y= "Percentage") +
           theme(plot.title = element_text(hjust = 0.5, size=12),
                 plot.caption = element_text(hjust = 0, size=9),
                 panel.background = element_rect(fill = "white", colour = "gray50"),
                 panel.grid.major = element_line(colour = "gray80"),
                 axis.text.x = element_text(size=10),
                 axis.text.y=element_text(size=10))
    ggsave(fileMat[grCount],ggimg1, device="png", height = 3 , width = 7, dpi=300)
    grCount <- grCount + 1
    
}
  }

  if("ageemp" %in% chkList) {
    grCount <- 10
    ageemp_list <- outputMat[[2,1]]
    ageemp_data <- ageemp_list[[1]]$data
    ageemp_caption <- ageemp_list[[1]]$caption
    ageemp_title <- unlist(outputMat[[2,2]])
    ageemp_title2 <- paste0("Age Distribution by Percentage in Civilian Labor Force,\n",locList$plName1)
    ageemp_title3 <- paste0("Age Distribution by Percentage Unemployed,\n",locList$plName1)
    
    
    ageemp_data$age_cat <- factor(ageemp_data$age_cat, levels=c("16 to 19", "20 to 64", "65+"))
    ageemp_data$pct <- ageemp_data$pct * 100
      
    
    # Creating ggplot
   for(i in 1:length(ctyfips)) {
       ageemp_data2 <- ageemp_data[which(ageemp_data$county == ctyfips[i] & ageemp_data$type == "In Civilian Labor Force"),]
       ageemp_data3 <- ageemp_data[which(ageemp_data$county == ctyfips[i] & ageemp_data$type == "Unemployed"),]
       
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
              theme(plot.title = element_text(hjust = 0.5, size=12),
                 plot.caption = element_text(hjust = 0, size=9),
                    panel.background = element_rect(fill = "white", colour = "gray50"),
                    panel.grid.major = element_line(colour = "gray80"),
                    axis.text.x = element_text(size=10),
                    axis.text.y=element_text(size=10),
                    legend.position= "none")
           ggsave(fileMat[grCount],ggimg2a, device="png", height = 3 , width = 7, dpi=300)
           grCount <- grCount + 1
       
       ggimg2b <-ggplot(ageemp_data3, aes(x=age_cat, y=pct)) +
       geom_bar(stat="identity", position="dodge", color="black", fill="blue")+
             scale_y_continuous(limits = c(0, maxLim3), label=percent, expand = c(0, 0)) +
              theme_codemog(base_size=base) +
              labs(title = ageemp_title3,
                   subtitle = LocName,
                   caption = ageemp_caption,
                   x = "Age Category",
                   y= "Percentage") +
              theme(plot.title = element_text(hjust = 0.5, size=12),
                 plot.caption = element_text(hjust = 0, size=9),
                    panel.background = element_rect(fill = "white", colour = "gray50"),
                    panel.grid.major = element_line(colour = "gray80"),
                    axis.text.x = element_text(size=10),
                    axis.text.y=element_text(size=10),
                    legend.position= "none")
         ggsave(fileMat[grCount],ggimg2b, device="png", height = 3 , width = 7, dpi=300)
         grCount <- grCount + 1
    
}
  }   
 
 if("pov" %in% chkList) {
   grCount <- 22
    pov_list <- outputMat[[3,1]]
    pov_data <- pov_list[[1]]$data
    pov_caption <- pov_list[[1]]$caption
    pov_title <- unlist(outputMat[[3,2]])
    pov_title <- paste0(pov_title,",\n",locList$plName1)
    
 for(i in 1:length(ctyfips)) {
       pov_data2 <- pov_data[which(pov_data$county == ctyfips[i]),]
       
       pov_data2$pct <- pov_data2$value * 100
       maxLim <- max(pov_data2$pct) + 20
       
       LocName <- unique(pov_data2$geoname)
       ggimg3 <-ggplot(pov_data2, aes(x=POV.LEVEL, y=pct)) +
       geom_bar(stat="identity", position="dodge", color="black", fill="blue")+
             scale_y_continuous(limits = c(0, maxLim), label=percent, expand = c(0, 0)) +
              theme_codemog(base_size=base) +
              labs(title = pov_title,
                   subtitle = LocName,
                   caption = pov_caption,
                   x = "Percentage of Federal Poverty Level",
                   y= "Percentage") +
              theme(plot.title = element_text(hjust = 0.5, size=12),
                 plot.caption = element_text(hjust = 0, size=9),
                    panel.background = element_rect(fill = "white", colour = "gray50"),
                    panel.grid.major = element_line(colour = "gray80"),
                    axis.text.x = element_text(size=10),
                    axis.text.y=element_text(size=10),
                    legend.position= "none")
         ggsave(fileMat[grCount],ggimg3, device="png", height = 3 , width = 7, dpi=300)
         grCount <- grCount + 1
  }  
 }
  
 if("educatt" %in% chkList) {
  grCount <- 28
    educatt_list <- outputMat[[4,1]]
    educatt_data <- educatt_list[[1]]$data
    educatt_caption <- educatt_list[[1]]$caption
    educatt_title <- unlist(outputMat[[4,2]])
    educatt_title <- paste0(educatt_title,",\n",locList$plName1)
    
    educatt_data$educatt <- plyr::revalue(educatt_data$educatt,  
                        c("Less Than High School"="Less Than\nHigh School",
                         "High School Graduate" = "High School\nGraduate",
                         "Some College, Associates Degree" = "Some College,\nAssociates Degree",
                         "Bachelor's Degree or Higher" = "Bachelor's Degree\nor Higher"))
    educatt_data$educatt <-  factor(educatt_data$educatt, levels=c("Less Than\nHigh School",
                 "High School\nGraduate", "Some College,\nAssociates Degree",
                 "Bachelor's Degree\nor Higher")) 
    
   for(i in 1:length(ctyfips)) {
       educatt_data2 <- educatt_data[which(educatt_data$county == ctyfips[i]),]
        
       
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
              theme(plot.title = element_text(hjust = 0.5, size=12),
                 plot.caption = element_text(hjust = 0, size=9),
                    panel.background = element_rect(fill = "white", colour = "gray50"),
                    panel.grid.major = element_line(colour = "gray80"),
                    axis.text.x = element_text(size=10),
                    axis.text.y=element_text(size=10))
        ggsave(fileMat[grCount],ggimg4, device="png", height = 3 , width = 7, dpi=300)
        grCount <- grCount + 1
    }
 } 
  
 if("povage" %in% chkList) {
    grCount <- 33
    povage_list <- outputMat[[5,1]]
    povage_data <- povage_list[[1]]$data
    povage_caption <- povage_list[[1]]$caption
    povage_title <- unlist(outputMat[[5,2]])
    povage_title <- paste0(povage_title,",\n",locList$plName1)
    
    povage_data$age_cat <- factor(povage_data$age_cat , levels= c("5 to 17", "18 and Older", "Total"))
    
    for(i in 1:length(ctyfips)) {
       povage_data2 <- povage_data[which(povage_data$fips == ctyfips[i]),]
      
       povage_data2$value <- povage_data2$value * 100
       maxLim <- max(povage_data2$value) + 20
       
       LocName <- unique(povage_data2$geoname)
       ggimg5 <-ggplot(povage_data2, aes(x=age_cat, y=value)) +
       geom_bar(stat="identity", position="dodge",  color="black", fill="blue")+
             scale_y_continuous(limits = c(0, maxLim), label=percent, expand = c(0, 0)) +
              theme_codemog(base_size=base) +
              labs(title = povage_title,
                   subtitle = LocName,
                   caption = povage_caption,
                   x = "Age Category",
                   y= "Percentage") +
              theme(plot.title = element_text(hjust = 0.5, size=12),
                    plot.caption = element_text(hjust = 0, size=9),
                    panel.background = element_rect(fill = "white", colour = "gray50"),
                    panel.grid.major = element_line(colour = "gray80"),
                    axis.text.x = element_text(size=10),
                    axis.text.y=element_text(size=10))
        ggsave(fileMat[grCount],ggimg5, device="png", height = 3 , width = 7, dpi=300)
        grCount <- grCount + 1
    }
 } 
  
 if("povagetr" %in% chkList) {
    grCount <- 40
    povagetr_list <- outputMat[[6,1]]
    povagetr_data <- povagetr_list[[1]]$data
    povagetr_caption <- povagetr_list[[1]]$caption
    povagetr_title <- unlist(outputMat[[6,2]])
    povagetr_title <- paste0(povagetr_title,",\n",locList$plName1)
    
      for(i in 1:length(ctyfips)) {
       povagetr_data2 <- povagetr_data[which(povagetr_data$fips == ctyfips[i]),]
       povagetr_data2$age_cat <- factor(povagetr_data2$age_cat , levels= c("5 to 17", "18 and Older", "All Persons"))
      
       povagetr_data2$value <- povagetr_data2$value * 100
       maxLim <- max(povagetr_data2$value) + 10
       
       LocName <- unique(povagetr_data2$geoname)
       ggimg6 <-ggplot(povagetr_data2, aes(x=year, y=value, color=age_cat)) +
       geom_line(size=1.2) +
             scale_color_manual(values=c("blue","orange","green"), name="Age Category") +
             scale_y_continuous(limits = c(0, maxLim), label=percent, expand = c(0, 0)) +
              theme_codemog(base_size=base) +
              labs(title = povagetr_title,
                   subtitle = LocName,
                   caption = povagetr_caption,
                   x = "Year",
                   y= "Percentage") +
              theme(plot.title = element_text(hjust = 0.5, size=12),
                    plot.caption = element_text(hjust = 0, size=9),
                    panel.background = element_rect(fill = "white", colour = "gray50"),
                    panel.grid.major = element_line(colour = "gray80"),
                    axis.text.x = element_text(size=10),
                    axis.text.y=element_text(size=10))
        ggsave(fileMat[grCount],ggimg6, device="png", height = 3 , width = 7, dpi=300)
        grCount <- grCount + 1
    }
 } 
  
 if("povagedis" %in% chkList) {
    grCount <- 46
    povagedis_list <- outputMat[[7,1]]
    povagedis_data <- povagedis_list[[1]]$data
    povagedis_caption <- povagedis_list[[1]]$caption
    povagedis_title <- unlist(outputMat[[7,2]])
    povagedis_title <- paste0(povagedis_title,",\n",locList$plName1)
    
    povagedis_data$age_cat <- factor(povagedis_data$age_cat , levels= c("Under 18", "18 to 64", "65+", "Total"))
    povagedis_data$pct <- as.numeric(sub("%","",povagedis_data$pct))
    
      for(i in 1:length(ctyfips)) {
       povagedis_data2 <- povagedis_data[which(povagedis_data$county == ctyfips[i] & povagedis_data$meas == "Below Poverty Level" ),]
 
       maxLim <- max(povagedis_data2$pct) + 20
       
       LocName <- unique(povagedis_data2$geoname)
       ggimg7 <-ggplot(povagedis_data2, aes(x=age_cat, y=pct)) +
       geom_bar(stat="identity", position="dodge",  color="black", fill="blue")+
             scale_y_continuous(limits = c(0, maxLim), label=percent, expand = c(0, 0)) +
              theme_codemog(base_size=base) +
              labs(title = povagedis_title,
                   subtitle = LocName,
                   caption = povagedis_caption,
                   x = "Age Category",
                   y= "Percentage") +
              theme(plot.title = element_text(hjust = 0.5, size=12),
                 plot.caption = element_text(hjust = 0, size=9),
                    panel.background = element_rect(fill = "white", colour = "gray50"),
                    panel.grid.major = element_line(colour = "gray80"),
                    axis.text.x = element_text(size=10),
                    axis.text.y=element_text(size=10))
        ggsave(fileMat[grCount],ggimg7, device="png", height = 3 , width = 7, dpi=300)
        grCount <- grCount + 1
    }
 }
  

if("hhpov" %in% chkList) {
    grCount <- 52
    hhpov_list <- outputMat[[8,1]]
    hhpov_data <- hhpov_list[[1]]$data
    hhpov_table <- hhpov_list[[1]]$FlexTable
    hhpov_caption <- hhpov_list[[1]]$caption
    hhpov_title <- unlist(outputMat[[8,2]])
    hhpov_title <- paste0(hhpov_title,",\n",locList$plName1)
    
    hhpov_data$pct <- hhpov_data$pct * 100
    
    for(i in 1:length(ctyfips)) {
      hhpov_data2 <- hhpov_data[which(hhpov_data$county == ctyfips[i]),]
       
       maxLim <- max(hhpov_data2$pct) + 20
       
       LocName <- unique(hhpov_data2$geoname)
      ggimg8 <-ggplot(hhpov_data2, aes(x=famtype, y=pct, fill=kids)) +
       geom_bar(stat="identity", position="dodge")+
             scale_fill_manual(values=c("#00953A","#6EC4E8"),
                      name="Presence of Children") +
             scale_y_continuous(limits = c(0, maxLim), label=percent, expand = c(0, 0)) +
              theme_codemog(base_size=base) +
              labs(title = hhpov_title,
                   subtitle = LocName,
                   caption = hhpov_caption,
                   x = "Family Type",
                   y= "Percentage") +
              theme(plot.title = element_text(hjust = 0.5, size=12),
                 plot.caption = element_text(hjust = 0, size=9),
                    panel.background = element_rect(fill = "white", colour = "gray50"),
                    panel.grid.major = element_line(colour = "gray80"),
                    axis.text.x = element_text(size=10),
                    axis.text.y=element_text(size=10))
        ggsave(fileMat[grCount],ggimg8, device="png", height = 3 , width = 7, dpi=300)
        grCount <- grCount + 1
    }
}
  
 if("tenure" %in% chkList) {
    grCount <- 58
    tenure_list <- outputMat[[9,1]]
    tenure_data <- tenure_list[[1]]$data
    tenure_caption <- tenure_list[[1]]$caption
    tenure_title <- unlist(outputMat[[9,2]])
    tenure_title <- paste0(tenure_title,",\n",locList$plName1)
    
    tenure_data$pct <- tenure_data$pct * 100
    
    for(i in 1:length(ctyfips)) {
    tenure_data2 <- tenure_data[which(tenure_data$county == ctyfips[i] ),]
       maxLim <- max(tenure_data2$pct) + 20
       
       LocName <- unique(tenure_data2$geoname)
      ggimg9 <-ggplot(tenure_data2, aes(x=famtype, y=pct, fill=tenure)) +
       geom_bar(stat="identity", position="dodge")+
             scale_fill_manual(values=c("#00953A","#6EC4E8"),
                      name="Housing Tenure") +
             scale_y_continuous(limits = c(0, maxLim), label=percent, expand = c(0, 0)) +
              theme_codemog(base_size=base) +
              labs(title = tenure_title,
                   subtitle = LocName,
                   caption = tenure_caption,
                   x = "Family Type",
                   y= "Percentage") +
              theme(plot.title = element_text(hjust = 0.5, size=12),
                 plot.caption = element_text(hjust = 0, size=9),
                    panel.background = element_rect(fill = "white", colour = "gray50"),
                    panel.grid.major = element_line(colour = "gray80"),
                    axis.text.x = element_text(size=10),
                    axis.text.y=element_text(size=10))
        ggsave(fileMat[grCount],ggimg9, device="png", height = 3 , width = 7, dpi=300)
        grCount <- grCount + 1
    }
 }

  
   if("snap" %in% chkList) {
    grCount <- 64
    snap_list <- outputMat[[10,1]]
    snap_data <- snap_list[[1]]$data
    snap_caption <- snap_list[[1]]$caption
    snap_title <- unlist(outputMat[[10,2]])
    snap_title <- paste0(snap_title,",\n",locList$plName1)
    snap_data$pct <- snap_data$pct * 100
    
  for(i in 1:length(ctyfips)) {
      snap_data2 <- snap_data[which(snap_data$fips == ctyfips[i] ),]
       
       maxLim <- max(snap_data2$pct) + 20
     
       
       LocName <- unique(snap_data2$county)
     ggimg10 <-ggplot(snap_data2, aes(x=SNAP, y=pct)) +
       geom_bar(stat="identity", position="dodge",  color="black", fill="blue")+
             scale_y_continuous(limits = c(0, maxLim), label=percent, expand = c(0, 0)) +
              theme_codemog(base_size=base) +
              labs(title = snap_title,
                   subtitle = LocName,
                   caption = snap_caption,
                   x = "Program Participation",
                   y= "Percentage") +
              theme(plot.title = element_text(hjust = 0.5, size=12),
                 plot.caption = element_text(hjust = 0, size=9),
                    panel.background = element_rect(fill = "white", colour = "gray50"),
                    panel.grid.major = element_line(colour = "gray80"),
                    axis.text.x = element_text(size=10),
                    axis.text.y=element_text(size=10))
        ggsave(fileMat[grCount],ggimg10, device="png", height = 3 , width = 7, dpi=300)
        grCount <- grCount + 1
    }
   }
  
   if("wic" %in% chkList) {
     grCount <- 70
    wic_list <- outputMat[[11,1]]
    wic_data <- wic_list[[1]]$data
    wic_caption <- wic_list[[1]]$caption
    wic_title <- unlist(outputMat[[11,2]])
    wic_title <- paste0(wic_title,",\n",locList$plName1)       
    wic_data$pct <- wic_data$pct * 100
    
    for(i in 1:length(ctyfips)) {
       wic_data2 <- wic_data[which(wic_data$fips == ctyfips[i] ),]
       maxLim <- max(wic_data2$pct) + 20
       
       LocName <- unique(wic_data2$county)
     ggimg11 <-ggplot(wic_data2, aes(x=WIC, y=pct)) +
       geom_bar(stat="identity", position="dodge",  color="black", fill="blue")+
             scale_y_continuous(limits = c(0, maxLim), label=percent, expand = c(0, 0)) +
              theme_codemog(base_size=base) +
              labs(title = wic_title,
                   subtitle = LocName,
                   caption = wic_caption,
                   x = "Program Participation",
                   y= "Percentage") +
              theme(plot.title = element_text(hjust = 0.5, size=12),
                 plot.caption = element_text(hjust = 0, size=9),
                    panel.background = element_rect(fill = "white", colour = "gray50"),
                    panel.grid.major = element_line(colour = "gray80"),
                    axis.text.x = element_text(size=10),
                    axis.text.y=element_text(size=10))
        ggsave(fileMat[grCount],ggimg11, device="png", height = 3 , width = 7, dpi=300)
        grCount <- grCount + 1
    }
   }
  
 
  if("insurance" %in% chkList) {
    grCount <- 76
    insurance_list <- outputMat[[12,1]]
    insurance_data <- insurance_list[[1]]$data
    insurance_caption <- insurance_list[[1]]$caption
    insurance_title <- unlist(outputMat[[12,2]])
    insurance_title <- paste0(insurance_title,",\n",locList$plName1)
    
    insurance_data$ins <- plyr::revalue(insurance_data$ins, c( "Uninsured" = "Uninsured",
                                                       "Employer Sponsored" = "Employer\nSponsored",
                                                        "Medicaid" = "Medicaid",
                                                        "Individually Purchased" = "Individually\nPurchased" ,
                                                         "Child Health Plan Plus" = "Child Health\nPlan Plus"))
    insurance_data$ins <- factor(insurance_data$ins, levels = c( "Individually\nPurchased",
                                                           "Employer\nSponsored", 
                                                           "Medicaid",
                                                          "Uninsured",
                                                          "Child Health\nPlan Plus"))

       
     insurance_data$pct <- insurance_data$pct * 100
    
   for(i in 1:length(ctyfips)) {
       insurance_data2 <- insurance_data[which(insurance_data$fips == ctyfips[i] ),]
       
       maxLim <- max(insurance_data2$pct) + 20

       LocName <- unique(insurance_data2$county)
     ggimg12 <-ggplot(insurance_data2, aes(x=ins, y=pct)) +
       geom_bar(stat="identity", position="dodge",  color="black", fill="blue")+
             scale_y_continuous(limits = c(0, maxLim), label=percent, expand = c(0, 0)) +
              theme_codemog(base_size=base) +
              labs(title = insurance_title,
                   subtitle = LocName,
                   caption = insurance_caption,
                   x = "Type of Insurance",
                   y= "Percentage") +
              theme(plot.title = element_text(hjust = 0.5, size=12),
                 plot.caption = element_text(hjust = 0, size=9),
                    panel.background = element_rect(fill = "white", colour = "gray50"),
                    panel.grid.major = element_line(colour = "gray80"),
                    axis.text.x = element_text(size=10),
                    axis.text.y=element_text(size=10))
         ggsave(fileMat[grCount],ggimg12, device="png", height = 3 , width = 7, dpi=300)
        grCount <- grCount + 1
    }
   }
   
  return(x)
}
  