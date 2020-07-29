#' Community Services Block Grant Dashboard
#' @author  Adam Bickford, Colorado State Demography Office, October 2019
#' Release Version 1.0


rm(list = ls())

library(tidyverse, quietly=TRUE)
library(readr)
library(readxl, quietly=TRUE)
library(scales, quietly=TRUE)

library(codemogAPI, quietly=TRUE)
library(codemogLib)
library(codemog)

library(knitr, quietly=TRUE)
library(kableExtra, quietly=TRUE)
library(RPostgreSQL)
library(rmarkdown)
library(shiny, quietly=TRUE)
library(shinydashboard, quietly=TRUE)
library(shinyjs, quietly=TRUE)
library(rgdal)
library(jsonlite)
library(geojsonio)
library(units)
library(grid)
library(gridExtra)
library(ggthemes)
library(maptools)
library(officer)
library(flextable)
library(ggplotify)
library(ggrepel)  # These are the new packages
library(leaflet)
library(htmltools)
library(mapview)
library(DT)
library(plotly)
library(tidycensus)
library(censusapi)  # Installed 2019


# Additions for Database pool
library('pool') 
library('DBI')
library('stringr')
library('config')

source("R/API_setup.R")  # Codes setting up ACS version and Census API Key
source("R/bldCaption.R")
source("R/ageEmployment.R")
source("R/agePlotPRO2.R")
source("R/boxContent.R")
source("R/captionSrc.R")
source("R/chkID.R")
source("R/clrGeoname.R")

source("R/CountyName.R")
source("R/disabilityPRO.R")
source("R/downloadObj.R")
source("R/downloadObjUI.R")
source("R/educPRO.R")
source("R/familiesPRO.R")
source("R/graph_objects.R")
source("R/insurance.R")
source("R/housingPRO.R")
source("R/listTofips.R")
source("R/NumFmt.R")
source("R/outputWord.R")
source("R/percent.R")
source("R/popPlace.R")
source("R/popTable.R")
source("R/povertyPRO.R")
source("R/povertyPRO2.R")
source("R/povertyTrend.R")
source("R/roundUpNice.R")
source("R/submitPush.R")
source("R/submitReport.R")
source("R/tabList.R")
source("R/tabTitle.R")
source("R/TempFil.R")
source("R/simpleCap.R")
source("R/snap.R")
source("R/unemploymentTrend.R")
source("R/wic.R")





# The GLOBAL Variables  Add Additional lists items as sections get defined
#File Locations ALSO LOOK AT LINE IN THE WORD OUTPUT CODE  LINE 990
# Local/Development
# tPath <- "J:/Community Profiles/Shiny Demos/TempDir"  #Development
# tPath <- "C:/Users/adamb/OneDrive/Documents/TempDir"

#Production
 tPath <- "/tmp"  

# Locations for Google Analtyics Java Script Files
# Local/ Development

# initJS <- "J:/Community Profiles/Shiny Demos/codemogLib/www/dL_init.js"
# tagManJS <- "J:/Community Profiles/Shiny Demos/codemogLib/www/tag_manager.js"

#Production
# initJS <- "/srv/shiny-server/ProfileDashboard2/www/dL_init.js"
# tagManJS <- "/srv/shiny-server/ProfileDashboard2/www/tag_manager.js"


# Set up database pool 1/23/19

config <- get("database")
DOLAPool <-  dbPool(
  drv <- dbDriver(config$Driver),
  dbname = config$Database,
  host = config$Server,
  port = config$Port,
  user = config$UID,
  password = config$PWD
)



dbGetInfo(DOLAPool)


onStop(function(){
  poolClose(DOLAPool)
})


fixPath <- function(inPath){
  outPath <- gsub("ABICKF~1","ABickford",inPath)
  outPath <-gsub("\\\\","/",outPath)
  return(outPath)
}

# Output List
outputObj <- matrix(list() , nrow = 12,ncol=2)

#Output Objects
age.list <<- list()
emp.list <<- list()
pov3.list <<-list()
educ.list <<- list()
pov5.list <<- list()
pov6.list <<- list()
dis7.list <<- list()
fam8.list <<- list()
hh9.list <<- list()
snap10.list <<- list()
wic11.list <<- list()
ins12.list <<- list()



# Structure of user Interface
ui <-
  dashboardPage( skin="green", 
                 title= "Community Services Block Grant Dashboard",
                 dashboardHeader(title = span(img(src="co_dola__NoText-dept.png", height = 45, align = "top"),"Community Services Block Grant Dashboard"), titleWidth=550), #dashboardHeader
                 dashboardSidebar( width = 300,  useShinyjs(),
                                   # data level Drop down
                                   selectInput("level", "Select an Agency" ,
                                               choices=c("Select an Agency",
                                                         "Adams County",
                                                          "Arapahoe County",
                                                          "Baca County",
                                                          "Boulder County",
                                                          "Broomfield, City and County",
                                                          "Colorado East Community Action Agency",
                                                          "Delta County",
                                                          "Denver, City and County",
                                                          "Douglas County",
                                                          "Eagle County",
                                                          "El Paso County",
                                                          "Garfield County, MCSA",
                                                          "Gunnison County, MCSA",
                                                          "Housing Solutions for the Southwest",
                                                          "Jefferson County, MCSA",
                                                          "Kiowa County",
                                                          "Larimer County",
                                                          "MADA",
                                                          "Mesa County",
                                                          "Moffat County United Way",
                                                          "Mountain Family Center",
                                                          "Northeastern Colorado Association of Local Governments",
                                                          "Otero County, MCSA",
                                                          "Prowers County",
                                                          "Pueblo County",
                                                          "Rio Blanco County",
                                                          "Routt County",
                                                          "San Luis Valley Community Action Agency",
                                                          "South Central Council of Governments",
                                                          "Summit County, MCSA",
                                                          "Upper Arkansas Area Council of Governments",
                                                          "Weld County"
                                                          )  #Enabled in V1
                                   ),
                                   #Output Content Checkboxes
                                   checkboxGroupInput("outChk", "Select the Data Elements to display:",
                                                      choices = c("Population by Age" = "age",
                                                                  "Unemployment Rate" = "ageemp",
                                                                  "Population by Federal Poverty Level" = "pov",
                                                                  "Educational Attainment by Federal Poverty Level" = "educatt",
                                                                  "Age by Federal Poverty Level" = "povage",
                                                                  "Age by Federal Poverty Level Trend" = "povagetr",
                                                                  "Age by Federal Poverty Level for Persons with Disabilities" = "povagedis",
                                                                  "Families by Type and Poverty Status" = "hhpov",
                                                                  "Housing Tenure by Poverty Status" = "tenure",
                                                                  "Supplemental Nutrition Assistance Program (SNAP)" = "snap",
                                                                  "Women, Infants and Children (WIC)" = "wic",
                                                                  "Health Insurance by Source" = "insurance"
                                                                  

                                                      ),
                                                      selected =  c("age","ageemp","pov", "educatt","povage",
                                                                    "povagetr","povagedis","hhpov",
                                                                    "tenure", "snap", "wic", "insurance")
                                   ),
                                   
                                   #Action Button
                                   actionButton("profile","View Profile"),
                                   actionButton("contact","Contact SDO",onclick ="window.open('https://goo.gl/forms/xvyxzq6DGD46rMo42', '_blank')"),
                                   downloadButton("outputWord", label="Download Word Report",
                                                  style="color: black; background-color: gray90; border-color: black")
                                   
                 ), #dashboardSidebar
                 dashboardBody(  tags$head( 
                   tags$meta(name="keywords", content="Colorado, demographic, CSBG, community, municipality, city, population, housing, household, age, median income, jobs, wages"),
                  # includeScript(initJS),
                  # includeScript(tagManJS), #writes GTM connection
                   tags$link(rel = "stylesheet", type = "text/css", href = "dashboard.css"),  #Link to CSS...
                   tags$title("Community Services Block Grant Dashboard") #,
                   # includeScript("www/dataL.js") # This is the linkage to the dataLayer Output code
                 ),
                 tags$body(includeHTML("www/tag_body.js")),  # for non-JS instances
                 tags$style(HTML("
                                 .box.box-solid.box-primary>.box-header {
                                 color:#fffff;
                                 background:#C9C6C5
                                 }
                                 .box.box-solid.box-primary{
                                 color: #ffffff;
                                 border-bottom-color:#C9C6C5;
                                 border-left-color:#C9C6C5;
                                 border-right-color:#C9C6C5;
                                 border-top-color:#C9C6C5;
                                 }   ")),
                fluidRow(uiOutput("ui")
                 )
                
                 ) #dashboardBody
                
                 ) # dashboardPage/ui


# Server Management Function
server <- function(input, output, session) {
  
  infoSrc <- matrix(" ",nrow=12,ncol=2)
  infoSrc[1,1] <- "<b>Population by Age</b>"
  infoSrc[1,2] <- "Population for selected age caregories"
  infoSrc[2,1] <- "<b>Unemployment Rate</b>"
  infoSrc[2,2] <- "Unemployment Rate by Month"
  infoSrc[3,1] <- "<b>Population by Federal Poverty Level</b>"
  infoSrc[3,2] <- "Population by percentage of the Federal Poverty Level"
  infoSrc[4,1] <- "<b>Educational Attainment by Federal Poverty Level</b>"
  infoSrc[4,2] <- "Educational Attainment for persons Age 25 and older by 100% of Federal Poverty Level"
  infoSrc[5,1] <- "<b>Age by Federal Poverty Level</b>"
  infoSrc[5,2] <- "Age by Federal Poverty level using Small Area Income and Poverty Estimates (SAIPE) data"
  infoSrc[6,1] <- "<b>Age by Federal Poverty Level</b>"
  infoSrc[6,2] <- paste0("Age by Federal Poverty level using Small Area Income and Poverty Estimates (SAIPE) Trend Data 2010 to ",curYr)
  infoSrc[7,1] <- "<b>Age by Federal Poverty Level for Persons with Disabilities</b>"
  infoSrc[7,2] <- "Age by Federal Poverty Level for Persons by General Disability Status"
  infoSrc[8,1] <- "<b>Families by Type and Poverty Status</b>"
  infoSrc[8,2] <- "Families by Headship Type and Poverty Status"
  infoSrc[9,1] <- "<b>Housing Tenure by Poverty Status</b>"
  infoSrc[9,2] <- "Housing Tenure by Poverty Status, Family Headship and Presence of Children"
  infoSrc[10,1] <- "<b>Supplemental Nutrition Assistance Program (SNAP)</b>"
  infoSrc[10,2] <- "Eligibility and Participation in the Supplemental Nutrition Assistance Program"
  infoSrc[11,1] <- "<b>Women, Infants and Children (WIC)</b>"
  infoSrc[11,2] <- "Eligibility and Participation in the Women, Infants and Children Program"
  infoSrc[12,1] <- "<b>Health Insurance by Source</b>"
  infoSrc[12,2] <- "Health Insurance enrollment by source"
  
  infoTab <-  kable(infoSrc, format='html', table.attr='class="cleanTab"',align='l',linesep = "") %>%
    kable_styling(bootstrap_options ="condensed", full_width = F) %>%
    column_spec(1, width = "4in") 
  infoTab <- gsub("&lt;","<",infoTab)
  infoTab <- gsub("&gt;",">",infoTab)
  
 
  frontPgBox1 <- box(width=11,tags$div(tags$b("Welcome to the Community Services Block Garnt (CSBG) Data Dashboard"), tags$br(),
                                       "This dashboard provides summary plots and quantitative demographic and poverty data specific to CO CSBG Grantee service areas to assist in their three-year Needs Assessment and Community Action Plan.", tags$br(),
                                       tags$em("Profile Contents:"),
                                       HTML(infoTab),
                                       "To create a profile:",tags$br(),
                                       tags$ul(
                                         tags$li("Select a CSBG Agency using the 'Select an Agency' dropdown box."),
                                         tags$li("Select specific Data Elements to display using the checkboxes."),
                                         tags$li("Click on the 'View Profile' button to display the selected profile.")
                                       ), 
                                       tags$br(),
                                       tags$em(tags$b("Notes:")), tags$br(), 
                                       tags$ul(
                                         tags$li("Producing the requested outputs may take up to 3 minutes, depending on your request and your connection speed."),
                                         tags$li("You can download individual plots by hovering over the image and using the Plotly download functions."),
                                         tags$li("You can download individual tables and underlying data for each display by selecting the 'Sources and Downloads' panel of each display box."),
                                         tags$li("You can download a comprehensive report containing your data request by clicking on the 'Download Word Report' button."),
                                         tags$li("Downloaded files will be saved in the 'Download' location supported by your browser."),
                                         tags$li("Questions?  Click on the 'Contact SDO' button to submit a question about this tool.")
                                         )))

  
  frontPg <- list(frontPgBox1)
  shinyjs::hide("outputWord")
  
  


  output$ui <- renderUI(frontPg)

  
  observeEvent(input$level, ({
    shinyjs::hide("outputWord")
    
    #clears the comp2 dropdown on change
    
    if(input$level == "Select an Agency") { #the initial state of the dropdowns
      outUnit <- ""
    }

  }))  #observeEvent input$level
  
  # Event for Comparison selection
  observeEvent(input$comp, {
    shinyjs::hide("outputWord")
    
  }) #observeEvent input$comp
  
  # Event for click on profile button
  observeEvent(input$profile,  {
 
 #   dLout <- submitPush(input$level,input$unit,input$outChk)  # Generate dataLayer Command
 #   session$sendCustomMessage("handler1",dLout)  #Sends dataLayer command to dataL.js script
    
    outputList <<- list()
    output$ui <- renderUI(outputList)
    
        #creating the input FIPS list to generate data
    if(input$level == "Select an Agency") {
      lnError <- tags$h2("Please Select an Agency")
      outputList <<- list(lnError)
    }  else {
      withProgress(message = 'Generating Profile', value = 0, {
        fipslist <<- listTofips(value1=input$level,inlist2="",value2="")
          placeName <- input$level
        
        
        #Generate profile UI objects
        
        svals <- reactiveValues(a=NULL,b=NULL,c=NULL)
   
        ln1 <- tags$h1(placeName)
        
        
        #Population by Age  and Age by Poverty Status
        if("age" %in% input$outChk) {
          age_text <- tags$h2("Population by Age")
          age_list <- agePlotPRO2(lvl=input$level,listID=fipslist,ACS=curACS,curYr=curYr)
          
          outplotp1 <- age_list$plot
          outtabp1 <- age_list$table
          outCaption1 <- age_list$caption
          
          sketch1 <- htmltools::withTags(table(
                    tableHeader(outtabp1),
                    tableFooter(outCaption1)
                    ))

          
          AgeTabOut <- datatable(outtabp1, 
                                 container = sketch1,
                                 rownames = FALSE,
                                 caption = paste0("Population by Age: ",input$level),
                                 options = list(pageLength = 12,
                                 autowidth= TRUE,
                                columnDefs = list(list(width = '300px', targets = "_all"))))
                                                                 

          
          popa1.info <- tags$div(boxContent(title= "Population by Age",
                                              description = "The Population by Age Plot displays age categories for a single year.",
                                              MSA= "F", stats = "F", muni = "F", multiCty = "F", PlFilter = "F", 
                                              urlList = list(c("American Community Survey, Table B01001","https://data.census.gov")) ),
                                   tags$br(),
                                   downloadObjUI("popa1tabl"), downloadObjUI("popa1data"))
          
          age.box0 <- box(width=12,ln1)
          age.box1 <- tabBox(width=12, height=500,
                               tabPanel("Plot",renderPlotly({outplotp1})))
          age.box2 <- tabBox(width=12, height=500,
                               tabPanel("Table",DT::renderDataTable(AgeTabOut)),
                               tabPanel("Sources and Downloads",popa1.info))
          
          
          #building List
          age.list <<- list(age.box0, age.box1, age.box2)
          outputObj[[1,1]] <- list(age_list)
          outputObj[[1,2]] <- list("Population by Age")
          
          incProgress()
        }

        #Unemployment Rate
        if("ageemp" %in% input$outChk) {
          emp_text <- tags$h2("Unemployment Rate")
          emp_list <- unemploymentTrend(lvl=input$level,listID=fipslist,curYr=curYr)

          outplote1 <- emp_list$plot
          outtabe1 <- emp_list$table
          outCaption2 <- emp_list$caption
          
          sketch2 <- htmltools::withTags(table(
                    tableHeader(outtabe1),
                    tableFooter(outCaption2)
                    ))
          
          
          EMPTabOut <- datatable(outtabe1,
                       container = sketch2,
                       rownames = FALSE,
                      caption = paste0("Unemployment Rate: ",input$level),
                      options = list(pageLength = 14,
                      autowidth= TRUE,
                     columnDefs = list(list(width = '300px', targets = "_all"))))
                      

          
          pope1.info <- tags$div(boxContent(title= "Unemployment Rate",
                                              description = "The Unemployment Rate display shows the adjusted unemployment rate for the current year.",
                                              MSA= "F", stats = "F", muni = "F", multiCty = "F", PlFilter = "F",
                                              urlList = list(c("Bureau of Labor Statistics, Local Area Unemployment Statistics","https://www.bls.gov/lau/")) ),
                                   tags$br(),
                                   downloadObjUI("pope1tabl"), downloadObjUI("pope1data"))

          emp.box0 <- box(width=12,ln1)
          emp.box1 <- tabBox(width=12, height=500,
                               tabPanel("Plot",renderPlotly({outplote1})))
          emp.box2 <- tabBox(width=12, height=500,
                               tabPanel("Table",DT::renderDataTable(EMPTabOut)),
                               tabPanel("Sources and Downloads",pope1.info))
          
          
          #building List
          emp.list <<- list(emp.box0, emp.box1, emp.box2)
          outputObj[[2,1]] <- list(emp_list)
          outputObj[[2,2]] <- list("Unemployment Rate")
          
          incProgress()
        }  
        
  #Federal Poverty Level
        if("pov" %in% input$outChk) {
          pov3_text <- tags$h2("Population by Federal Poverty Level")
          pov3_list <- povertyPRO(lvl=input$level,listID=fipslist,ACS=curACS,PreACS = "",curYr=curYr)

          outplotpov3 <- pov3_list$plot
          outtabpov3 <- pov3_list$table
          outCaption3 <- pov3_list$caption
          
          sketch3 <- htmltools::withTags(table(
                    tableHeader(outtabpov3),
                    tableFooter(outCaption3)
                    ))
         
          
          POVTabOut3 <- datatable(outtabpov3,
                                  container = sketch3,
                                  rownames = FALSE,
                                  caption = paste0("Population by Federal Poverty Level: ",input$level),
                                  options = list(pageLength = 6,
                                                 autowidth= TRUE,
                                                columnDefs = list(list(width = '300px', targets = "_all"))))
                                                                  

          
          povpp3.info <- tags$div(boxContent(title= "Population by Federal Poverty Level",
                                              description = "The Federal Poverty Level plots and tables show the population distribution by the Federal Poverty level for the selected agency.",
                                              MSA= "F", stats = "F", muni = "F", multiCty = "F", PlFilter = "F", 
                                              urlList = list(c("American Community Survey, Table B17024","https://data.census.gov")) ),
                                   tags$br(),
                                   downloadObjUI("povpp3tabl"), downloadObjUI("povpp3data"))

          pov3.box0 <- box(width=12,pov3_text)
          pov3.box1 <- tabBox(width=12, height=500,
                               tabPanel("Plot",renderPlotly({outplotpov3})))
          pov3.box2 <- tabBox(width=12, height=500,
                               tabPanel("Table",DT::renderDataTable(POVTabOut3)),
                               tabPanel("Sources and Downloads",povpp3.info))
          
          
          #building List
          pov3.list <<- list(pov3.box0, pov3.box1, pov3.box2)
          outputObj[[3,1]] <- list(pov3_list)
          outputObj[[3,2]] <- list("Population by Federal Poverty Level")
          
          incProgress()
        }                       
        
      #Educational Attainment by Federal Poverty Level
        if("educatt" %in% input$outChk) {
          educatt_text <- tags$h2("Educational Attainment by Federal Poverty Level")
          educatt_list <- educPRO(lvl=input$level,listID=fipslist,ACS=curACS,curYr=curYr)

          outploted1 <- educatt_list$plot
          outtabed1 <- educatt_list$table
          outCaption4 <- educatt_list$caption
         
          sketch4 <- htmltools::withTags(table(
                    tableHeader(outtabed1),
                    tableFooter(outCaption4)
                    ))
          
          EDUCTabOut <- datatable(outtabed1, 
                                  container = sketch4,
                                  rownames = FALSE,
                                  caption = paste0("Educational Attainment by Federal Poverty Level: ",input$level),
                                                   options = list(pageLength = 8,
                                                                  autowidth= TRUE,
                                                                  columnDefs = list(list(width = '300px', targets = "_all"))))
                                                                  
                                                                  
          
          poped1.info <- tags$div(boxContent(title= "Educational Attainment by Federal Poverty Level",
                                              description = "Educational Attainment by Federal Poverty Level plots and tables show Educational Attainment by the Federal Poverty level for persons age 25 and older.",
                                              MSA= "F", stats = "F", muni = "F", multiCty = "F", PlFilter = "F", 
                                              urlList = list(c("American Community Survey, Table B17003","https://data.census.gov")) ),
                                   tags$br(),
                                   downloadObjUI("poped1tabl"), downloadObjUI("poped1data"))

          educ.box0 <- box(width=12,ln1)
          educ.box1 <- tabBox(width=12, height=500,
                               tabPanel("Plot",renderPlotly({outploted1})))
          educ.box2 <- tabBox(width=12, height=500,
                               tabPanel("Table",DT::renderDataTable(EDUCTabOut)),
                               tabPanel("Sources and Downloads",poped1.info))
          
          
          #building List
          educ.list <<- list(educ.box0, educ.box1, educ.box2)
          outputObj[[4,1]] <- list(educatt_list)
          outputObj[[4,2]] <- list("Educational Attainment by Federal Poverty Level")

          incProgress()
        }
        
   # Age by Federal Poverty Level
        if("povage" %in% input$outChk) {
          pov5_text <- tags$h2("Age by Federal Poverty Level")
          pov5_list <- povertyPRO2(lvl=input$level,listID=fipslist,ACS=curACS,curYr=curYr,censKey=censAPI)

          outplotpov5 <- pov5_list$plot
          outtabpov5 <- pov5_list$table
          outCaption5 <- pov5_list$caption
         
          sketch5 <- htmltools::withTags(table(
                    tableHeader(outtabpov5),
                    tableFooter(outCaption5)
                    ))
        
          
          POVTabOut5 <- datatable(outtabpov5,
                                  container = sketch5,
                                  rownames = FALSE,
                                  caption = paste0("Age by Federal Poverty Level: ",input$level, " ",curYr),
                                  options = list(pageLength = 9,
                                  autowidth= TRUE, 
                                 columnDefs = list(list(width = '300px', targets = "_all"))))

          
          povpp5.info <- tags$div(boxContent(title= "Age by Federal Poverty Level",
                                              description = "The Age by Federal Poverty Level plots and tables show the distribution of age categoeries by the Federal Poverty level for the selected agency.",
                                              MSA= "F", stats = "F", muni = "F", multiCty = "F", PlFilter = "F", 
                                              urlList = list(c("Small Area Income and Poverty Estimates (SAIPE) Program","https://www.census.gov/data-tools/demo/saipe/#/?map_geoSelector=aa_c")) ),
                                   tags$br(),
                                   downloadObjUI("povpp5tabl"), downloadObjUI("povpp5data"))

          pov5.box0 <- box(width=12,pov5_text)
          pov5.box1 <- tabBox(width=12, height=500,
                               tabPanel("Plot",renderPlotly({outplotpov5})))
          pov5.box2 <- tabBox(width=12, height=500,
                               tabPanel("Table",DT::renderDataTable(POVTabOut5)),
                               tabPanel("Sources and Downloads",povpp5.info))
          
          
          #building List
          pov5.list <<- list(pov5.box0, pov5.box1, pov5.box2)
          outputObj[[5,1]] <- list(pov5_list)
          outputObj[[5,2]] <- list("Age by Federal Poverty Level")

          incProgress()
        }   
        
        #Percent Below Federal Poverty Level by Age Trend
        if("povagetr" %in% input$outChk) {
          pov6_text <- tags$h2("Percent Below Federal Poverty Level by Age Trend")
          pov6_list <- povertyTrend(lvl=input$level,listID=fipslist,ACS=curACS,curYr=curYr,censKey=censAPI)

          outplotpov6 <- pov6_list$plot
          outtabpov6 <- pov6_list$table
          outCaption6 <- pov6_list$caption
    
          sketch6 <- htmltools::withTags(
            table(
                  tableHeader(outtabpov6),
                  tableFooter(outCaption6)
                    ))
          
          POVTabOut6 <-datatable(outtabpov6,
                                 container = sketch6,
                                 rownames = FALSE,
                                 caption = paste0("Percent Below Federal Poverty Level by Age Trend: ",input$level),
                                 options = list(pageLength = 12,
                                               autowidth= TRUE,
                                               columnDefs = list(list(width = '300px', targets = "_all"))))
                                                                 

          
          povpp6.info <- tags$div(boxContent(title= "Percent Below Federal Poverty Level by Age Trend",
                                              description = "Percent Below Federal Poverty Level by Age Trend plot and table show the percentage of persons below the Federal Poverty Level for a 10-year period.",
                                              MSA= "F", stats = "F", muni = "F", multiCty = "F", PlFilter = "F", 
                                              urlList = list(c("Small Area Income and Poverty Estimates (SAIPE) Program","https://www.census.gov/data-tools/demo/saipe/#/?map_geoSelector=aa_c")) ),
                                   tags$br(),
                                   downloadObjUI("povpp6tabl"), downloadObjUI("povpp6data"))

          pov6.box0 <- box(width=12,pov6_text)
          pov6.box1 <- tabBox(width=12, height=500,
                               tabPanel("Plot",renderPlotly({outplotpov6})))
          pov6.box2 <- tabBox(width=12, height=500,
                               tabPanel("Table",DT::renderDataTable(POVTabOut6)),
                               tabPanel("Sources and Downloads",povpp6.info))
          
          
          #building List
          pov6.list <<- list(pov6.box0, pov6.box1, pov6.box2)
          outputObj[[6,1]] <- list(pov6_list)
          outputObj[[6,2]] <- list("Age by Federal Poverty Level Trend")

          incProgress()
        }

        #Age by Federal Poverty Level for Persons with Disabilities
        if("povagedis" %in% input$outChk) {
          dis7_text <- tags$h2("Age by Federal Poverty Level for Persons with Disabilities")
          dis7_list <- disabilityPRO(lvl=input$level,listID=fipslist,ACS=curACS,curYr=curYr)

          outplotdis7 <- dis7_list$plot
          outtabdis7 <- dis7_list$table
          outCaption7 <- dis7_list$caption
  
          sketch7 <- htmltools::withTags(
            table(
                  tableHeader(outtabdis7),
                  tableFooter(outCaption7)
                    ))
          
          DISTabOut7 <-datatable(outtabdis7,
                                 container = sketch7,
                                 rownames = FALSE,
                                 caption = paste0("Age by Federal Poverty Level for Persons with Disabilities: ",input$level),
                                 options = list(pageLength = 8,
                                               autowidth= TRUE,
                                               columnDefs = list(list(width = '300px', targets = "_all"))))
                                                                 

          
          dispp7.info <- tags$div(boxContent(title= "Age by Federal Poverty Level for Persons with Disabilities",
                                              description = "Age by Federal Poverty Level for Persons with Disabilities plot and table show the percentage of persons with Disabilities below the Federal Poverty Level.",
                                              MSA= "F", stats = "F", muni = "F", multiCty = "F", PlFilter = "F", 
                                              urlList = list(c("American Community Survey, Table B18131","https://data.census.gov")) ),
                                   tags$br(),
                                   downloadObjUI("dispp7tabl"), downloadObjUI("dispp7data"))

          dis7.box0 <- box(width=12,dis7_text)
          dis7.box1 <- tabBox(width=12, height=500,
                               tabPanel("Plot",renderPlotly({outplotdis7})))
          dis7.box2 <- tabBox(width=12, height=500,
                               tabPanel("Table",DT::renderDataTable(DISTabOut7)),
                               tabPanel("Sources and Downloads",dispp7.info))
          
          
          #building List
          dis7.list <<- list(dis7.box0, dis7.box1, dis7.box2)
          outputObj[[7,1]] <- list(dis7_list)
          outputObj[[7,2]] <- list("Age by Federal Poverty Level for Persons with Disabilities")
          
          incProgress()
        }
 
       #Families by Type and Poverty Status
        if("hhpov" %in% input$outChk) {
          fam8_text <- tags$h2("Families by Type and Poverty Status")
          fam8_list <- familiesPRO(lvl=input$level,listID=fipslist,ACS=curACS,curYr=curYr)

          outplotfam8 <- fam8_list$plot
          outtabfam8 <- fam8_list$table
          outCaption8 <- fam8_list$caption
  
          sketch8 <- htmltools::withTags(
            table(
                  tableHeader(outtabfam8),
                  tableFooter(outCaption8)
                    ))
          
          DISTabOut8 <-datatable(outtabfam8,
                                 container = sketch8,
                                 rownames = FALSE,
                                 caption = paste0("Families by Type and Poverty Status: ",input$level),
                                 options = list(pageLength = 24,
                                               autowidth= TRUE,
                                               columnDefs = list(list(width = '300px', targets = "_all"))))
                                                                 

          
          dispp8.info <- tags$div(boxContent(title= "Families by Type and Poverty Status",
                                              description = "Families by Type and Poverty Status plot and table show the percentage of families by type above and velow the Federal Poverty level.",
                                              MSA= "F", stats = "F", muni = "F", multiCty = "F", PlFilter = "F", 
                                              urlList = list(c("American Community Survey, Table B18131","https://data.census.gov")) ),
                                   tags$br(),
                                   downloadObjUI("fampv8tabl"), downloadObjUI("fampv8data"))

          fam8.box0 <- box(width=12,fam8_text)
          fam8.box1 <- tabBox(width=12, height=500,
                               tabPanel("Plot",renderPlotly({outplotfam8})))
          fam8.box2 <- tabBox(width=12, height=500,
                               tabPanel("Table",DT::renderDataTable(DISTabOut8)),
                               tabPanel("Sources and Downloads",dispp8.info))
          
          
          #building List
          fam8.list <<- list(fam8.box0, fam8.box1, fam8.box2)
          outputObj[[8,1]] <- list(fam8_list)
          outputObj[[8,2]] <- list("Families by Type and Poverty Status")

          incProgress()
        }       
 
       #Housing Tenure by Poverty Status
        if("tenure" %in% input$outChk) {
          hh9_text <- tags$h2("Housing Tenure by Poverty Status")
          hh9_list <- housingPRO(lvl=input$level,listID=fipslist,ACS=curACS,curYr=curYr)

          outplothh9 <- hh9_list$plot
          outtabhh9 <- hh9_list$table
          outCaption9 <- hh9_list$caption
  
          sketch9 <- htmltools::withTags(
            table(
                  tableHeader(outtabhh9),
                  tableFooter(outCaption9)
                    ))
          
          HHTabOut9 <-datatable(outtabhh9,
                                 container = sketch9,
                                 rownames = FALSE,
                                 caption = paste0("Housing Tenure by Poverty Status: ",input$level),
                                 options = list(pageLength = 24,
                                               autowidth= TRUE,
                                               columnDefs = list(list(width = '300px', targets = "_all"))))
                                                                 

          
          hh9.info <- tags$div(boxContent(title= "Housing Tenure by Poverty Status",
                                              description = "Housing Tenure by Family Type and Poverty Status plot and table show the percentage of Housing Tenure by type above and velow the Federal Poverty level.",
                                              MSA= "F", stats = "F", muni = "F", multiCty = "F", PlFilter = "F", 
                                              urlList = list(c("American Community Survey, Table B18131","https://data.census.gov")) ),
                                   tags$br(),
                                   downloadObjUI("house9tabl"), downloadObjUI("house9data"))

          hh9.box0 <- box(width=12,hh9_text)
          hh9.box1 <- tabBox(width=12, height=500,
                               tabPanel("Plot",renderPlotly({outplothh9})))
          hh9.box2 <- tabBox(width=12, height=500,
                               tabPanel("Table",DT::renderDataTable(HHTabOut9)),
                               tabPanel("Sources and Downloads",hh9.info))
          
          
          #building List
          hh9.list <<- list(hh9.box0, hh9.box1, hh9.box2)
          outputObj[[9,1]] <- list(hh9_list)
          outputObj[[9,2]] <- list("Housing Tenure by Poverty Status")

          incProgress()
        }    
        
       #Supplemental Nutrition Assistance Program (SNAP)
        if("snap" %in% input$outChk) {
          snap_text <- tags$h2("Supplemental Nutrition Assistance Program (SNAP)")
          snap_list <- snap(DBPool=DOLAPool,lvl=input$level,listID=fipslist,curYR=curYr)

          outplotsnap <- snap_list$plot
          outtabsnap <- snap_list$table
          outCaption10 <- snap_list$caption
  
          sketch10 <- htmltools::withTags(
            table(
                  tableHeader(outtabsnap),
                  tableFooter(outCaption10)
                    ))
          
          snapTabOut <-datatable(outtabsnap,
                                 container = sketch10,
                                 rownames = FALSE,
                                 caption = paste0("Supplemental Nutrition Assistance Program (SNAP): ",input$level),
                                 options = list(pageLength = 10,
                                               autowidth= TRUE,
                                               columnDefs = list(list(width = '300px', targets = "_all"))))
                                                                 

          
          snap.info <- tags$div(boxContent(title= "Supplemental Nutrition Assistance Program (SNAP)",
                                              description = "The Supplemental Nutrition Assistance Program (SNAP) Tables display eligibiliity for the Supplemental Nutrition Assistance Program (SNAP) program from Hunger Free Colorado",
                                              MSA= "F", stats = "F", muni = "F", multiCty = "F", PlFilter = "F", 
                                              urlList = list(c("Hunger Free Colorado SNAP Impact Reports","https://www.hungerfreecolorado.org/impact-reports/")) ),
                                   tags$br(),
                                   downloadObjUI("snap10tabl"), downloadObjUI("snap10data"))

          snap.box0 <- box(width=12,snap_text)
          snap.box1 <- tabBox(width=12, height=500,
                               tabPanel("Plot",renderPlotly({outplotsnap})))
          snap.box2 <- tabBox(width=12, height=500,
                               tabPanel("Table",DT::renderDataTable(snapTabOut)),
                               tabPanel("Sources and Downloads",snap.info))
          
          
          #building List
          snap10.list <<- list(snap.box0, snap.box1, snap.box2)
          outputObj[[10,1]] <- list(snap_list)
          outputObj[[10,2]] <- list("Supplemental Nutrition Assistance Program (SNAP)")

          incProgress()
        }   
        
        #Women, Infants and Children (WIC)
        if("wic" %in% input$outChk) {
          wic_text <- tags$h2("Women, Infants and Children (WIC)")
          wic_list <- wic(DBPool=DOLAPool,lvl=input$level,listID=fipslist,curYR=curYr)

          outplotwic <- wic_list$plot
          outtabwic <- wic_list$table
          outCaption11 <- wic_list$caption
  
          sketch11 <- htmltools::withTags(
            table(
                  tableHeader(outtabwic),
                  tableFooter(outCaption11)
                    ))
          
          wicTabOut <-datatable(outtabwic,
                                 container = sketch11,
                                 rownames = FALSE,
                                 caption = paste0("Women, Infants and Children (WIC): ",input$level),
                                 options = list(pageLength = 10,
                                               autowidth= TRUE,
                                               columnDefs = list(list(width = '300px', targets = "_all"))))
                                                                 

          
          wic.info <- tags$div(boxContent(title= "Women, Infants and Children (WIC)",
                                              description = "The Women, Infants and Children (WIC) Tables display eligibiliity for the Women, Infants and Children program (WIC) program from the Annie E. Casey Kids Count Data Center",
                                              MSA= "F", stats = "F", muni = "F", multiCty = "F", PlFilter = "F", 
                                              urlList = list(c("Annie E. Casey Kids Count Data Center","https://datacenter.kidscount.org/data#CO/2/0/char/0")) ),
                                   tags$br(),
                                   downloadObjUI("wic11tabl"), downloadObjUI("wic11data"))

          wic.box0 <- box(width=12,wic_text)
          wic.box1 <- tabBox(width=12, height=500,
                               tabPanel("Plot",renderPlotly({outplotwic})))
          wic.box2 <- tabBox(width=12, height=500,
                               tabPanel("Table",DT::renderDataTable(wicTabOut)),
                               tabPanel("Sources and Downloads",wic.info))
          
          
          #building List
          wic11.list <<- list(wic.box0, wic.box1, wic.box2)
          outputObj[[11,1]] <- list(wic_list)
          outputObj[[11,2]] <- list("Women, Infants and Children (WIC)")
 
          incProgress()
        }  
        
        
        #Health Insurance by Source
        if("insurance" %in% input$outChk) {
          insurance_text <- tags$h2("Health Insurance by Source")
          insurance_list <- insurance(DBPool=DOLAPool,lvl=input$level,listID=fipslist,curYR=curYr)

          outplotinsurance <- insurance_list$plot
          outtabinsurance <- insurance_list$table
          outCaption12 <- insurance_list$caption
  
          sketch12 <- htmltools::withTags(
            table(
                  tableHeader(outtabinsurance),
                  tableFooter(outCaption12)
                    ))
          
          insuranceTabOut <-datatable(outtabinsurance,
                                 container = sketch12,
                                 rownames = FALSE,
                                 caption = paste0("Health Insurance by Source: ",input$level),
                                 options = list(pageLength = 10,
                                               autowidth= TRUE,
                                               columnDefs = list(list(width = '300px', targets = "_all"))))
                                                                 

          
          insurance.info <- tags$div(boxContent(title= "Health Insurance by Source",
                                              description = "The Health Insurance by Source Tables display Health Insturance by Source data compiled by the Colorado Health Institute",
                                              MSA= "F", stats = "F", muni = "F", multiCty = "F", PlFilter = "F", 
                                              urlList = list(c("Coloeado Health Institute Regional Data Workbook","https://www.coloradohealthinstitute.org/data/%7B%22field_category%22:[%2298%22]%7D")) ),
                                   tags$br(),
                                   downloadObjUI("ins12tabl"), downloadObjUI("ins12data"))

          insurance.box0 <- box(width=12,insurance_text)
          insurance.box1 <- tabBox(width=12, height=500,
                               tabPanel("Plot",renderPlotly({outplotinsurance})))
          insurance.box2 <- tabBox(width=12, height=500,
                               tabPanel("Table",DT::renderDataTable(insuranceTabOut)),
                               tabPanel("Sources and Downloads",insurance.info))
          
          
          #building List
          ins12.list <<- list(insurance.box0, insurance.box1, insurance.box2)
          outputObj[[12,1]] <- list(insurance_list)
          outputObj[[12,2]] <- list("Health Insurance by Source")
          
          incProgress()
        } 
 
        #Creating output file location and Prepping Matrix of filenames
        
        tName <- ""
        tmpName <- sample(c(0:9, LETTERS),8, replace=TRUE)
        for(i in 1:8) {
          tName <- paste0(tName,tmpName[i])
        }
        
        fullDir <- file.path(tPath,tName)
        tDir <- dir.create(fullDir) #Setting Temporary Directory location for Reporting
        
        fileMat <- TempFil(fullDir)  
        
        x <-  outputWord(input$outChk, fipslist, input$level, outputObj,fileMat)  # x is  a list with nothing in it.
        shinyjs::show("outputWord")
        }) #Progress Bar
    }#if input$unit == ""
    
    # Output UI...
    
    if(length(outputList) == 0) {
    
      tabs <- lapply(1:length(input$outChk), function(i) {  # this determines the number of tabs needed
        id <- paste0("tab", i)
        tabPanel(
          title = tabTitle(input$outChk[[i]]), tabList(input$outChk[[i]])
        ) # TabPanel
      })
    }  else {
      tabs <- outputList
    }
    output$ui  <- renderUI({ do.call(tabsetPanel, tabs) }) #renderUI
    
     #Event to output Word documents

    output$outputWord <- downloadHandler(
      
      filename <- function() {
        paste0(input$level," CSBG Report ",as.character(Sys.Date()),".docx")
      },
      content <- function(file) {
        
      withProgress(message = 'Generating Word Document', value = 0, {
        incProgress()
        #Generate Report
        #knitting file and copy to final document
        tempRMD <- fixPath(fileMat[1])   #Testing
        tempWord <- fixPath(fileMat[2])
       
        
       
        
        rmarkdown::render(input= tempRMD, output_file = tempWord,
                          params =  list(outChk = input$outChk,
                                         olistID = fipslist,
                                         olevel = input$level,
                                         oObj = outputObj,
                                         ofileMat = fileMat),
                          run_pandoc = TRUE)
        incProgress()
        file.rename(tempWord, file) # move word to file for downloading
      }) # progress
       shinyjs::hide("outputWord") 
      } #Content
    ) #Download Handler

 
    #Event to outload plots and data files
    
    # Age
    if("age" %in% input$outChk){
    callModule(downloadObj, id = "popa1tabl", input$level,"popa1tabl", age_list$FlexTable)
    callModule(downloadObj, id = "popa1data", input$level,"popa1data", age_list$table)
    }
    
   #Age by Employment  
    if("ageemp" %in% input$outChk){
    callModule(downloadObj, id = "pope1tabl", input$level,"pope1tabl", emp_list$FlexTable)
    callModule(downloadObj, id = "pope1data", input$level,"pope1data", emp_list$table)
    }

   #Federal Poverty Level
   if("pov" %in% input$outChk){
    callModule(downloadObj, id = "povpp3tabl", input$level,"povpp3tabl", pov3_list$FlexTable)
    callModule(downloadObj, id = "povpp3data", input$level,"povpp3data", pov3_list$data)
     }
    
   #Educational Attainment by Federal Poverty Level
   if("educatt" %in% input$outChk){
    callModule(downloadObj, id = "poped1tabl", input$level,"poped1tabl", educatt_list$FlexTable)
    callModule(downloadObj, id = "poped1data", input$level,"poped1data", educatt_list$data)
   }
  
   #Age by Federal Poverty Level
   if("povage" %in% input$outChk){
    callModule(downloadObj, id = "povpp5tabl", input$level,"povpp5tabl", pov5_list$FlexTable)
    callModule(downloadObj, id = "povpp5data", input$level,"povpp5data", pov5_list$data)
   }
  
  #Age by Federal Poverty Level Trend
   if("povagetr" %in% input$outChk){
    callModule(downloadObj, id = "povpp6tabl", input$level,"povpp6tabl", pov6_list$FlexTable)
    callModule(downloadObj, id = "povpp6data", input$level,"povpp6data", pov6_list$table)
     }
    
   #Age by Federal Poverty Level for Persons with Disabilities
   if("povagedis" %in% input$outChk){
    callModule(downloadObj, id = "dispp7tabl", input$level,"dispp7tabl", dis7_list$FlexTable)
    callModule(downloadObj, id = "dispp7data", input$level,"dispp7data", dis7_list$data)
   }
    
    #Families by Type and Poverty Status
    if("hhpov" %in% input$outChk){
    callModule(downloadObj, id = "fampv8tabl", input$level,"fampv8tabl", fam8_list$FlexTable)
    callModule(downloadObj, id = "fampv8data", input$level,"fampv8data", fam8_list$data)
    }
    
   #Housing Tenure by Poverty Status
    if("tenure" %in% input$outChk) {
    callModule(downloadObj, id = "house9tabl", input$level,"house9tabl", hh9_list$FlexTable)
    callModule(downloadObj, id = "house9data", input$level,"house9data", hh9_list$data)
    }
    
    #Supplemental Nutrition Assistance Program (SNAP)
    if("snap" %in% input$outChk) {
    callModule(downloadObj, id = "snap10tabl", input$level,"snap10tabl", snap_list$FlexTable)
    callModule(downloadObj, id = "snap10data", input$level,"snap10data", snap_list$data)
    }
    
    #Womens Infants and Children (WIC)
    if("wic" %in% input$outChk) {
    callModule(downloadObj, id = "wic11tabl", input$level,"wic11tabl", wic_list$FlexTable)
    callModule(downloadObj, id = "wic11data", input$level,"wic11data", wic_list$data)
    }
    
    #health Insurance by Source
    if("insurance" %in% input$outChk) {
    callModule(downloadObj, id = "ins12tabl", input$level,"ins12tabl", insurance_list$FlexTable)
    callModule(downloadObj, id = "ins12data", input$level,"ins12data", insurance_list$data)
    }
  }) #observeEvent input$profile
  
  
  
}  #server



shinyApp(ui = ui, server = server)
