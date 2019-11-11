#' Community Services Block Grant Dashboard
#' @author  Adam Bickford, Colorado State Demography Office, October 2019
#' Release Version 1.0


rm(list = ls())
library(tidyverse, quietly=TRUE)
library(readr)
library(readxl, quietly=TRUE)
library(scales, quietly=TRUE)
library(codemogAPI, quietly=TRUE)
library(codemogProfile, quietly=TRUE)
library(codemogLib)
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

# Additions for Database pool
library('pool') 
library('DBI')
library('stringr')
library('config')


source("R/ageForecastPRO.R")
source("R/agePlotPRO.R")
source("R/ageEmployment.R")
source("R/agePoverty.R")
source("R/boxContent.R")
source("R/captionSrc.R")
source("R/chkID.R")
source("R/clrGeoname.R")
source("R/cocPlot.R")
source("R/codemog_cdp.r")
source("R/Countyname.R")
source("R/dashboardMAP.R")
source("R/downloadObj.R")
source("R/downloadObjUI.R")
source("R/educPRO.R")
source("R/GenerateVenn.R")
source("R/houseEstPRO.R")
source("R/housePRO.R")
source("R/HouseVal.R")
source("R/incomePRO.R")
source("R/incomeSrc.R")
source("R/jobMigration.R")
source("R/jobsByIndustry.R")
source("R/jobsPlot.R")
source("R/jobsPopForecast.R")
source("R/listTofips.R")
source("R/medianAgeTab.R")
source("R/migbyagePRO.R")
source("R/NumFmt.R")
source("R/HousingUnits.R")
source("R/percent.R")
source("R/pop_timeseries.R")
source("R/popForecast.R")
source("R/popPlace.R")
source("R/popTable.R")
source("R/povertyPRO.R")
source("R/raceTab1.R")
source("R/raceTab2.R")
source("R/residentialLF.R")
source("R/roundUpNice.R")
source("R/setAxis.R")
source("R/setYrRange.R")
source("R/simpleCap.R")
source("R/statsTable1.R")
source("R/submitPush.R")
source("R/submitReport.R")
source("R/tabList.R")
source("R/tabTitle.R")
source("R/TempFil.R")
source("R/weeklyWages.R")
source("R/unemployment.R")



# The GLOBAL Variables  Add Additional lists items as sections get defined
#File Locations ALSO LOOK AT LINE IN THE PDF OUTPUT CODE  LINE 1229
# Local/Development
 tPath <- "J:/Community Profiles/Shiny Demos/TempDir"  #Development

#Production
# tPath <- "/tmp"  

# Locations for Google Analtyics Java Script Files
# Local/ Development

# initJS <- "J:/Community Profiles/Shiny Demos/codemogLib/www/dL_init.js"
# tagManJS <- "J:/Community Profiles/Shiny Demos/codemogLib/www/tag_manager.js"

#Production
# initJS <- "/srv/shiny-server/ProfileDashboard2/www/dL_init.js"
# tagManJS <- "/srv/shiny-server/ProfileDashboard2/www/tag_manager.js"

# Current ACS database
curACS <- "acs1317"
preACS <- "acs0812"
curYr <- 2017
fipslist <<- ""

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

#Table 1: Age Distribution
age.list <<- list()

# Table 2: Age Distribution by Employment Status
emp.list <<- list()

#Population Forecast
popf1 <<- list()
popf2 <<- list()
popf3 <<- list()
popf4 <<- list()
popf.list <<- list()


#Population Characteristics
popc1 <<- list()
popc2 <<- list()
popc3 <<- list()
popc4 <<- list()
popc.list <<- list()

#Housing and Household Characteristics
poph1 <<- list()
poph2 <<- list()
poph3 <<- list()
poph4 <<- list()
poph5 <<- list() # Housing Values
poph.list <<- list()

#Commuting (Transit)
popt1 <<- list()
popt2 <<- list()  # This is for the jobs and Migration chart
popt.list <<- list()

#Employment by Industry
popei1 <<- list()
popei2 <<- list()
popei3 <<- list()
popei.list <<- list()

#Employment and Demographic Forecast
popem1 <<- list()
popem2 <<- list()
popem3 <<- list()
popem4 <<- list()
popem.list <<- list()


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
                                                          "Garfield County",
                                                          "Gunnison County",
                                                          "Housing Solutions for the Southwest",
                                                          "Jefferson County",
                                                          "Kiowa County",
                                                          "Larimer County",
                                                          "MADA",
                                                          "Mesa County",
                                                          "Moffat County United Way",
                                                          "Mountain Family Center",
                                                          "Northeastern Colorado Association of Local Governments",
                                                          "Otero County",
                                                          "Prowers County",
                                                          "Pueblo County",
                                                          "Rio Blanco County",
                                                          "Routt County",
                                                          "San Luis Valley Community Action Agency",
                                                          "South Central Council of Governments",
                                                          "Summit County",
                                                          "Upper Arkansas Area Council of Governments",
                                                          "Weld County"
                                                          )  #Enabled in V1
                                   ),
                                   #Output Content Checkboxes
                                   checkboxGroupInput("outChk", "Select the Data Elements to display:",
                                                      choices = c("Table 1: Age Distribution" = "age",
                                                                  "Table 2: Age by Employment Status" = "ageemp",
                                                                  "Table 3: Population by Federal Poverty Level" = "pov",
                                                                  "Table 4: Educational Attainment by Federal Poverty Level" = "educatt",
                                                                  "Table 5: Age by Federal Poverty Level" = "povage",
                                                                  "Table 6: Age by Federal Poverty Level Trend" = "povagetr",
                                                                  "Table 7: Age by Federal Poverty Level for Persons with Disabilities" = "povagedis",
                                                                  "Table 8: Households by Occupancy" = "hhpov",
                                                                  "Table 9: Health Insurance by Source" = "insurance",
                                                                  "Table 10: Housing Tenure by Poverty" = "tenure"

                                                      ),
                                                      selected =  c("age","ageemp","pov", "educatt","povage",
                                                                    "povagetr","povagedis","hhpov","hspov",
                                                                    "insurance","tenure")
                                   ),
                                   
                                   #Action Button
                                   actionButton("profile","View Profile"),
                                   actionButton("contact","Contact SDO",onclick ="window.open('https://goo.gl/forms/xvyxzq6DGD46rMo42', '_blank')"),
                                   downloadButton("outputPDF", label="Download PDF Report",
                                                  style="color: black; background-color: gray90; border-color: black")
                                   
                 ), #dashboardSidebar
                 dashboardBody(  tags$head( 
                   tags$meta(name="keywords", content="Colorado, demographic, county, community, municiplaity, city, population, housing, household, age, median income, jobs, wages"),
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
  
  infoSrc <- matrix(" ",nrow=8,ncol=2)
  infoSrc[1,1] <- "<b>Basic Statistics</b>"
  infoSrc[1,2] <- "Summary Table and Map"
  infoSrc[2,1] <- "<b>Population Trends</b>"
  infoSrc[2,2] <- "Population Estimates and Forecasts"
  infoSrc[3,1] <- "<b>Population Characteristics: Age</b>"
  infoSrc[3,2] <- "Population Estimates and Migration by Age"
  infoSrc[4,1] <- "<b>Population Characteristics: Income, Education and Race</b>"
  infoSrc[4,2] <- "Population Estimates by Income, Income Source, Educational Attainment and Race"
  infoSrc[5,1] <- "<b>Housing and Households</b>"
  infoSrc[5,2] <- "Housing Units, Costs and Unit Characteristics"
  infoSrc[6,1] <- "<b>Commuting and Job Growth</b>"
  infoSrc[6,2] <- "Commuting Patterns and Job Growth and Migration"
  infoSrc[7,1] <- "<b>Employment by Industry</b>"
  infoSrc[7,2] <- "Employment Data by Industry"
  infoSrc[8,1] <- "<b>Employment Forecast and Wage Information</b>"
  infoSrc[8,2] <- "Employment Forecasts, Wage and Number of Firms"
  
  infoTab <-  kable(infoSrc, format='html', table.attr='class="cleanTab"',align='l',linesep = "") %>%
    kable_styling(bootstrap_options ="condensed", full_width = F) %>%
    column_spec(1, width = "4in") 
  infoTab <- gsub("&lt;","<",infoTab)
  infoTab <- gsub("&gt;",">",infoTab)
  
  #Creating data Source Links Table
  linkSrc <- matrix(" ", nrow=6, ncol=5)
  linkSrc[1,1]  <- "<b>Data Dashboard</b>"
  linkSrc[2,1]  <- "<a href='https://gis.dola.colorado.gov/apps/demographic_dashboard/' target='_blank'>Demographic Dashboard</a>"
  linkSrc[3,1]  <- "<a href='https://gis.dola.colorado.gov/apps/netmigration_dashboard/' target='_blank'>Net Migration Dashboard</a>"
  
  linkSrc[4,1] <- "<b>Data Lookup Pages</b>"
  linkSrc[5,1] <- "<a href='https://demography.dola.colorado.gov/population/data/profile-county/' target='_blank'>County Data Lookup</a>"
  linkSrc[6,1] <- "<a href='https://demography.dola.colorado.gov/population/data/profile-regions/' target='_blank'>Regional Data Lookup</a>"
  
  linkSrc[1,2]  <- "<b>Maps and GIS data</b>" 
  linkSrc[2,2]  <- "<a href='https://demography.dola.colorado.gov/gis/map-gallery/' target='_blank'>Interactive Map Gallery</a>"
  linkSrc[3,2]  <- "<a href='https://demography.dola.colorado.gov/gis/thematic-maps/#thematic-maps' target='_blank'>Thematic Maps</a>"
  linkSrc[4,2]  <- "<a href='https://demography.dola.colorado.gov/demography/region-reports-2014/#colorado-planning-region-reports' target='_blank'>Region Reports</a>"
  linkSrc[5,2] <- "<a href='https://demography.dola.colorado.gov/gis/gis-data/#gis-data' target='_blank'>GIS Data Downloads</a>"
  linkSrc[6,2] <- "<a href='https://demography.dola.colorado.gov/gis/gis-data/#gis-data' target='_blank'>Links to GIS Data and DOLA Grants</a>"
  
  
  linkSrc[1,3]  <- "<b>Population Data</b>"
  linkSrc[2,3]  <- "<a href='https://demography.dola.colorado.gov/population/' target='_blank'>Population Estimates and Forecasts</a>"
  linkSrc[3,3]  <- "<a href='https://demography.dola.colorado.gov/births-deaths-migration/' target='_blank'>Births Deaths and Migration</a>"
  linkSrc[4,3]  <- "<a href='https://demography.dola.colorado.gov/economy-labor-force/' target='_blank'>Economy and Labor Force</a>"
  linkSrc[5,3]  <- "<a href='https://demography.dola.colorado.gov/housing-and-households/' target='_blank','>Housing and Households</a>"
  
  linkSrc[1,4] <- "<b>Census and ACS Data</b>"
  linkSrc[2,4] <- "<a href='https://demography.dola.colorado.gov/data/#census-data-tools' target='_blank'>Census Data Tools</a>"
  linkSrc[3,4] <- "<a href='https://demography.dola.colorado.gov/census-acs/' target='_blank'>Census Data Page</a>"
  linkSrc[1,5]  <- "<b>Publications</b>"
  linkSrc[2,5]  <- "<a href='https://demography.dola.colorado.gov/demography/publications-and-presentations/#publications-and-presentations' target='_blank'>Publications and Reports</a>"
  linkSrc[3,5]  <- "<a href='https://demography.dola.colorado.gov/crosstabs/' target='_blank'>Crosstabs</a>"
  linkSrc[4,5]  <- "<a href='https://demography.dola.colorado.gov/demography/publications-and-presentations/#annual-demography-summit-2017' target='_blank'>Annual Summit</a>"
  
  
  linkTab <-  kable(linkSrc, format='html', table.attr='class="cleanTab"',align='l',linesep = "") %>%
    kable_styling(bootstrap_options ="condensed") %>%
    column_spec(1, width = "2.25in") %>%
    column_spec(2, width = "2.25in") %>%
    column_spec(3, width = "2.25in") %>%
    column_spec(4, width = "2.25in") %>%
    column_spec(5, width = "2.25in")
  
  linkTab <- gsub("&lt;","<",linkTab)
  linkTab <- gsub("&gt;",">",linkTab)
  
  frontPgBox1 <- box(width=11,tags$div(tags$b("Welcome to the State Demography Office (SDO) Colorado Demographic Profiles Website"), tags$br(),
                                       "This tool provides summary plots and data describing Counties and Incorporated Municipalities in Colorado.", tags$br(),
                                       tags$em("Profile Contents:"),
                                       HTML(infoTab),
                                       "To create a profile:",tags$br(),
                                       tags$ul(
                                         tags$li("Select a Data Level and Location using the dropdown boxes."),
                                         tags$li("Select specific Data Elements to display using the checkboxes."),
                                         tags$li("Click on the 'View Profile' button to display the selected profile.")
                                       ), 
                                       "You can download the plots and underlying data for each display by selecting the 'Sources and Downloads' 
                                       panel of each display box.", tags$br(),
                                       tags$em(tags$b("Notes:")), tags$br(), 
                                       tags$ul(
                                         tags$li("Profiles are available for Counties and Incorporated Municipalites.  
                                                 Please contact SDO for information on other geographies and places."),
                                         tags$li("Producing the requested outputs may take up to 3 minutes, depending on your request and your connection speed."),
                                         tags$li("Downloading any report, plot or data object will open a new browser window while the 
                                                 object is being processed and downloaded.  This window will close once the object processing is completed."),
                                         tags$li("Downloaded objects will be saved in the 'Download' location supported by your browser.")
                                         )))
  frontPgBox2 <-  box(width=11, tags$div(
    tags$b("Links to other SDO Data Sources:"),
    HTML(linkTab)))
  
  frontPg <- list(frontPgBox1,frontPgBox2)
  shinyjs::hide("outputPDF")
  

 # output$ui <- renderUI(frontPg)

  
  observeEvent(input$level, ({
    shinyjs::hide("outputPDF")
    
    #clears the comp2 dropdown on change
    
    if(input$level == "Select an Agency") { #the initial state of the dropdowns
      outUnit <- ""
    }

  }))  #observeEvent input$level
  
  # Event for Comparison selection
  observeEvent(input$comp, {
    shinyjs::hide("outputPDF")
    
  }) #observeEvent input$comp
  
  # Event for click on profile button
  observeEvent(input$profile,  {
 
    shinyjs::hide("outputPDF")
    
  
    

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
          placeName <- simpleCap(input$level)
        
        
        #Generate profile UI objects
        
        svals <- reactiveValues(a=NULL,b=NULL,c=NULL)
   
        ln1 <- tags$h1(placeName)
        
        
        #Table 1 Age Distribution  and Age by Poverty Status
        if("age" %in% input$outChk) {
          age_text <- tags$h2("Table 1: Age Distribution")
          age_list <- agePlotPRO(lvl=input$level,listID=fipslist,ACS=curACS,curYr=curYr)
          
          outplotp1 <- age_list$plot
          outtabp1 <- age_list$table
          
          output$AgeTabOut <- DT::renderDataTable(outtabp1,
                                                   options = list(pageLength = 12,
                                                                  autowidth= TRUE,
                                                                  scrollX = TRUE, scrollY=TRUE),
                                                                  rownames = FALSE,
                                                                  caption = paste0("Table 1: Age Distribution: ",input$level))

          
          popa1.info <- tags$div(boxContent(title= "Table 1: Age Distribution",
                                              description = "The Population by Age Plot displays age categories for a single year.",
                                              MSA= "F", stats = "F", muni = "F", multiCty = "F", PlFilter = "F", 
                                              urlList = list(c("American Community Survey, Table B01001","https://data.census.gov")) ),
                                   tags$br(),
                                   downloadObjUI("popa1tabl"), downloadObjUI("popa1data"))
          
          age.box0 <- box(width=12,ln1)
          age.box1 <- tabBox(width=12, height=500,
                               tabPanel("Plot",renderPlotly({outplotp1})))
          age.box2 <- tabBox(width=12, height=500,
                               tabPanel("Table",DT::dataTableOutput("AgeTabOut")),
                               tabPanel("Sources and Downloads",popa1.info))
          
          
          #building List
          age.list <<- list(age.box0, age.box1, age.box2)
          
          incProgress()
        }

        #Table 2 Age by Employment Status
        if("ageemp" %in% input$outChk) {
          emp_text <- tags$h2("Table 2: Age Distribution by Employment Status")
          emp_list <- ageEmployment(lvl=input$level,listID=fipslist,ACS=curACS,curYr=curYr)

          outplote1 <- emp_list$LFPlot
          outplote2 <- emp_list$UEMPPlot
          outtabe1 <- emp_list$data
          
          output$EMPTabOut <- DT::renderDataTable(outtabe1,
                                                   options = list(pageLength = 10,
                                                                  autowidth= TRUE,
                                                                  scrollX = TRUE, scrollY=TRUE),
                                                                  rownames = FALSE,
                                                                  caption = paste0("Table 2: Age Distribution by Employment Status: ",input$level))

          
          pope1.info <- tags$div(boxContent(title= "Table 2: Age by Employment Status",
                                              description = "The Age  by Employment Status outputs show 1) the proportion of individuals in the labor force, and the proportion of civilians in the labor force who are unemployed.",
                                              MSA= "F", stats = "F", muni = "F", multiCty = "F", PlFilter = "F", 
                                              urlList = list(c("American Community Survey, Table B23001","https://data.census.gov")) ),
                                   tags$br(),
                                   downloadObjUI("pope1tabl"), downloadObjUI("pope1data"))

          emp.box0 <- box(width=12,ln1)
          emp.box1 <- tabBox(width=12, height=500,
                               tabPanel("Plot",renderPlotly({outplote1})))
          emp.box2 <- tabBox(width=12, height=500,
                               tabPanel("Plot",renderPlotly({outplote2})))
          emp.box3 <- tabBox(width=12, height=500,
                               tabPanel("Table",DT::dataTableOutput("EMPTabOut")),
                               tabPanel("Sources and Downloads",pope1.info))
          
          
          #building List
          emp.list <<- list(emp.box0, emp.box1, emp.box2,emp.box3)
          
          incProgress()
        }  
        
  #Table 3: Federal Poverty Level
        if("pov" %in% input$outChk) {
          pov3_text <- tags$h2("Table 3: Population by Federal Poverty Level")
          pov3_list <- povertyPRO(lvl=input$level,listID=fipslist,ACS=curACS,PreACS = "",curYr=curYr,tabtype = 3)

          outplotpov3 <- pov3_list$plot
          outtabpov3 <- pov3_list$data
          
          output$POVTabOut3 <- DT::renderDataTable(outtabpov3,
                                                   options = list(pageLength = 10,
                                                                  autowidth= TRUE,
                                                                  scrollX = TRUE, scrollY=TRUE),
                                                                  rownames = FALSE,
                                                                  caption = paste0("Table 3: Population by Federal Poverty Level ",input$level))

          
          povpp3.info <- tags$div(boxContent(title= "Table 3: Population by Federal Poverty Level",
                                              description = "The Federal Poverty Level plots and tables show the population distribution by the Federal Poverty level for the selected agency.",
                                              MSA= "F", stats = "F", muni = "F", multiCty = "F", PlFilter = "F", 
                                              urlList = list(c("American Community Survey, Table B17024","https://data.census.gov")) ),
                                   tags$br(),
                                   downloadObjUI("povpp3tabl"), downloadObjUI("povpp3data"))

          pov3.box0 <- box(width=12,pov3_text)
          pov3.box1 <- tabBox(width=12, height=500,
                               tabPanel("Plot",renderPlotly({outplotpov3})))
          pov3.box2 <- tabBox(width=12, height=500,
                               tabPanel("Table",DT::dataTableOutput("POVTabOut3")),
                               tabPanel("Sources and Downloads",povpp3.info))
          
          
          #building List
          pov3.list <<- list(pov3.box0, pov3.box1, pov3.box2)
          
          incProgress()
        }                       
        
      #Table 4: Educational Attainment by Federal Poverty Level
        if("educatt" %in% input$outChk) {
          educatt_text <- tags$h2("Table 4: Educational Attainment by Federal Poverty Level")
          educatt_list <- educPRO(lvl=input$level,listID=fipslist,ACS=curACS,curYr=curYr)

          outploted1 <- educatt_list$plot
          outtabed1 <- educatt_list$data
          
          output$EDUCTabOut <- DT::renderDataTable(outtabed1,
                                                   options = list(pageLength = 10,
                                                                  autowidth= TRUE,
                                                                  scrollX = TRUE, scrollY=TRUE),
                                                                  rownames = FALSE,
                                                                  caption = paste0("Table 4: Educational Attainment by Federal Poverty Level: ",input$level))

          
          poped1.info <- tags$div(boxContent(title= "Table 4: Educational Attainment by Federal Poverty Level",
                                              description = "Educational Attainment by Federal Poverty Level plots and tables show Educational Attainment by the Federal Poverty level for persons age 25 and older.",
                                              MSA= "F", stats = "F", muni = "F", multiCty = "F", PlFilter = "F", 
                                              urlList = list(c("American Community Survey, Table B17003","https://data.census.gov")) ),
                                   tags$br(),
                                   downloadObjUI("poped1tabl"), downloadObjUI("poped1data"))

          educ.box0 <- box(width=12,ln1)
          educ.box1 <- tabBox(width=12, height=500,
                               tabPanel("Plot",renderPlotly({outploted1})))
          educ.box2 <- tabBox(width=12, height=500,
                               tabPanel("Table",DT::dataTableOutput("EDUCTabOut")),
                               tabPanel("Sources and Downloads",poped1.info))
          
          
          #building List
          educ.list <<- list(educ.box0, educ.box1, educ.box2)
          
          incProgress()
        }
        
   # Table 5: Age by Federal Poverty Level
        if("povage" %in% input$outChk) {
          pov5_text <- tags$h2("Table 5: Age by Federal Poverty Level")
          pov5_list <- povertyPRO(lvl=input$level,listID=fipslist,ACS=curACS,PreACS="",curYr=curYr,tabtype = 5)

          
          outtabpov5 <- pov5_list$data
          
          output$POVTabOut5 <- DT::renderDataTable(outtabpov5,
                                                   options = list(pageLength = 20,
                                                                  autowidth= TRUE,
                                                                  scrollX = TRUE, scrollY=TRUE),
                                                                  rownames = FALSE,
                                                                  caption = paste0("Table 5: Age by Federal Poverty Level",input$level))

          
          povpp5.info <- tags$div(boxContent(title= "Table 5: Age by Federal Poverty Level",
                                              description = "The Age by Federal Poverty Level plots and tables show the distribution of age categoeries by the Federal Poverty level for the selected agency.",
                                              MSA= "F", stats = "F", muni = "F", multiCty = "F", PlFilter = "F", 
                                              urlList = list(c("American Community Survey, Table B17024","https://data.census.gov")) ),
                                   tags$br(),
                                   downloadObjUI("povpp5tabl"), downloadObjUI("povpp5data"))

          pov5.box0 <- box(width=12,pov5_text)
          
          pov5.box1 <- tabBox(width=12, height=500,
                               tabPanel("Table",DT::dataTableOutput("POVTabOut5")),
                               tabPanel("Sources and Downloads",povpp5.info))
          
          
          #building List
          pov5.list <<- list(pov5.box0, pov5.box1)
          
          incProgress()
        }   
        
        #Table 6: Percent Below Federal Poverty Level by Age Trend
        if("povagetr" %in% input$outChk) {
          pov6_text <- tags$h2("Table 6: Percent Below Federal Poverty Level by Age Trend")
          pov6_list <- povertyPRO(lvl=input$level,listID=fipslist,ACS=curACS,PreACS = preACS,curYr=curYr,tabtype = 6)

          outplotpov6 <- pov6_list$plot
          outtabpov6 <- pov6_list$data
          
          output$POVTabOut6 <- DT::renderDataTable(outtabpov6,
                                                   options = list(pageLength = 6,
                                                                  autowidth= TRUE,
                                                                  scrollX = TRUE, scrollY=TRUE),
                                                                  rownames = FALSE,
                                                                  caption = paste0("Table 6: Percent Below Federal Poverty Level by Age Trend",input$level))

          
          povpp6.info <- tags$div(boxContent(title= "Table 6: Percent Below Federal Poverty Level by Age Trend",
                                              description = "Percent Below Federal Poverty Level by Age Trend plot and table show the th percent below the Federal Poverty level by age for the selected agency for two ACS data periods.",
                                              MSA= "F", stats = "F", muni = "F", multiCty = "F", PlFilter = "F", 
                                              urlList = list(c("American Community Survey, Table B17024","https://data.census.gov")) ),
                                   tags$br(),
                                   downloadObjUI("povpp6tabl"), downloadObjUI("povpp6data"))

          pov6.box0 <- box(width=12,pov6_text)
          pov6.box1 <- tabBox(width=12, height=500,
                               tabPanel("Plot",renderPlotly({outplotpov6})))
          pov6.box2 <- tabBox(width=12, height=500,
                               tabPanel("Table",DT::dataTableOutput("POVTabOut6")),
                               tabPanel("Sources and Downloads",povpp6.info))
          
          
          #building List
          pov6.list <<- list(pov6.box0, pov6.box1, pov6.box2)
          
          incProgress()
        }
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
    
    #Event to output PDF documents
    
    output$outputPDF <- downloadHandler(
      
      filename <- function() {
        paste0(input$unit," Community Profile Report ",as.character(Sys.Date()),".pdf")
      },
      content <- function(file) {
        #Generate Report
        #knitting file and copy to final document
        
        tempRMD <- fixPath(fileMat[88])  #Testing
        tempPDF <- fixPath(fileMat[89]) 
        
    #     tempRMD <- fileMat[88]  
    #     tempPDF <- fileMat[89] 
        
        
        rmarkdown::render(input= tempRMD, output_file = tempPDF,
                          params =  list(outChk = input$outChk,
                                         olistID = idList,
                                         olevel = input$level,
                                         filemat = fileMat),
                          run_pandoc = TRUE)
        
        file.rename(tempPDF, file) # move pdf to file for downloading
      } #Content
    ) #Download Handler
    
    
    
    #Event to outload plots and data files
    
    #Table 1 Age
    if("age" %in% input$outChk){
    callModule(downloadObj, id = "popa1tabl", simpleCap(input$level),"popa1tabl", age_list$FlexTable)
    callModule(downloadObj, id = "popa1data", simpleCap(input$level),"popa1data", age_list$table)
    }
    
   #Table 2 Age by Employment  
    if("ageemp" %in% input$outChk){
    callModule(downloadObj, id = "pope1tabl", simpleCap(input$level),"pope1tabl", emp_list$FlexTable)
    callModule(downloadObj, id = "pope1data", simpleCap(input$level),"pope1data", emp_list$data)
    }

   #Table 3: Federal Poverty Level
   if("pov" %in% input$outChk){
    callModule(downloadObj, id = "povpp3tabl", simpleCap(input$level),"povpp3tabl", pov3_list$FlexTable)
    callModule(downloadObj, id = "povpp3data", simpleCap(input$level),"povpp3data", pov3_list$data)
     }
    
   #Table 4: Educational Attainment by Federal Poverty Level
   if("educatt" %in% input$outChk){
    callModule(downloadObj, id = "poped1tabl", simpleCap(input$level),"poped1tabl", educatt_list$FlexTable)
    callModule(downloadObj, id = "poped1data", simpleCap(input$level),"poped1data", educatt_list$data)
   }
  
   #Table 5: Age by Federal Poverty Level
   if("povage" %in% input$outChk){
    callModule(downloadObj, id = "povpp5tabl", simpleCap(input$level),"povpp5tabl", pov5_list$FlexTable)
    callModule(downloadObj, id = "povpp5data", simpleCap(input$level),"povpp5data", pov5_list$data)
   }
  
  #Table 6: Age by Federal Poverty Level Trend
   if("povagetr" %in% input$outChk){
    callModule(downloadObj, id = "povpp6tabl", simpleCap(input$level),"povpp6tabl", pov6_list$FlexTable)
    callModule(downloadObj, id = "povpp6data", simpleCap(input$level),"povpp6data", pov6_list$data)
     }
    
  }) #observeEvent input$profile
  
  
  
}  #server



shinyApp(ui = ui, server = server)
