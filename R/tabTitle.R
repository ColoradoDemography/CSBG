#' tabTitle manages the output of descriptive tabs in the interface
#'
#' @param item the item name in input$outChk
#' @return  Descriptive string provided in the tabs of the main interface
#' @export
#'
tabTitle <-function(item) {
  outTitle <- switch(item,
                      "age" = "Table 1: Age Distribution",
                     "ageemp" = "Table 2: Age by Employment Status ",
                     "pov" = "Table 3: Population by Federal Poverty Level ",
                     "educatt" = "Table 4: Educational Attainment by Federal Poverty Level ",
                     "povage" = "Table 5: Age by Federal Poverty Level ",
                     "povagetr" = "Table 6: Age by Federal Poverty Level Trend ",
                     "povagedis" = "Table 7: Age by Federal Poverty Level for Persons with Disabilities ",
                     "hhpov" = "Table 8: Households by Occupancy ",
                     "insurance" = "Table 9: Health Insurance by Source ",
                     "tenure" = "Table 10: Housing Tenure by Poverty "

)
  return(outTitle)
}
