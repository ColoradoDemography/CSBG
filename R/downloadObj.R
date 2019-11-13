#' downloadObj  File Download Modules
#'
#' downloadObj is the server function that facilitates the download
#'
#' @param place is the place name, typically the value of input$unit
#' @param oname the input data description object
#' @param dboj is the data object to be output
#' @export

downloadObj <- function(input, output, session, place, oname, dobj) {

  if(nchar(oname) == 9) {
    dname <- substr(oname,1,5)
    dtype <- substr(oname,6,9)
  }

  if(nchar(oname) == 10) {
    dname <- substr(oname,1,6)
    dtype <- substr(oname,7,10)
  }


  prefix <- switch(dname,
                   "popa1" = " Table 1 Age Distribution",
                   "pope1" = " Table 2 Age by Employment Status",
                   "povpp3" = " Table 3 Population by Federal Poverty Level",
                   "poped1" = " Table 4 Educational Attainment by Fed Poverty Level",
                   "povpp5" = " Table 5: Age by Federal Poverty Level",
                   "povpp6" = " Table 6: Age by Federal Poverty Level Trend"
  )

  suffix <- ifelse(dtype == "data"," Data.csv"," Table.docx")

  output$download <-  downloadHandler(
    filename = function() {
      paste0(prefix," ",place,suffix)
    },
    content = function(file) {
      if(suffix == " Data.csv") {
        write_csv(dobj, file)
      }
      if(suffix == " Table.docx") {
        doc <- read_docx()
        doc <- body_add_flextable(doc, value = dobj)
        print(doc, target = file)
      }
    } #content
  ) #DowhloadHandler
} #downloadObj
