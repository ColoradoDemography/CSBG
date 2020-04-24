#' TempFil Outputs a matrix of temporary file names and directories
#'
#' @param filemat Temporary file matrix
#' @return Matrix of filename vectors
#' @export

TempFil <- function(oDir) {
  oMatrix <- matrix(data=NA,nrow=200)

  # Copying RMD File      
  file.copy("SDO_Report.Rmd",oDir)
  oMatrix[1] <- file.path(paste0(oDir,"/","SDO_Report.Rmd"))
  
  # Location of PDF File
  oMatrix[2] <- file.path(paste0(oDir,"/","SDO_REPORT.docx"))

  
 #Copying Dola Image
  file.copy("www/co_dola__NoText-dept.png",oDir)
  oMatrix[3] <- file.path(paste0(oDir,"/","co_dola__NoText-dept.png"))
 file.copy("www/Landscape.docx",oDir)
  
  
  for(i in 4:200) {
      oMatrix[i] <- tempfile(tmpdir=oDir,fileext=".png")
    } 
 
return(oMatrix)  
}