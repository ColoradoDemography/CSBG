#' TempFil Outputs a matrix of temporary file names and directories
#'
#' @param filemat Temporary file matrix
#' @return Matrix of filename vectors
#' @export

TempFil <- function(oDir,chkList, locList) {
  ctyfips <- as.character(as.numeric(substr(locList$list1,3,5)))
  npng <- (length(chkList) * (length(ctyfips)+1)) + 4
  oMatrix <- matrix(data=NA,nrow=npng)

  # Copying RMD File      
  file.copy("SDO_Report.Rmd",oDir)
  oMatrix[1] <- file.path(paste0(oDir,"/","SDO_Report.Rmd"))
  
  # Location of PDF File
  oMatrix[2] <- file.path(paste0(oDir,"/","SDO_REPORT.docx"))

  
 #Copying Dola Image
  file.copy("www/co_dola__NoText-dept.png",oDir)
  oMatrix[3] <- file.path(paste0(oDir,"/","co_dola__NoText-dept.png"))
 file.copy("www/Landscape.docx",oDir)
  
  
  for(i in 4:npng) {
      oMatrix[i] <- tempfile(tmpdir=oDir,fileext=".png")
    } 
 
return(oMatrix)  
}