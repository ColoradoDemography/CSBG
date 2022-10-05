#' captionSrc formats the captions for the tables and plots
#'
#' @param type the data type, "SDO" for State Demography Office data, "ACS" for American Community Survey data
#' @param dataSrc The data string for ACS data
#' @return  Date-stamped caption string
#' @export
#'
captionSrc <- function(type, dataSrc,tab) {

  dateStr <- paste0(", Print Date: ",as.character(format(Sys.Date(),"%m/%d/%Y")))

  if(type == "SDO") {
    srcStr <- paste0("State Demography Office", dateStr)
  }

  if(type == "SDOBEA") {
    srcStr <- paste0("State Demography Office and U.S. Bureau of Economic Analysis", dateStr)
  }
  if(type == "SAIPE") {
    srcStr <- paste0("U.S. Census Bureau Small Area Income and Poverty Estimates (SAIPE) ", dateStr)
  }
  if(type == "QCEW") {
    srcStr <- paste0("Department of Labor and Employment (QCEW)", dateStr)
  }
  if(type == "SNAP") {
    srcStr <- paste0("Supplemental Nutrition Assistance Program (SNAP) Participation,\nHunger Free Colorado", dateStr)
  }
  if(type == "WIC") {
    srcStr <- paste0("Women, Infants and Children (WIC) Program,\nAnnie E. Casey Foundation Kids Count Data Center", dateStr)
  }
    if(type == "SAHIE") {
      srcStr <- paste0("U.S. Census Bureau, ",dataSrc,". Small Area Health Insurance Estimates (SAHIE) ", dateStr)
    }
  if(type == "BLS") {
    srcStr <- paste0("Bureau of Labor Statistics,\nLocal Area Unemployment Statistics", dateStr)
  }
  if(type =="ACS") {
    byr <- paste0("20",substr(dataSrc,4,5))
    eyr <- paste0("20",substr(dataSrc,6,7))
    srcStr <- paste0("U.S. Census Bureau, ",byr,"-",eyr," American Community Survey,\nTable Number ",tab,dateStr)
    
  }
  return(srcStr)
}
