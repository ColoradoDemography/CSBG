#'  listTofips : Produces a vector of FIPS codes from an input list of Census County and Place Name Codes.
#'
#' @param  lvl the comparison level ("Regional Summary","Region to County","County to County","Municipality to Municipality")
#' @param inlist1 The data frame for the base of comparison
#' @param value1  The selected item for the base of the comparison
#' @param inlist2 The data frame for the comparison
#' @param value2  The selected item(s) for the comparisons
#' @return the fipscode(s) for a selected data level
#' @export

listTofips <- function(value1,inlist2,value2){

  # Function to produce a vector of FIPS codes from an input list of names and codes
           reglist <- switch(value1,
                             "Adams County Human Services Department" =  c("08001"),
                             "Arapahoe County Community Resources" =  c("08005"),
                             "Baca County Public Health Agency" =  c("08009"),
                             "Boulder County Community Programs" =  c("08013"),
                             "Broomfield Health and Human Services" =  c("08014"),
                             "City and County of Denver Department of Human Services" =  c("08031"),
                             "Colorado East Community Action Agency" =  c("08017", "08039", "08063", "08073"),
                             "Delta County Health Department" =  c("08029"),
                             "Douglas County" =  c("08035"),
                             "El Paso County" =  c("08041"),
                             "Garfield County Department of Human Services" =  c("08045", "08097"),
                             "Gunnison County Department of Health and Human Services" =  c("08051", "08053"),
                             "Housing Solutions for the Southwest" =  c("08007", "08033", "08067", "08083", "08111"),
                             "Huerfano Las Animas Area Council of Governments" =  c("08055", "08071"),
                             "Jefferson County" =  c("08059", "08093", "08119"),
                             "Kiowa County" =  c("08061"),
                             "La Puente Housing Inc. Additional Service Area" =  c("08037", "08085", "08091", "08113"),
                             "La Puente Housing Inc. Primary Service Area" =  c("08003", "08021", "08023", "08079", "08105", "08109"),
                             "Larimer County Department of Human Services" =  c("08069"),
                             "Mesa County Public Health" =  c("08077"),
                             "Moffat County United Way" =  c("08081"),
                             "Mountain Family Center" =  c("08049", "08057"),
                             "Northeastern Colorado Association of Local Governments" =  c("08075", "08087", "08095", "08115", "08121", "08125"),
                             "Otero County Department of Human Services" =  c("08011", "08025", "08089"),
                             "Prowers County" =  c("08099"),
                             "Pueblo County Department of Housing and Human Services" =  c("08101"),
                             "Rio Blanco County Department of Human Services" =  c("08103"),
                             "Routt County Department of Human Services" =  c("08107"),
                             "Summit County Community and Senior Center" =  c("08019", "08047", "08117"),
                             "Upper Arkansas Area Council of Governments" =  c("08015", "08027", "08043", "08065"),
                             "Weld County Department of Human Services" =  c("08123")
           )
          fipsl <- list("length1" = length(reglist),"plName1" = value1, "list1"= reglist,"length2" = 0, "plName2" = CountyName(reglist), "list2" = reglist)

  return(fipsl)
} #end listTofips
