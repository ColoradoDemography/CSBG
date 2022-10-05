genDropdown <- function(ctyfips, NameVar) {
  outlist <- "list("
  for(i in 1:length(ctyfips)) {
    outlist <- paste0(outlist, 'list(method = "restyle", args = list("transforms[0].value", unique(',NameVar,')[',i,']),label = unique(',NameVar,')[',i,']),')
  }
  outlist <- paste0(substr(outlist,1,nchar(outlist)-1),")")
  return(outlist)
}