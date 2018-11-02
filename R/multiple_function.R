
#' Use r code to connect uniprot web api
#'
#'You can choose one id as input id as well as another id at output id
#'know more information you can learnf rom uniprot documatation
#'
#'
#' @param query vector of protein ids
#' @param inputid type of input id, characterr
#' @param outputid type of output id, character
#' @param fmt output form
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' result <- idmapping(query = proid, inputid ='ACC', outputid = "P_ENTREZGENEID", fmt = "fmt")
idmapping <- function(query, inputid, outputid, fmt){
  query <- paste(query, collapse = ",")
  r <- httr::POST('http://www.uniprot.org/uploadlists/', body = list(from= inputid, to = outputid, format = fmt, query = query), encode = "form")
  cont <- httr::content(r, type = "text")
  result <- readr::read_tsv(cont)
}




#' Title
#' using the description1 in para1 to update the description2 in para2
#' these parameters should not be factor level
#' @param description1
#' @param para1
#' @param description2
#' @param para2
#'
#' @return
#' @export
#'
#' @examples

AutoUpdate <- function(description1, para1, description2,  para2){

  s1 <- list()
  p <- vector()
  description <- vector()
  nn <- length(para2)
  for (i in 1:nn){
    s1[[i]] <- which(para1 %in% para2[i] ==TRUE)
    p[i]<- s1[[i]][1]
  }

  for(i in 1:nn){
    if(!is.na(p[i])){
      description[i] <- description1[p[i]]
    } else{
      description[i] <- description2[i]
    }
  }
  return(description)
}
