
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
