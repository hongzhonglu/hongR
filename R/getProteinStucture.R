#' function to parse the pdb file from PDB database
#'
#' @param pdb a pdbfile
#'
#' @return list
#' @export
#'
#' @examples
#' getProteinStucture("3o8o.pdb")
#'
#'
getProteinStucture <- function(pdb) {
  pdb <- scan(pdb, sep = "\n", what = "complex")
  dd <- list()
  ss1 <- which(str_detect(pdb, "HEADER") == TRUE)
  ss2 <- which(str_detect(pdb, "TITLE") == TRUE)
  ss3 <- which(str_detect(pdb, "COMPND") == TRUE)
  ss4 <- which(str_detect(pdb, "SOURCE") == TRUE & str_detect(pdb, "REMARK") != TRUE)
  ss5 <- which(str_detect(pdb, "KEYWDS") == TRUE)
  ss6 <- which(str_detect(pdb, "EXPDTA") == TRUE)
  ss7 <- which(str_detect(pdb, "AUTHOR") == TRUE & str_detect(pdb, "REMARK") != TRUE)
  ss8 <- which(str_detect(pdb, "REVDAT") == TRUE)
  ss9 <- which(str_detect(pdb, "JRNL") == TRUE & str_detect(pdb, "REVDAT") != TRUE)
  ss10 <- which(str_detect(pdb, "REMARK") == TRUE)
  dd$HEADER <- pdb[ss1]
  dd$TITLE <- pdb[ss2]
  dd$COMPND <- pdb[ss3]
  dd$SOURCE <- pdb[ss4]
  dd$KEYWDS <- pdb[ss5]
  dd$EXPDTA <- pdb[ss6]
  dd$AUTHOR <- pdb[ss7]
  dd$REVDAT <- pdb[ss8]
  dd$JRNL <- pdb[ss9]
  return(dd)
}
