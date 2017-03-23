#' Codes some DNA codon into aminoacid.
#'
#' @param codon A string with tree characters in lower or upper case indicating three nucleotides from DNA codon.
#' @return Character indicating the aminoacid as a result from the genetic code.
#' @examples
#' geneticCoding("ACA")
#' geneticCoding("gta")

geneticCoding <- function(codon){
  codon <- toupper(codon)
  if(nchar(codon) != 3)
    return("Codon must have three nucleotides!")
  codonString <- strsplit(codon,"")[[1]]
  if(checkValidNucleobasesInDNA(codonString) == FALSE)
    return("Codon must have valid DNA nucleotides!")
  data("geneticCode")
  aminoacid <- geneticCode[[codon]]
  return(aminoacid)
}
