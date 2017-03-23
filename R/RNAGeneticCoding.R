#' Codes some DNA codon into aminoacid.
#'
#' @param codon A string with tree characters in lower or upper case indicating three nucleotides from mRNA codon.
#' @return Character indicating the aminoacid as a result from the genetic code.
#' @examples
#' RNAGeneticCoding("ACA")
#' RNAGeneticCoding("gua")

RNAGeneticCoding <- function(codon){
  codon <- toupper(codon)
  if(nchar(codon) != 3)
    return("Codon must have three nucleotides!")
  codonString <- strsplit(codon,"")[[1]]
  if(checkValidNucleobasesInRNA(codonString) == FALSE)
    return("Codon must have valid RNA nucleotides!")
  data("RNAGeneticCode")
  aminoacid <- RNAGeneticCode[[codon]]
  return(aminoacid)
}
