#' Decodes given aminoacid to RNA codon/s.
#'
#' @param codon A character indicating the aminoacid.
#' @return Vector with the coresponding RNA codons in it.
#' @examples
#' RNAGeneticDecoding("M")
#' RNAGeneticDecoding("S")
#' RNAGeneticDecoding("*")

RNAGeneticDecoding <- function(aminoacid){
  data("inverseRNAGeneticCode")
  if(is.element(aminoacid, names(inverseGeneticCode)) == FALSE)
    return("The aminoacid is not valid!")
  codons <- inverseRNAGeneticCode[[aminoacid]]
  codons <- strsplit(codons, split=" ")[[1]]
  return(codons)
}
