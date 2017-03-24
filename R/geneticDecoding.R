#' Decodes given aminoacid to DNA codon/s.
#'
#' @param codon A character indicating the aminoacid.
#' @return Vector with the coresponding DNA codons in it.
#' @examples
#' geneticDecoding("M")
#' geneticDecoding("S")
#' geneticDecoding("*")

geneticDecoding <- function(aminoacid){
  data("inverseGeneticCode")
  if(is.element(aminoacid, names(inverseGeneticCode)) == FALSE)
    return("The aminoacid is not valid!")
  codons <- inverseGeneticCode[[aminoacid]]
  codons <- strsplit(codons, split=" ")[[1]]
  return(codons)
}

