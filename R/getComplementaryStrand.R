#' Finds the complementary strand for given DNA strand
#'
#' @param dna A vector of characters in lower or upper case indicating some DNA sequence.
#' @return Vector of characters reperesenting the complementary DNA strand.
#' @examples
#' getComplementaryStrand(c("A","C","A","C","A","C","T","A"))
#' getComplementaryStrand(c("a","g","c","a","c","a","c","a"))

getComplementaryStrand <- function(dna){
  cDNA <- c()
  for(i in 1:length(dna)){
    cDNA <- append(cDNA, complement(dna[i]))
  }
  return(cDNA)
}
