#' Finds the mRNA sequence for given DNA sequence
#'
#' @param dna A vector of characters in lower or upper case indicating some DNA sequence.
#' @return Vector of characters reperesenting mRNA sequence for given DNA.
#' @examples
#' findRNAforDNA(c("A","C","A","C","A","C","T","A"))
#' findRNAforDNA(c("a","g","c","a","c","a","c","a"))

findRNAforDNA <- function(dna){
  rna <- c()
  for(i in 1:length(dna)){
    if(dna[i] == 'T'){
      rna[i] <- 'U'
    }
    else if(dna[i] == 't'){
      rna[i] <- 'u'
    }
    else{
      rna[i] <- dna[i]
    }
  }
  return(rna)
}
