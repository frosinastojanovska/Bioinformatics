#' Finds the mRNA sequence for given DNA sequence
#'
#' @param dna A vector of characters in lower or upper case indicating some DNA sequence.
#' @param strandDirection A string indicating the DNA strand direction. It could be "5'->3'", or "3'->5'".
#' @return Vector of characters reperesenting mRNA sequence for given DNA.
#' @examples
#' findRNAforDNA(c("A","C","A","C","A","C","T","A"), "3'->5'")
#' findRNAforDNA(c("a","g","c","a","t","a","c","a"), "5'->3'")

findRNAforDNA <- function(dna, strandDirection = c("5'->3'", "3'->5'")){

  if(strandDirection != "5'->3'" && strandDirection !=  "3'->5'")
    return("Not valid strand direction.")

  if(strandDirection == "3'->5'")
    dna <- getComplementaryStrand(dna)

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
