#' Finds all protein sequences for the given DNA sequence
#'
#' @param sequence A vector of characters in lower or upper case indicating some DNA sequence.
#' @return A list with vectors indicating all possible protein sequences for given DNA.
#' @examples
#' translation(c("A","T","G","C","A","C","A","T","G","T","A","C","T","G","A","T","A","C"))

translation <- function(sequence){

  if(checkValidNucleobasesInDNA(sequence) == FALSE)
    return("Invalid DNA sequence!")

  rna <- findRNAforDNA(sequence, "5'->3'")

  list <- vector("list")
  numSequence <- 0
  startCodon <- "AUG"
  for(i in 1:(length(rna)-2)){
    codon <- paste(rna[i:(i+2)], collapse = "", sep="")
    if(codon == startCodon){
      numSequence <- numSequence + 1
      protein <- c(RNAGeneticCoding(startCodon))
      numAminoAcid <- 2
      position <- i + 3
      for(j in seq(position,length(rna),3)){
        if(j + 2 > length(rna))
          break;
        codon <- paste(rna[j:(j+2)], collapse = "", sep="")
        aminoAcid <- RNAGeneticCoding(codon)
        if(aminoAcid == "*")
          break;
        protein[numAminoAcid] <- aminoAcid
        numAminoAcid <- numAminoAcid + 1
      }
      list[[numSequence]] <- protein
    }
  }

  if(length(list) == 0)
    return("There's none start codon.")

  return(list)
}
