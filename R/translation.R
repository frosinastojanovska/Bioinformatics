#' Finds all protein sequences for the given DNA sequence. First the algorithm finds the RNA for the given
#' DNA sequence. Then it starts from the begining searching for a start codon, and stops when finds stop
#' codon of end of sequence.When one sequence is found, the algorithm searches for new start codon in the
#' sequence, after the previous start codon.
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
    codon <- toupper(paste(rna[i:(i+2)], collapse = "", sep=""))
    if(codon == startCodon){
      numSequence <- numSequence + 1
      protein <- c(RNAGeneticCoding(startCodon))
      numAminoAcid <- 2
      position <- i + 3
      for(j in seq(position,length(rna),3)){
        if(j + 2 > length(rna))
          break;
        codon <- toupper(paste(rna[j:(j+2)], collapse = "", sep=""))
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
