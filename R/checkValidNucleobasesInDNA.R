#' Checks if given DNA sequence is composed with valid characters representing the nucleobases.
#'
#' @param sequence A vector of characters in lower or upper case indicating some DNA sequence.
#' @return TRUE if the sequence is valid DNA sequence, otherwise returns FALSE
#' @examples
#' checkValidNucleobasesInDNA(c("A","C","A","C","A","C","T","A"))
#' checkValidNucleobasesInDNA(c("a","g","c","a","c","a","c","a"))

checkValidNucleobasesInDNA <- function(sequence){
  dnaSequence <- toupper(sequence)

  adenine <- "A"
  thymine <- "T"
  guanine <- "G"
  cytosine <- "C"
  foundDNA <- TRUE

  if(length(dnaSequence) == 0){
    return(FALSE)
  }

  for (i in 1:length(dnaSequence))
  {
    current <- dnaSequence[i]
    if(current != adenine && current != thymine && current != cytosine && current != guanine){
      foundDNA <- FALSE
      break
    }
  }

  return(foundDNA)
}
