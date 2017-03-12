#' Checks if given RNA sequence is composed with valid characters representing the nucleobases.
#'
#' @param sequence A vector of characters in lower or upper case indicating some RNA sequence.
#' @return TRUE if the sequence is valid RNA sequence, otherwise returns FALSE
#' @examples
#' checkValidNucleobasesInRNA(c("A","C","A","C","A","C","U","A"))
#' checkValidNucleobasesInRNA(c("a","g","c","a","c","a","c","a"))

checkValidNucleobasesInRNA <- function(sequence){
  rnaSequence <- toupper(sequence)

  adenine <- "A"
  uracil <- "U"
  guanine <- "G"
  cytosine <- "C"
  foundRNA <- TRUE

  if(length(rnaSequence) == 0){
    return(FALSE)
  }

  for (i in 1:length(rnaSequence))
  {
    current <- rnaSequence[i]
    if(current != adenine && current != uracil && current != cytosine && current != guanine){
      foundRNA <- FALSE
      break
    }
  }

  return(foundRNA)
}
