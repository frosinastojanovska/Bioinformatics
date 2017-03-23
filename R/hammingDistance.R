#' Calculates hamming distance between two DNA sequences with same lenght.
#'
#' @param seq1 A vector with characters indicating first DNA sequence.
#' @param seq2 A vector with characters indicating second DNA sequence.
#' @return Character indicating the aminoacid as a result from the genetic code.
#' @examples
#' hammingDistance(c("A","C","A","A","C","T","C","A","T"), c("A","G","A","G","C","T","C","A","A"))

hammingDistance <- function(seq1, seq2) {
  total <- 0
  l = length(seq1)

  for (i in 1:l){
    if (seq1[i] != seq2[i]){
      total = total + 1
    }
  }
  return(total)
}
