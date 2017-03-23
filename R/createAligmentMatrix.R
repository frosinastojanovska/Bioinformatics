#' Creates aligment matrix for given DNA sequences.
#'
#' @param DNA A matrix with DNA sequences in each row.
#' @param startPositions A vector with start position for every DNA sequence.
#' @param t A number indicating the number of DNA sequences.
#' @param l Lenght of the motif.
#' @return Aligment matrix
#' @examples
#' DNA <- matrix(c("A","C","A","C","A","C","G","A","A","C","U","G","A","C","G","C","A","C","G","G","G","C","U","A"), nrow=4, ncol=6)
#' startPositions <- c(1,2,1,3)
#' t <- nrow(DNA)
#' createAligmentMatrix(DNA, startPositions, t, 4)

createAligmentMatrix <- function(DNA, startPositions, t, l){
  matrix <- matrix(, nrow = t, ncol = l)
  for(i in 1:t){
    start <- startPositions[i]
    end <- start + l - 1
    matrix[i,] <- DNA[i, start:end]
  }
  return(matrix)
}
