#' Finds score and consensus along with aligment and profile matrices for given DNA sequences and start positions.
#'
#' @param DNA A matrix with DNA sequences in each row.
#' @param startPositions A vector with start position for every DNA sequence.
#' @param l Lenght of the motif.
#' @return List with the aligment matrix, profile matrix, score and consensus.
#' @examples
#' DNA <- matrix(c("A","C","A","C","A","C","G","A","A","C","U","G","A","C","G","C","A","C","G","G","G","C","U","A"), nrow=4, ncol=6)
#' startPositions <- c(1,2,1,3)
#' findMotifWithConsensus(DNA, startPositions, 4)

findMotifWithConsensus <- function(DNA, startPositions, l){
  t <- length(startPositions)
  aligmentMatrix <- createAligmentMatrix(DNA, startPositions, t, l)
  profileMatrix <- createProfileMatrix(aligmentMatrix)

  consensus <- c()
  score <- 0
  m <- ncol(profileMatrix)
  for(i in 1:m){
    A <- profileMatrix[1,i]
    C <- profileMatrix[2,i]
    T <- profileMatrix[3,i]
    G <- profileMatrix[4,i]
    max <- max(A,T,C,G)
    score <- score + max
    if(max == A){
      consensus <- append(consensus, "A")
    }
    else if(max == C){
      consensus <- append(consensus, "C")
    }
    else if(max == T){
      consensus <- append(consensus, "T")
    }
    else{
      consensus <- append(consensus, "G")
    }
  }

  return(list(aligmentMatrix=aligmentMatrix, profileMatrix = profileMatrix, score = score, consensus = consensus))
}
