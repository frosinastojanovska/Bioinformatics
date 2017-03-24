#' Finds total distance and start positions for given DNA sequences and consensus.
#'
#' @param DNA A matrix with DNA sequences in each row.
#' @param consensus A vector with start position for every DNA sequence.
#' @param motifLen Lenght of the motif.
#' @return List with number for total distance and vector with starting positions of the DNA sequences.
#' @examples
#' DNA <- matrix(c("A","C","A","C","A","C","G","A","A","C","U","G","A","C","G","C","A","C","G","G","G","C","U","A"), nrow=4, ncol=6)
#' consensus <- c("A","C","A","A")
#' getStartPositionsWithMedianString(DNA, consensus, 4)

getStartPositionsWithMedianString <- function(DNA, consensus, motifLen){
  startPositions <- c()
  totalDistance <- 0
  for(i in 1:nrow(DNA)){
    result <- getStartPosition(DNA[i,], consensus, motifLen)
    position <- result$minSP
    totalDistance <- totalDistance + result$minHD
    startPositions <- append(startPositions, position)
  }
  return(list(startPositions, totalDistance))
}

getStartPosition <- function(DNAsequence, consensus, motifLen){
  minHD = hammingDistance(consensus, DNAsequence[1:motifLen])
  minSP = 1
  seqLength = length(DNAsequence)

  for(i in 2:(seqLength - motifLen + 1)){
    HD = hammingDistance(consensus, DNAsequence[i:(i + motifLen)])
    if (HD < minHD){
      minHD = HD
      minSP = i
    }
  }
  return(list(minSP, minHD))
}
