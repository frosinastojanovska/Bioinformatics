#' Finds score and consensus along with aligment and profile matrices for given DNA sequences and start positions.
#'
#' @param DNA A matrix with DNA sequences in each row.
#' @param consensus A vector with start position for every DNA sequence.
#' @param motifLen Lenght of the motif.
#' @return Vector with starting positions of the DNA sequences.
#' @examples
#' DNA <- matrix(c("A","C","A","C","A","C","G","A","A","C","U","G","A","C","G","C","A","C","G","G","G","C","U","A"), nrow=4, ncol=6)
#' consensus <- c("A","C","A","A")
#' getStartPositionsWithMedianString(DNA, consensus, 4)

getStartPositionsWithMedianString <- function(DNA, consensus, motifLen){
  startPositions <- c()
  for(i in 1:nrow(DNA)){
    position <- getStartPosition(DNA[i,], consensus, motifLen)
    startPositions <- append(startPositions, position)
  }
  return(startPositions)
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
  return(minSP)
}
