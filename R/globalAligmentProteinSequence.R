#' Finds the optimal global nucleotide sequence aligment with Needleman-Wunsch algorithm with BLOSUM50 as score matrix.
#'
#' @param sequence1 First vector with characters indicating some protein sequence.
#' @param sequence2 Second vector with characters indicating some protein sequence.
#' @param gapPenalty A number indicating the gap penalty value.
#' @param blosumName Name of the BLOSUM matrix.
#' @return List containing the score and the resulting aligned sequences.
#' @examples
#' globalAligmentProteinSequence(c("P","A","W","H","E","A","E"), c("H","E","A","G","A","W","G","H","E","E"), 8, "BLOSUM50")


globalAligmentProteinSequence <- function(sequence1, sequence2, gapPenalty, blosumName){
  if(exists(blosumName) == FALSE)
    return("BLOSUM matrix with that name isn't available")
  BLOSUM <- get(blosumName)

  if(gapPenalty > 0)
    d = gapPenalty * -1
  else
    d = gapPenalty

  matrix <- matrix(data = NA, nrow = length(sequence1) + 1, ncol = length(sequence2) + 1)
  dimnames(matrix) <- list(c("-", sequence1), c("-", sequence2))

  matrix[1,1] = 0  #prviot element e nula

  # popolnuvanje na prva kolona
  for(i in 2:nrow(matrix)){
    matrix[i,1] = (i - 1) * d
  }

  # popolnuvanje na prv red
  for(j in 2:ncol(matrix)){
    matrix[1,j] = (j - 1) * d
  }

  # presmetka na matricata
  for(i in 2:nrow(matrix)){
    for(j in 2:ncol(matrix)){
      rowname <- rownames(matrix)[i]
      colname <- colnames(matrix)[j]
      match <- matrix[i-1,j-1] + BLOSUM[rowname, colname]
      delete <- matrix[i-1,j] + d
      insert <- matrix[i,j-1] + d
      matrix[i,j] <- max(match, insert, delete)
    }
  }

  finalScore <- matrix[nrow(matrix),ncol(matrix)]

  # back track
  newSequence1 <- ""
  newSequence2 <- ""
  i <- nrow(matrix)
  j <- ncol(matrix)

  while(i > 1 && j > 1){
    score <- matrix[i,j]
    scoreDiag <- matrix[i-1,j-1]
    scoreLeft <- matrix[i,j-1]
    scoreUp <- matrix[i-1,j]
    rowname <- rownames(matrix)[i]
    colname <- colnames(matrix)[j]

    if(score == scoreUp + d){
      newSequence1 <- paste(rowname, newSequence1, sep = "")
      newSequence2 <- paste("-", newSequence2, sep = "")
      i <- i - 1
    }
    else if(score == scoreLeft + d){
      newSequence1 <- paste("-", newSequence1, sep = "")
      newSequence2 <- paste(colname, newSequence2, sep = "")
      j <- j - 1
    }
    else{
      newSequence1 <- paste(rowname, newSequence1, sep = "")
      newSequence2 <- paste(colname, newSequence2, sep = "")
      i <- i - 1
      j <- j - 1
    }
  }

  while(i > 1){
    rowname <- rownames(matrix)[i]
    score <- matrix[i,j]
    newSequence1 <- paste(rowname, newSequence1, sep = "")
    newSequence2 <- paste("-", newSequence2, sep = "")
    i <- i - 1
  }

  while(j > 1){
    colname <- colnames(matrix)[j]
    score <- matrix[i,j]
    newSequence1 <- paste("-", newSequence1, sep = "")
    newSequence2 <- paste(colname, newSequence2, sep = "")
    j <- j - 1
  }

  solution = list(score = finalScore, sequence1 = newSequence1, sequence2 = newSequence2)
  return(solution)
}
