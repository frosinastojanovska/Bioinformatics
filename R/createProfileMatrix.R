#' Creates profile matrix for given aligment matrix.
#'
#' @param aligmentMatrix A matrix indicating the aligment matrix.
#' @return Profile matrix
#' @examples
#' createProfileMatrix(matrix(c("A","C","A","C","A","C","U","A","A","C","U","A"), nrow=4, ncol=3))

createProfileMatrix <- function(aligmentMatrix){
  n <- nrow(aligmentMatrix)
  m <- ncol(aligmentMatrix)

  profileMatrix <- matrix(data = 0, nrow = 4, ncol = m)
  rownames(profileMatrix) <- c('A', 'C', 'T', 'G')

  for(i in 1:n){
    for(j in 1:m){
      if(aligmentMatrix[i,j] == "A")
        profileMatrix[1,j] <- profileMatrix[1,j] + 1
      else if(aligmentMatrix[i,j] == "C")
        profileMatrix[2,j] <- profileMatrix[2,j] + 1
      else if(aligmentMatrix[i,j] == "T")
        profileMatrix[3,j] <- profileMatrix[3,j] + 1
      else if(aligmentMatrix[i,j] == "G")
        profileMatrix[4,j] <- profileMatrix[4,j] + 1
    }
  }

  return(profileMatrix)
}
