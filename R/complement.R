#' Finds the complement of nucleobase in DNA or RNA.
#'
#' @param nucleobase A character in lower or upper case indicating some of the nucleobases in DNA or RNA.
#' @param mode String 'dna' or 'rna' indicationg the nucleic acid
#' @return The complement of \code{nucleobase}.
#' @examples
#' complement('A', 'dna')
#' complement('c', 'dna')
#' complement('U', 'rna')
#' complement('g', 'rna')

complement <- function(nucleobase, mode){
  comp <- ''
  if(mode == 'dna'){
    comp <- switch(nucleobase,
                   A = 'T',
                   T = 'A',
                   C = 'G',
                   G = 'C',
                   a = 't',
                   t = 'a',
                   c = 'g',
                   g = 'c')
  }
  else if(mode == 'rna'){
    comp <- switch(nucleobase,
                   A = 'U',
                   U = 'A',
                   C = 'G',
                   G = 'C',
                   a = 'u',
                   u = 'a',
                   c = 'g',
                   g = 'c')
  }
  return(comp)
}
