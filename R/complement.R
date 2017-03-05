#' Finds the complement of nucleobase in DNA or RNA.
#'
#' @param nucleobase A character in lower or upper case indicating some of the nucleobases in DNA or RNA.
#' @return The complement of \code{nucleobase}.
#' @examples
#' complement('A')
#' complement('c')
#' complement('U')
#' complement('u')

complement <- function(nucleobase){
  comp <- ''
  comp <- switch(nucleobase,
                A = 'T',
                T = 'A',
                C = 'G',
                G = 'C',
                U = 'A',
                a = 't',
                t = 'a',
                c = 'g',
                g = 'c',
                u = 'a')
  return(comp)
}
