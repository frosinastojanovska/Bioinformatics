\name{complement}
\alias{complement}
\title{Finds the complement of nucleobase in DNA or RNA.}
\usage{
complement(nucleobase)
}
\arguments{
\item{nucleobase}{A character in lower or upper case indicating some of the nucleobases in DNA or RNA.}
}
\value{
The complement of \code{nucleobase}.
}
\description{
Finds the complement of nucleobase in DNA or RNA.
}
\examples{
complement('A')
complement('c')
complement('U')
complement('u')
}
