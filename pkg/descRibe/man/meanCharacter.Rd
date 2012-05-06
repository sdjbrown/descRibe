\name{meanCharacter}
\alias{meanCharacter}

\title{Summarise a character by species}

\description{
Summarises the given character by each species in the dataset. 
}

\usage{
meanCharacter(descRibe, column, sppVector)
}

\arguments{
  \item{descRibe}{A \code{descRibe} dataframe.}
  \item{column}{Numeric. Column number to be summarised.}
  \item{sppVector}{A vector referring each specimen to a species.}
}

\details{
This function uses the \code{"Type"} attribute of the \code{descRibe} object to determine the method of formatting the character.
}

\value{
A list with the number of elements equal to the number of species in the dataset. 
}

\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{read.descRibe}}.
}
\examples{

}

\keyword{Summary}
