\name{penultPaste}
\alias{penultPaste}

\title{Collapses a vector}

\description{
Collapses a vector, with a different seperator between the last two elements.
}

\usage{
penultPaste(vec, sep = ", ", penultimate = " or ")
}

\arguments{
  \item{vec}{A vector, the elements of which are to be collapsed.}
  \item{sep}{The string to separate all but the last element.}
  \item{penultimate}{The string to separate the last element.}
}

\details{

}

\value{
A character vector with a single element
}

\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{paste}}.
}

\examples{

penultPaste(LETTERS[1:5])
penultPaste(LETTERS[1:5], penultimate = " and ")
penultPaste(LETTERS[1:5], sep = "; ")

}

\keyword{Format}
